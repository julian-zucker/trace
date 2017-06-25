library(RSelenium)
library(stringr)
library(dplyr)
library(XML)

# Sets the working directory to whatever directory this script was executed
# from so we can source files using relative position
setwd(".")



# Logs in to MyNEU
login <- function() {
  source("credentials.R", local = TRUE)
  # Load MyNEU username/password
  # If you want to run this script, create a credentials.R looking like this:
  # myNEUuser <- "your.username"
  # myNEUpass <- "your.password"
  
  dr$navigate(urlWithCookies)
  
  
  tries <- 0
  # Sometimes MyNEU gives you a time out error, so this waits for you to get
  # redirected to the login page before you login.
  repeat {
    # If we found the login button, enter our credentials;
    if (length(dr$findElements(using="xpath", '//form[@name="cplogin"] | //form[@id="fm1"]'))) {
      break;
    }
    
    # If we're already logged in, get outta here.
    if (str_detect(dr$getCurrentUrl(), "applyweb.com")) {
      return();
    }
    
    # Stop after 100 tries
    tries <- tries + 1
    if (tries > 100) {
      stop("Couldn't log in after 100 tries")
    }
    Sys.sleep(.3)
  }
  
  # The username box is active when the page loads, so 
  # the webdriver can just talk to the active element
  dr$sendKeysToActiveElement(list(myNEUuser, "\t", myNEUpass, "\n"))
}

getIFrameSource <- function() {
  repeat {
    a <- dr$findElements("xpath", "//*[@id='contentFrame']")
    if (length(a)) {
      break
    }
  }
  
  dr$executeScript(str_c('return document.getElementById("contentFrame")',
                         ".contentWindow.document.body.innerHTML"))[[1]]
}

getReportURLs <- function() {
  # Navigate to page, wait for reports to load
  dr$navigate("https://www.applyweb.com/eval/new/reportbrowser")
  Sys.sleep(10)
  
  # First count how many rows there are to validate the load process
  numClasses <- dr$executeScript(
    'var totals = document.evaluate("//span[@id=\\"nav-page\\"]", 
        document.getElementById("contentFrame").contentWindow.document, null, XPathResult.ANY_TYPE, null);
    var n = totals.iterateNext();
    return n.innerHTML;') %>%
    extract2(1) %>%
    str_extract("[0-9]+(?= total)") %>%
    as.numeric()
  
  
  # Find and click view all button -- easier to scrape all URLs, then
  # visit each one seperately in a second pass
  dr$executeScript(
    'var nodes = document.evaluate("//span/a", 
        document.getElementById("contentFrame").contentWindow.document, 
    null, XPathResult.ANY_TYPE, null);
    
    var viewAllButton = nodes.iterateNext();
    
    viewAllButton.click();
    var nodes = document.evaluate("//span/a", 
        document.getElementById("contentFrame").contentWindow.document, 
    null, XPathResult.ANY_TYPE, null);')
  dr$acceptAlert()
  
  # While the number of classes we should have is less than the amount
  # that are currently on the screen, wait.
  
  while (numClasses > length(getNodeSet(htmlParse(getIFrameSource()),
                                        '//table[@id="EvaluatedCourses"]/tbody/tr/td/a'))) {
    Sys.sleep(1);
  }
  
  # Gets the text parts of the table, need to use xmlGetAttr to pull the link
  parseTD <- function(x) {
    x %>% getChildrenStrings() %>% `[`(1:4)
  }
  
  parsePageSource <- function() {
    links <- xpathSApply(source, '//table[@id="EvaluatedCourses"]/tbody/tr/td/a', 
                         function(x) xmlGetAttr(x, "href"))
    return(links)
  }
  
  
  source <- htmlParse(getIFrameSource())
  
  links <- parsePageSource()
  info <- source %>%
    getNodeSet('//table[@id="EvaluatedCourses"]/tbody/tr') %>%
    lapply(parseTD) %>% 
    unlist() %>%
    unname() %>%
    matrix(ncol=4, byrow=TRUE)
  linkDF <- cbind(info, links)
  linkDF <- as.data.frame(linkDF) %>%
    lapply(as.character)
  colnames(linkDF) <- c("Term", "CourseNumber", "CourseName", "Instructor", "Link")
  return(linkDF)
}


parseTraceEval <- function(eval) {
  out <- vector(mode="character")
  
  tree <- htmlParse(eval, useInternalNodes = TRUE)
  
  name <- xmlValue(getNodeSet(tree, '/html/body/div[1]/div[2]/div/h3')[[1]])
  course_data <- getNodeSet(tree, '/html/body/div[1]/div[2]/div/div/div[1]/ul/li/strong') %>%
    xmlSApply(xmlValue) 
  survey_response <- getNodeSet(tree, '/html/body/div[1]/div[2]/div/div/div[2]/ul/li/strong') %>%
    xmlSApply(xmlValue) 
  
  processBarChart <- function(xpath) {
    getNodeSet(tree, xpath) %>%
      xmlSApply(function(x) xmlSApply(x, xmlValue) %>% str_extract("[0-9]\\.[0-9]")) %>%
      unlist() %>% t()
  }
  
  category_summary <- processBarChart('//*[@id="bar_mean_cat_1"]/div/div[1]/div/svg/g[2]/g[4]')
  course_related_questions <- processBarChart('//*[@id="bar_mean_2_1"]/div/div[1]/div/svg/g[2]/g[4]')
  learning_related_questions <- processBarChart('//*[@id="bar_mean_2_2"]/div/div[1]/div/svg/g[2]/g[4]')
  instructor_related_questions <- processBarChart('//*[@id="bar_mean_2_3"]/div/div[1]/div/svg/g[2]/g[4]')
  instructor_overall <- processBarChart('//*[@id="bar_mean_2_22"]/div/div[1]/div/svg/g[2]/g[4]')
  
  times <- c("1-", "5-", "9-", "3-", "7-")
  
  # Percents and labels are stored seperately... but in the same order! So not
  # all is lost, we can lookup the order of the times and match.
  percents <- xmlSApply(getNodeSet(tree, '//*[@id="pie_3_9"]/svg/g[3]'), xmlValue) %>%
    str_split("%(?!$)") %>% extract2(1) %>% str_replace(., "%", "")
  labels <- xmlSApply(getNodeSet(tree, '//*[@id="pie_3_9"]/svg/g[4]'), xmlValue)
  
  # This clever regex lets us only care about the right before the dash,
  # which is unique between timeslots
  labels <- str_split(labels, "[0-9]+(?=([0-9]-|$))")[[1]]
  timeSpentInOrder <- 
    sapply(times, function(x) 
      ifelse(length(which(labels == x)), 
             percents[which(labels == x)], NA))
  
  
  out <- c(name, course_data, survey_response, category_summary,
           course_related_questions, learning_related_questions, instructor_related_questions,
           instructor_overall, timeSpentInOrder)
  
  names(out) <- c("Course Name",
                  "Instructor",
                  "Section",
                  "Course Title",
                  "CRN",
                  "Subject",
                  "Enrollment",
                  "Responses",
                  "Declines",
                  "Department",
                  "Number",
                  "Course Related Questions Course",
                  "Course Related Questions Department",
                  "Course Related Questions University",
                  "Learning Related Questions Course",
                  "Learning Related Questions Department",
                  "Learning Related Questions University",
                  "Instructor Related Questions Course",
                  "Instructor Related Questions Department",
                  "Instructor Related Questions University",
                  "Instructor Effectiveness Course",
                  "Instructor Effectiveness Department",
                  "Instructor Effectiveness University",
                  "Syllabus Helped Course", 
                  "Syllabus Helped Department", 
                  "Syllabus Helped University", 
                  "Textbook Helped Course", 
                  "Textbook Helped Department", 
                  "Textbook Helped University",
                  "Online Helped Course", 
                  "Online Helped Department", 
                  "Online Helped University", 
                  "Assignments Helped Course", 
                  "Assignments Helped Department", 
                  "Assignments Helped University",
                  "Lectures Helped Course", 
                  "Lectures Helped Department", 
                  "Lectures Helped University", 
                  "Inclass Helped Course", 
                  "Inclass Helped Department", 
                  "Inclass Helped University", 
                  "Classroom Technology Helped Course", 
                  "Classroom Technology Helped Department", 
                  "Classroom Technology Helped University", 
                  "Course Was Intellectual Course", 
                  "Course Was Intellectual Department", 
                  "Course Was Intellectual University",
                  "Learned a Lot Course", 
                  "Learned a Lot Department",
                  "Learned a Lot University", 
                  "Learned Concepts And Principles Course", 
                  "Learned Concepts And Principles Department", 
                  "Learned Concepts And Principles University", 
                  "Developed Skills Expressing Course", 
                  "Developed Skills Expressing Department",
                  "Developed Skills Expressing University", 
                  "Learned Analyze and Evaluate Course", 
                  "Learned Analyze and Evaluate Department", 
                  "Learned Analyze and Evaluate University",
                  "Instructor Communication Skills Course", 
                  "Instructor Communication Skills Department", 
                  "Instructor Communication Skills University",
                  "Instructor Communicated Course", 
                  "Instructor Communicated Department", 
                  "Instructor Communicated University", 
                  "Instructor Stated Objectives Course",
                  "Instructor Stated Objectives Department", 
                  "Instructor Stated Objectives University",
                  "Instructor Covered Stated Course", 
                  "Instructor Covered Stated Department", 
                  "Instructor Covered Stated University", 
                  "Instructor Prepared Course", 
                  "Instructor Prepared Department", 
                  "Instructor Prepared University",
                  "Instructor Used Class Time Well Course", 
                  "Instructor Used Class Time Well Department", 
                  "Instructor Used Class Time Well University", 
                  "Instructor Provided Feedback Course", 
                  "Instructor Provided Feedback Department", 
                  "Instructor Provided Feedback University",
                  "Instructor Fairly Evaluated Performance Course", 
                  "Instructor Fairly Evaluated Performance Department",
                  "Instructor Fairly Evaluated Performance University", 
                  "Would Recommend Course", 
                  "Would Recommend Department",
                  "Would Recommend University", 
                  "Respected Students Course", 
                  "Respected Students Department",
                  "Respected Students University",
                  "Instructor Took Effective Action Course",
                  "Instructor Took Effective Action Department", 
                  "Instructor Took Effective Action University",
                  "Instructor Available Outside of Class Course", 
                  "Instructor Available Outside of Class Department",
                  "Instructor Available Outside of Class University", 
                  "Instructor Displayed Enthusiasm Course", 
                  "Instructor Displayed Enthusiasm Department", 
                  "Instructor Displayed Enthusiasm University",
                  "Instructor Overall Course",
                  "Instructor Overall Department",
                  "Instructor Overall University",
                  "Spent 1-4 Hours",
                  "Spent 5-8 Hours",
                  "Spent 9-12 Hours",
                  "Spent 13-16 Hours",
                  "Spent 17-20 Hours")
  return(out)
}

# Args: character vector representing local links on applyweb.com to be scraped
getTraceEvals <- function(links) {
  curLink <- 1
  baseURL <- "https://www.applyweb.com"
  evals <- list(mode="character", length=length(links))
  
  # Stores the current position if you start and stop the script
  on.exit({curLink <<- curLink})
  
  while (curLink < length(links)) {
    # Get the current link to scrape
    link <- links[curLink]
    dr$navigate(str_c(baseURL, link))
    
    # I'm not doing any parsing here because evals for different programs
    # have different formats, and I want the scraper to be general
    evals[[curLink]] <- 
      getIFrameSource() %>%
      parseTraceEval()
    
    
    curLink <- curLink + 1
    if (curLink %% 10 == 0) {
      print(str_c("SCRAPED ", curLink, " EVALS "))
    }
  }
  return (evals)
}






# Launch the selenium driver
mybrowser <- rsDriver()
dr <- mybrowser$client

login()
linkDF <- getReportURLs()

# Scraping everythign is kind of tedious, so let's just do CS classes
csClassLinks <- linkDF %>%
  filter(str_detect(CourseNumber, '^(CS|IS|DS|DSCS|IA)[0-9]{4,}')) %>%
  .$Link

csEvals <- getTraceEvals(csClassLinks)
csEvalsDF <- csEvals %>% unlist %>% matrix(byrow=TRUE, ncol=106)
write.csv(csEvalsDF, "CS TRACE Evaluations.csv")

mybrowser$server$stop()
