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
    `[[`(1) %>%
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
  linkDF <- as.data.frame(linkDF)
  colnames(linkDF) <- c("Term", "CourseNumber", "CourseName", "Instructor", "Link")
  return(linkDF)
}


# This creates R binary files to hold the evals, because otherwise they're
# just too damn big to anything reasonable with.
# Args: character vector representing local links on applyweb.com to be scraped
getTraceEvals <- function(links) {
  curLink <- 0 # Zero indexed because modulo starts at 0, so add one to work with R
  chunkSize <- 100
  baseURL <- "https://www.applyweb.com"
  evals <- list(mode="character", length=chunkSize)
  startTime <- Sys.time()
  
  # Stores the current position if you start and stop the script
  on.exit({curLink <<- curLink})
  
  while (curLink < length(links)) {
    # Get the current link to scrape
    link <- links[curLink + 1]
    dr$navigate(str_c(baseURL, link))
    
    # I'm not doing any parsing here because evals for different programs
    # have different formats
    eval <- getIFrameSource()
    evals[[(curLink %% chunkSize) + 1]] <- eval
    
    if (curLink %% 10 == 0) {
      curTime <- Sys.time()
      avgTime <- (curTime - startTime) / curLink
      print(str_c("SCRAPED ", curLink, " EVALS ",
                  "IN ", round(avgTime, 3), " SECONDS PER EVAL: ",
                  "ESTIMATED COMPLETION AT ", 
                  format(curTime + ((length(links) - curLink) * avgTime), 
                         "%Y-%m-%d %H:%M")))
    }
    curLink <- curLink + 1;
    
    if (curLink %% chunkSize == 0) {
      print("DUMPED EVALS TO FILE")
      save(evals, file=str_c("evals", curLink))
    }
  }
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


getTraceEvals(csClassLinks)

mybrowser$server$stop()
