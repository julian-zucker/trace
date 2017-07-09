#########################################################################################
#
#          FILE: server.R
#
#         USAGE: R -e 'if(!require("shiny")) install.packages("shiny"); runApp(".")'
#
#   DESCRIPTION: Given a list of contacts and credentials in the same folder, 
#                enrolls the contacts in hubspot email sequences.
#
#       OPTIONS: ---
#  REQUIREMENTS: R >3.3.0, plotly 4.7, ggplot 2.2.1, reshape2 1.4.2, dplyr 0.7.1
#          BUGS: Will show error message for a second whenever departments are swapped
#                AACE classes all show up as AACE 6010
#         NOTES: ---
#        AUTHOR: Julian Zucker, julian.zucker@gmail.com
#  ORGANIZATION: uConnect
#       CREATED: 23 June 2017
#      REVISION: 5 July 2017
#          TODO: Allow looking at all courses taught by a given professor
#                Allow comparing to course average 
#                Allow viewing aggregate data by college/subject/course level,
#                    but maybe that works best as a seperate app
# 
#########################################################################################


################################   Loading Packages   ####################################
library(plotly)
library(reshape2)
library(magrittr)
library(dplyr)
library(wesanderson)


##############################   Loading External Files   ################################

load("data/evals")
colnames(shinyData) <- c("Name", "Instructor", "Subject", "Department", "Number", 
                         "Course Quality", "Amount Learnt", "Instructor Quality", 
                         "Would Recommend", "Semester", "Time Per Week")

shinyData %<>%
  filter(!duplicated(shinyData)) %>%
  melt(id.vars=c("Name", "Instructor", "Subject", 
                 "Department", "Semester", "Number")) 


# Keep only undergraduate courses
shinyData %<>% filter(Number < 5000)
subjects <- sort(unique(shinyData$Subject))


######################################  Run Server  ######################################

shinyServer(function(input, output) {
  output$department <- renderUI(selectInput("subject", "Select Department", 
                                            subjects,
                                            multiple = FALSE))
  
  output$class <-   renderUI(selectInput("className", "Select Courses", 
                                         shinyData %>% filter(Subject == input$subject) %>%
                                           use_series(Name) %>% unique %>% sort,
                                         multiple=TRUE))
  
  
  output$classStatsBar <-
    renderPlotly({
      # If both inputs are filled out, we can draw individual graphs
      if (!is.null(input$subject) & !is.null(input$className)) {
        if (length(input$className) > 1) {
        data <- shinyData %>%
          filter(Name %in% input$className) %>%
          group_by(Name, variable) %>%
          summarize(Mean = round(mean(value), 2))
        
        p <- ggplot(data, aes(variable, Mean, fill=variable)) +
          # If there's only one class, facet by instructors, otherwise, facet by class names
          facet_wrap(~Name) +
          geom_col() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 22, hjust = 0)) +
          scale_fill_manual(values=wes_palette("Zissou"))
        } else {
          data <- shinyData %>%
            filter(Name %in% input$className) %>%
            group_by(Instructor, variable) %>%
            summarize(Mean = round(mean(value), 2))
          
          p <- ggplot(data, aes(variable, Mean, fill=variable)) +
            # If there's only one class, facet by instructors, otherwise, facet by class names
            facet_wrap(~Instructor) +
            geom_col() +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 22, hjust = 0)) +
            scale_fill_manual(values=wes_palette("Zissou"))
        }
        ggplotly(p) %>% 
          config(displayModeBar = FALSE,
                 scrollZoom = FALSE,
                 doubleClick = FALSE,
                 showAxisDragHandles = FALSE)
      } else {
        # If an input isn't filled out, display the graph for all data in aggregate
        ggplotly({
          ggplot(shinyData %>% 
                       filter(Subject %in% input$subject) %>% 
                   group_by(Number, variable) %>% 
                   summarize(Mean = round(mean(value), 2)),
                 aes(Number, Mean, color=variable)) +
            geom_point() +
            geom_smooth(se=FALSE)
        })
      }
    })
  return (output)
})
