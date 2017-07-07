#!/usr/bin/Rscript
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
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Julian Zucker, julian.zucker@gmail.com
#  ORGANIZATION: uConnect
#       CREATED: 23 June 2017
#      REVISION: 5 July 2017
#          TODO: ---
# 
#########################################################################################


################################   Loading Packages   ####################################
library(plotly)
library(reshape2)
library(magrittr)
library(dplyr)
library(wesanderson)

################################   Loading External Files   ##############################

load("data/evals")
colnames(shinyData) <- c("Name", "Instructor", "Subject", "Department", "Number", 
                         "Course Quality", "Amount Learnt", "Instructor Quality", 
                         "Would Recommend", "Semester", "Time Per Week")
shinyData %<>% melt(id.vars=c("Name", "Instructor", "Subject", 
                              "Department", "Semester", "Number"))


######################################  Run Server  ######################################

shinyServer(function(input, output) {
  dataFilteredBySubject <- filterBySubject(input, output, shinyData)
  dat <- filterBySubjectAndName(input, output, shinyData)

  output$class <- renderUI(
    selectInput("class",
               "Select a course",
               sort(unique(dataFilteredBySubject()$Name, multiple=TRUE))))
  
  output$classStatsBar <- renderPlotly({
    data <- dat() %>%
      group_by(Instructor, variable) %>%
      summarize(Mean = round(mean(value), 2))
    p <- ggplot(data, aes(variable, Mean, fill=variable)) +
      facet_wrap(~Instructor) +
      geom_col() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 22, hjust = 0)) +
      xlab(NULL) +
      scale_fill_manual(values=wes_palette("Zissou"))
    
    ggplotly(p) %>% 
      config(displayModeBar = FALSE,
             scrollZoom = FALSE,
             doubleClick = FALSE,
             showAxisDragHandles = FALSE)
  })
  
  return (output)
})

filterBySubject <- function(input, output, dat) {
  reactive({
    dat %>% 
      filter(Subject %in% input$subject)
  })
}

filterBySubjectAndName <- function(input, output, dat) {
  reactive({
    dat %>% 
      filter(Subject %in% input$subject, Name %in% input$class)
  })
}
