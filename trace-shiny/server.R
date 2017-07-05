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

load("data/csEvals")
colnames(shinyData) <- c("Name", "Instructor", "Subject", "Department", "Number", 
                         "Course Quality", "Amount Learnt", "Instructor Quality", 
                         "Would Recommend", "Semester", "Time Per Week")
shinyData %<>% melt(id.vars=c("Name", "Instructor", "Subject", 
                              "Department", "Semester", "Number"))



################################   Loading Packages   ####################################
library(plotly)
library(reshape2)
library(magrittr)
library(dplyr)
library(wesanderson)


######################################  Run Server  ######################################

shinyServer(function(input, output) {
  dat <- filterData(input, output)
  
  output$classStatsBar <- renderPlotly({
    p <- ggplot(dat(), aes(variable, m, fill=variable)) +
      geom_blank() +
      facet_wrap(~Instructor) +
      geom_col() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab(NULL) +
      scale_fill_manual(values=wes_palette("Zissou"))
    ggplotly(p)
  })
})

filterData <- function(input, output) {
  reactive({
    if (!length(input$class)) {
      input$class <- unique(shinyData$Course.Name)
    }
   # if (!length(input$instructor)) {
   #   input$instructor <- unique(shinyData$Instructor)
   # }
    
    shinyData %>% 
      filter(Name %in% input$class) %>%
      group_by(Instructor, variable) %>%
      summarize(m = mean(value))
  })
}