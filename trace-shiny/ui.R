#!/usr/bin/Rscript
#########################################################################################
#
#          FILE: ui.R
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


##############################   Loading External Files   ################################

load("data/csEvals")
colnames(shinyData) <- c("Name", "Instructor", "Subject", "Department", "Number", 
                        "Course Quality", "Amount Learnt", "Instructor Quality", 
                        "Would Recommend", "Semester", "Time Per Week")
shinyData %<>% melt(id.vars=c("Name", "Instructor", "Subject", 
                              "Department", "Semester", "Number"))




######################################  Create UI   ######################################


sidebarLayout(
  sidebarPanel(
    selectInput("class", "Select Class", unique(shinyData$Name), multiple=FALSE)
  #  selectInput("instructor", "Select Instructor", unique(shinyData$Instructor) , multiple=TRUE)
  ),
  mainPanel(
    plotlyOutput("classStatsBar", width = "1000px", height = "100")
  )
)
