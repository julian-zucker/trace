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
#          TODO: Add ability to view all courses that an instructor teaches
#                Add ability to see aggregate stats based on Department, semester, etc
# 
#########################################################################################


###############################   Loading Packages   ####################################
library(plotly)
library(reshape2)
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

######################################  Create UI   ######################################



shinyUI(
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("department"),
      uiOutput("class")
     ),
    mainPanel(
     tags$head(tags$style("#classStatsBar{height:90vh !important;}"),
               tags$script("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                           (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                           m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                           })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
                           
                           ga('create', 'UA-102257141-1', 'auto');
                           ga('send', 'pageview');")),
      plotlyOutput(outputId = "classStatsBar", inline=TRUE)
      
    )
  )
)
