library(plotly)
library(reshape2)
library(magrittr)
library(dplyr)

load("data/csEvals")




sidebarLayout(
  sidebarPanel(
    selectInput("class", "Select Class", unique(shinyData$Course.Name)),
    selectInput("instructor", "Select Instructor", unique(shinyData$Instructor))
  ),
  mainPanel(
    plotlyOutput("classTimeTakenAndWouldRecommend"),
    plotlyOutput("classStats")
  )
)
