
shinyServer(function(input, output) {
  dat <- reactive({
    if (!length(input$class)) {
      input$class <- unique(shinyData$Course.Name)
    }
    
    if (!length(input$instructor)) {
      input$instructor <- unique(shinyData$Instructor)
    }
    
    shinyData %<>% melt(id.vars=c("Course.Name", "Instructor", "Subject", 
                                  "Department", "Semester", "Number"))
    shinyData %>% 
      filter(Course.Name %in% input$class) %>%
      group_by(Instructor, variable) %>%
      summarize(m = mean(value))
    })
  
  output$classStatsBar <- renderPlotly({
    p <- ggplot(dat(), aes(variable, m)) +
      facet_wrap(~Instructor) +
      geom_col() +
      theme_bw()
    
    
    ggplotly(p)
  })
  
  output$classStats <- renderPlotly(
    p <- ggplot() +
      
    
    
    ggplotly(p)
  )
})