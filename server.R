
# This is the server logic for a Shiny web application.

library(shiny)
source("./R/script.R")

shinyServer(function(input, output) {
    
    output$laborPlot <- renderPlot({
        
        # generate labor dataset based on input$... from ui.R
        labor <- calculateLabor(input$partBenefit
                                , input$topSalary
                                , input$rankShift)
        
        # draw the labor graph
        plotLabor(labor)
    })
})
