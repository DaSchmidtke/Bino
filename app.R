library(shiny)

ui <- fluidPage(
  
  sliderInput(inputId = "leftHand",
               label = "Number of successful events",
               value = 100, min = 0, max = 300, width = "100%", step = 1),
  sliderInput(inputId = "totalEvents",
               label = "Total number of events",
               value = 200, min = 0, max = 300, width = "100%", step = 1),
  sliderInput(inputId = "probability",
               label = "Expected probability",
               value = 0.5, min = 0, max = 1, step = 0.1, width = "100%"),
  uiOutput(outputId = "binomialPvalue"),
  plotOutput(outputId = "hist"))

server <- function(input, output) {
  
  output$binomialPvalue <- renderUI({
    c("P-value = ", binom.test(input$leftHand, 
                               input$totalEvents, 
                               p=input$probability)[3])
  })
  
  output$hist <- renderPlot({
    
    x <- seq(0,
             input$totalEvents, 
             by = 1)
    y <- dbinom(x, 
                input$totalEvents, 
                input$probability)
    plot(x,y, xlab = "Number of events")
  })
  
  
}

shinyApp(ui = ui, server = server)
 
