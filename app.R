library(shiny)
options(scipen=999)

ui <- fluidPage(
  
  sliderInput(inputId = "success",
               label = "Number of successes",
               value = 100, min = 0, max = 1000, width = "100%", step = 1),
  sliderInput(inputId = "totalEvents",
               label = "Total number of trials",
               value = 200, min = 0, max = 1000, width = "100%", step = 1),
  sliderInput(inputId = "probability",
               label = "Estimated probability",
               value = 0.5, min = 0, max = 1, step = 0.001, width = "100%"),

  plotOutput(outputId = "hist"),
  htmlOutput(outputId = "binomialTest"),
  htmlOutput(outputId = "conclusion")
  
)

server <- function(input, output) {
  
  output$hist <- renderPlot({
    
    x <- seq(0,
             input$totalEvents, 
             by = 1)
    y <- dbinom(x, 
                input$totalEvents, 
                input$probability)
    plot(x,y, xlab = "Number of successes", ylab = "Probability", main = "Probability distribution")
    points(x[(input$success+1)], y[(input$success+1)], col = "red", pch = 16, cex = 1.5)
    points(c(x[(input$success+1)],x[(input$success+1)]), c(y[(input$success+1)],0), type = "l", col = "red")
  })
 
  output$binomialTest <- renderUI({ 
    
    sL <- nchar(as.character(binom.test(input$success, 
                                                  input$totalEvents, 
                                                  p=input$probability)[5]))
    
    if (nchar(as.character(binom.test(input$success, 
                                      input$totalEvents, 
                                      p=input$probability)[5])) >=35
    ) { sL = 35}
    
    if (binom.test(input$success, 
                  input$totalEvents, 
                  p=input$probability)[3] < 0.001) {
    HTML(
    c(paste("<h3>Results of the Binomial Test</h3><br>",
            "<b>Estimated probability of success:</b> ", 
            substr(as.character(binom.test(input$success, 
                                           input$totalEvents, 
                                           p=input$probability)[5]), start = 30, stop = sL-1),
            "<br><b>Confidence interval:</b> ",
            substr(as.character(unlist(binom.test(input$success, 
                                           input$totalEvents, 
                                           p=input$probability)[4])[1]),start = 1, stop = 5),
            ", ",
            substr(as.character(unlist(binom.test(input$success, 
                                           input$totalEvents, 
                                           p=input$probability)[4])[2]),start = 1, stop = 5),
            "<br><b>P-value:</b> <0.001"
            )))
    }
    
    else {
      
      HTML(
        c(paste("<h3>Results of the Binomial Test</h3><br>",
                "<b>Estimated probability of success:</b> ", 
                substr(as.character(binom.test(input$success, 
                           input$totalEvents, 
                           p=input$probability)[5]), start = 30, stop = sL-1),
                "<br><b>Confidence interval:</b> ",
                substr(as.character(unlist(binom.test(input$success, 
                                                      input$totalEvents, 
                                                      p=input$probability)[4])[1]),start = 1, stop = 5),
                ", ",
                substr(as.character(unlist(binom.test(input$success, 
                                                      input$totalEvents, 
                                                      p=input$probability)[4])[2]),start = 1, stop = 5),
                "<br><b>P-value:</b>",
                substr(as.character(unlist(binom.test(input$success, 
                                               input$totalEvents, 
                                               p=input$probability)[3])),start = 1, stop = 5)
        )))
      
    }
  })
  
  output$conclusion <- renderUI({ 
  
    if (binom.test(input$success, 
                   input$totalEvents, 
                   p=input$probability)[3] > 0.05) {
    
    HTML( c(paste("<b>Conclusion: </b>It may be that the true probability of success is equal to", input$probability)))
      
    }
    
    else if (binom.test(input$success, 
                   input$totalEvents, 
                   p=input$probability)[3] <= 0.05 && binom.test(input$success, 
                                                                 input$totalEvents, 
                                                                 p=input$probability)[3] > 0.01) {
      
      HTML( c(paste("<b>Conclusion: </b>It is unlikely that the true probability of success is equal to", input$probability)))
      
    }
    
    else if (binom.test(input$success, 
                        input$totalEvents, 
                        p=input$probability)[3] < 0.01) {
      
      HTML( c(paste("<b>Conclusion: </b>It is highly unlikely that the true probability of success is equal to", input$probability)))
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
