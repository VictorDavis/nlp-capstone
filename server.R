library(shiny)
source("exploratory.R")
shinyServer(
    function(input, output) {
        output$inputValue <- renderPrint({input$phrase})
        output$prediction <- renderPrint({predictNextWord(input$phrase, 3)})
    }
)