library(shiny)
shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel("Spooky Accurate Prediction"),
        sidebarPanel(
            textInput('phrase', 'Please begin typing what we already know you are going to say.')
        ),
        mainPanel(
            h3('Spooky Accurate Prediction'),
            h4('You have so far typed...'),
            verbatimTextOutput("inputValue"),
            h4('And your next word will be...'),
            verbatimTextOutput("prediction")
        )
    )
)
