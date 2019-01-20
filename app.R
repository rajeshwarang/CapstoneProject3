

library(shiny)
library(tidytext)
library(tidyverse)
library(stringr)
library(knitr)
library(wordcloud)
library(ngram)


source("ModelFunctions.R")


ui <- fluidPage(

    titlePanel("Data Science Capstone Project - Next Word Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      "Enter a short phrase in English and Click on the Submit Button",	
      textInput(inputId="text", label = ""),
      submitButton("SUBMIT")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Result", 
                 conditionalPanel(condition = "input.text != ''",
                                  verbatimTextOutput("text"),
                                  selectInput("predicts","Word predictions:",choices=c("")),
                                  verbatimTextOutput("caption1")
                                  
                                  
                                  
                                  )
        )
      )
    )
  )
)




server <- function(input, output, session) {
  
  output$text <- renderText({
    paste("The entered text is:", input$text)
  })
  
  
  observe({
    
    textCleansed <- input$text
    
    output$cleaned <- renderText({
      paste0("Cleansed text: [",textCleansed,"]")
    })
    
    predictWords <- ngrams(textCleansed)
    
    updateSelectInput(session = session, inputId = "predicts", choices = predictWords)
    
    output$caption1 <- renderText(paste("The entered text along with predicted text :",
                                        textCleansed , 
                                        predictWords))
    
  })
  
  
}





shinyApp(ui = ui, server = server)

  