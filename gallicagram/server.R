library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(Hmisc)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
})