library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(Hmisc)
library(xml2)
library(markdown)
library(shinythemes)
library(htmlwidgets)
library(httr)
library(ngramr)
library(dplyr)
library(htmltools)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
})