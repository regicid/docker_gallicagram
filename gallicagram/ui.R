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

shinyUI(fluidPage(
    sidebarLayout(
        sidebarPanel(
            sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
        ),
        mainPanel(plotOutput("distPlot"))
    )
))