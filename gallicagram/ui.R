library(shiny)

shinyUI(fluidPage(
    sidebarLayout(
        sidebarPanel(
            sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
        ),
        mainPanel(plotOutput("distPlot"))
    )
))