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
library(shinyWidgets)

shinyUI(navbarPage("Gallicagram",
                   tabPanel("Graphique",fluidPage(),
                            tags$head(
                                tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                                textInput("mot","Terme(s) à chercher","Joffre&Pétain&Foch"),
                                                conditionalPanel(condition="input.doc_type != 4",uiOutput("instructions")),
                                                conditionalPanel(condition="input.doc_type == 4",p('Recherche limitée à un seul syntagme')),
                                                selectInput("doc_type", "Corpus",choices = list("Presse / Gallica" = 1,"Recherche par titre de presse / Gallica" = 3, "Corpus personnalisé / Gallica"=4, "Livres / Gallica" = 2,"Livres / Ngram Viewer - Google Books" = 5),selected = 1),
                                                conditionalPanel(condition="input.doc_type == 3",selectizeInput("titres","Titre des journaux",choices = "",selected=NULL,multiple = TRUE)),
                                                conditionalPanel(condition="input.doc_type == 4",checkboxInput("occurrences_page", "Compter le nombre de pages correspondant à la recherche", value = FALSE)),
                                                conditionalPanel(condition="input.doc_type == 4",fileInput('target_upload','', 
                                                                                                           accept = c(
                                                                                                               'text/csv',
                                                                                                               'text/comma-separated-values',
                                                                                                               '.csv'
                                                                                                           ),buttonLabel='Importer', placeholder='un rapport de recherche')),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("beginning","Début",1914)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("end","Fin",1920)),
                                                conditionalPanel(condition="input.doc_type != 2 && input.doc_type != 5",
                                                                 selectInput("resolution", label = "Résolution", choices = c("Année","Mois"))),
                                                conditionalPanel(condition="input.doc_type == 2 || input.doc_type == 5",
                                                                 selectInput("resolution", label = "Résolution", choices = c("Année"))),
                                                conditionalPanel(condition="input.doc_type != 4 || (input.doc_type == 4 && output.fileUploaded == 1)",actionButton("do","Générer le graphique")),
                                                p(""),
                                                sliderInput("span","Lissage de la courbe",min = 0,max = 10,value = 0)
                                            ),
                                            
                                            mainPanel(dropdownButton(tags$h3("Options avancées"),
                                                                     checkboxInput("barplot", "Afficher la distribution des documents\nde la base Gallica sur la période", value = FALSE),
                                                                     checkboxInput("correlation_test", "Afficher les matrices de corrélation", value = FALSE),
                                                                     checkboxInput("delta", "Représenter la différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE),
                                                                     downloadButton("data_session","Télécharger les données de la session"),
                                                                     circle = TRUE, status = "default",
                                                                     icon = icon("sliders"), width = "300px",
                                                                     tooltip = tooltipOptions(title = "Afficher les options avancées")
                                                                     ),
                                                      plotlyOutput("plot"),
                                                      fluidRow(textOutput("legende"),align="right"),
                                                      conditionalPanel(condition="input.doc_type != 5",fluidRow(textOutput("legende0"),align="right")),
                                                      fluidRow(textOutput("legende1"),align="right"),
                                                      conditionalPanel(condition="input.doc_type != 5",fluidRow(textOutput("legende2"),align="right")),
                                                      conditionalPanel(condition="input.doc_type != 5",fluidRow(textOutput("legende3"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",p("")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr2"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(textOutput("pvalue"),align="right")),
                                                      div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadData', 'Télécharger les données')),
                                                      div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadPlot', 'Télécharger le graphique interactif')),
                                                      p(""),
                                                      h2(textOutput("currentTime"), style="color:white")
                                            ))),
                   tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                   tabPanel("Corpus de presse",fluidPage(),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                                radioButtons("corpus_structure_p", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Mode d'accès"=3,"Bibliothèque d'origine"=4, "Classement thématique de Dewey" = 5,"Périodicité" = 6),selected = 1),
                                                conditionalPanel(condition="input.corpus_structure_p!=0",checkboxInput("corpus_relative_p", "Afficher les résultats en valeurs relatives", value = FALSE))
                                            ),
                                            mainPanel(
                                                fluidRow(plotlyOutput("corpus1")),
                                                p("")
                                            )
                            )
                   ),
                   tabPanel("Corpus de livres",fluidPage(),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                                radioButtons("corpus_structure_l", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Droits d'auteur" = 3, "Bibliothèque d'origine" = 4,"Volume (nombre de pages moyen)" = 5,"Volume (nombre de pages médian)" = 6,"Etat de la numérisation"=7,"Qualité d'océrisation"=8,"Date de numérisation"=9, "Classement thématique de Dewey" = 10),selected = 1),
                                                conditionalPanel(condition="(input.corpus_structure_l==1 || input.corpus_structure_l==2 || input.corpus_structure_l==3 || input.corpus_structure_l==4 || input.corpus_structure_l==7 || input.corpus_structure_l==10)",
                                                                 checkboxInput("corpus_relative_l", "Afficher les résultats en valeurs relatives", value = FALSE)
                                                ),
                                                conditionalPanel(condition="input.corpus_structure_l==1",checkboxInput("corpus_ngram_l", "Distribution des livres dans Google Ngram Viewer", value = FALSE))
                                            ),
                                            mainPanel(
                                                conditionalPanel(condition="input.corpus_structure_l!=8",fluidRow(plotlyOutput("corpus2")),
                                                                 p("")),
                                                conditionalPanel(condition="input.corpus_structure_l==8",img(src = "nqamoyen.png", height = 589, width = 681)),
                                                conditionalPanel(condition="input.corpus_structure_l==9",fluidRow(plotlyOutput("corpus3"))),
                                                conditionalPanel(condition="input.corpus_structure_l==9",img(src = "numerisation.png", height = 589, width = 681)),
                                                p("")
                                            )
                            )
                   ),
                   tabPanel("Tutoriel",headerPanel("Tutoriel"),
                            fluidPage(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/SujS4t-ZGhQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                   tabPanel(title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/app/gallicapresse' target='_blank'>Gallicapresse"))
))