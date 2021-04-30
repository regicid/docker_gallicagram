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
                                                textInput("mot","Recherche","Joffre&Pétain&Foch"),
                                                uiOutput("instructions"),
                                                conditionalPanel(condition="input.doc_type == 4",p('Recherche limitée à un seul syntagme')),
                                                conditionalPanel(condition="(input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2",p('Recherche limitée à un seul syntagme dans 5 000 documents au maximum')),
                                                conditionalPanel(condition="((input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2) || input.doc_type == 4",textOutput("avertissement")),
                                                conditionalPanel(condition="input.language == 1",radioButtons("bibli", "",choices = list("Gallica"=1,"Bibliothèques nationales"=2,"Bibliothèques régionales, locales ou spécialisées"=3),inline = T)),
                                                selectInput("doc_type", "Corpus",choices = list("Presse française / Gallica" = 1,"Recherche par titre de presse / Gallica" = 3, "Corpus personnalisé / Gallica"=4, "Livres / Gallica" = 2,"Livres / Ngram Viewer - Google Books" = 5, "Presse allemande / Europeana" = 6, "Presse néerlandaise / Europeana" = 7, "Presse britannique / BNA" = 8, "Livres / Ngram Viewer Allemand" = 9, "Livres / Ngram Viewer Anglais" = 10, "Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12, "Presse wallonne / KBR"=13, "Presse flamande / KBR"=14, "Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse suisse-allemande / Bibliothèque nationale suisse"=16, "Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27, "Presse québécoise / BAnQ"=28),selected = 1),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",selectInput("language", "Langue",choices = list("Français" = 1, "Allemand" = 2, "Néerlandais"=3, "Anglais"=4, "Espagnol"=5),selected = 1)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",selectInput("search_mode", "Mode de recherche",choices = list("Par document" = 1),selected = 1)),

                                                conditionalPanel(condition="input.doc_type == 3",selectInput("theme_presse", "Thématique",choices = list("Liste de titres personnalisée"=1,"Principaux quotidiens"=2,"Presse littéraire"=3))),
                                                conditionalPanel(condition="input.doc_type == 3",uiOutput("titres")),
                                                conditionalPanel(condition="input.doc_type == 4",fileInput('target_upload','', 
                                                                                                           accept = c(
                                                                                                               'text/csv',
                                                                                                               'text/comma-separated-values',
                                                                                                               '.csv'
                                                                                                           ),buttonLabel='Importer', placeholder='un rapport de recherche')),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("beginning","Début",1914)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("end","Fin",1920)),
                                                radioButtons("resolution", label = "Résolution", choices = c("Année","Mois"),inline=T),
                                                conditionalPanel(condition="input.doc_type == 1 || (input.doc_type == 3 && input.search_mode == 1) || input.doc_type == 5 || input.doc_type == 6 || input.doc_type == 7 || input.doc_type == 8 || input.doc_type == 9 || input.doc_type == 10 || input.doc_type == 11 || input.doc_type == 12 || input.doc_type == 13 || input.doc_type == 14 || input.doc_type == 15 || input.doc_type == 16 || input.doc_type == 17 || input.doc_type == 18 || input.doc_type == 19 || input.doc_type == 20 || input.doc_type == 21 || input.doc_type == 22 || input.doc_type == 23 || input.doc_type == 24 || input.doc_type == 25 || input.doc_type == 26 || input.doc_type == 27 || input.doc_type == 28 || (input.doc_type == 2 && input.search_mode == 1) || (input.doc_type == 4 && output.fileUploaded == 1 && output.avertissement.includes('Modifiez')==false) || ((input.doc_type == 2 || input.doc_type == 3) && input.search_mode == 2 && output.avertissement.includes('Modifiez')==false)",actionButton("do","Générer le graphique")),
                                                p(""),
                                                sliderInput("span","Lissage de la courbe",min = 0,max = 10,value = 0)
                                            ),
                                            
                                            mainPanel(dropdownButton(tags$h3("Options avancées"),
                                                                     checkboxInput("barplot", "Afficher la distribution des documents de la base de données ", value = FALSE),
                                                                     checkboxInput("correlation_test", "Afficher les matrices de corrélation", value = FALSE),
                                                                     checkboxInput("delta", "Représenter la différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE),
                                                                     checkboxInput("scale", "Rééchelonner les résultats", value = FALSE),
                                                                     # checkboxInput("ponderation", "Pondérer les résultats par le volume de la base", value = FALSE),
                                                                     checkboxInput("multicourbes", "Afficher toutes les données de la session dans le graphique", value = FALSE),
                                                                     downloadButton("data_session","Télécharger les données de la session"),
                                                                     circle = TRUE, status = "default",
                                                                     icon = icon("sliders"), width = "300px",
                                                                     tooltip = tooltipOptions(title = "Afficher les options avancées")
                                                                     ),
                                                      plotlyOutput("plot"),
                                                      fluidRow(uiOutput("legende"),align="right"),
                                                      fluidRow(textOutput("legende0"),align="right"),
                                                      fluidRow(textOutput("legende1"),align="right"),
                                                      fluidRow(textOutput("legende4"),align="right"),
                                                      fluidRow(textOutput("legende3"),align="right"),
                                                      fluidRow(textOutput("legende2"),align="right"),
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
                                                radioButtons("corpus_structure_l", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Droits d'auteur" = 3, "Bibliothèque d'origine" = 4,"Volume en nombre de pages" = 5,"Etat de la numérisation"=7,"Qualité d'océrisation"=8,"Date de numérisation"=9, "Classement thématique de Dewey" = 10),selected = 1),
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
                                                conditionalPanel(condition="input.corpus_structure_l==5",fluidRow(plotlyOutput("corpus4")),
                                                                 p(""),
                                                                 fluidRow(plotlyOutput("corpus5"))),
                                                p("")
                                            )
                            )
                   ),
                   tabPanel("Tutoriel",headerPanel("Tutoriel"),
                            fluidPage(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/SujS4t-ZGhQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                   tabPanel(title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/app/gallicapresse' target='_blank'>Gallicapresse"))
))