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
library(rclipboard)
library(lubridate)

shinyUI(navbarPage("Gallicagram",
                   tabPanel("Graphique",fluidPage(),
                            tags$head(includeHTML(("google-analytics.html"))),
                            fluidPage(
                                            column(4,wellPanel(
                                                div(style="display: inline-block;vertical-align:bottom;width: 78%;",textInput("mot","Recherche","Joffre&Pétain&Foch")),
                                                div(style="display: inline-block;vertical-align:bottom;width: 20%;",conditionalPanel(condition="input.cooccurrences==1 && (input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",numericInput("prox","Distance",20))),
                                                conditionalPanel(condition="(input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",div(style = "margin-top: -20px")),
                                                conditionalPanel(condition="(input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",checkboxInput("cooccurrences", "Explorer les cooccurrences", value = FALSE)),
                                                div(style = "margin-top: -15px"),
                                                uiOutput("instructions"),
                                                conditionalPanel(condition="input.doc_type == 4",p('Recherche limitée à un seul syntagme')),
                                                conditionalPanel(condition="(input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2",p('Recherche limitée à un seul syntagme dans 5 000 documents au maximum')),
                                                conditionalPanel(condition="((input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2) || input.doc_type == 4",textOutput("avertissement")),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",selectInput("language", "Langue",choices = list("Français" = 1, "Allemand" = 2, "Néerlandais"=3, "Anglais"=4, "Espagnol"=5),selected = 1)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",conditionalPanel(condition="input.language == 1",selectInput("bibli", "Source",choices = list("Gallica"=1,"Bibliothèques nationales"=2,"Bibliothèques régionales, locales ou spécialisées"=3,"Presse contemporaine"=4,"Corpus scientifiques"=5)))),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",selectInput("doc_type", "Corpus",choices = list("Presse française / Gallica" = 1,"Recherche par titre de presse / Gallica" = 3, "Corpus personnalisé / Gallica"=4, "Livres / Gallica" = 2,"Livres / Ngram Viewer - Google Books" = 5, "Presse allemande / Europeana" = 6, "Presse néerlandaise / Europeana" = 7, "Presse britannique / BNA" = 8, "Livres / Ngram Viewer Allemand" = 9, "Livres / Ngram Viewer Anglais" = 10, "Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12, "Presse wallonne / KBR"=13, "Presse flamande / KBR"=14, "Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse suisse-allemande / Bibliothèque nationale suisse"=16, "Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27, "Presse québécoise / BAnQ"=28, "Presse austro-hongroise / ANNO"=29, "Le Monde"=30, "Le Figaro"=31,"CAIRN"=32),selected = 1)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",selectInput("search_mode", "Mode de recherche",choices = list("Par n-gramme" = 3),selected = 3)),

                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 3",radioButtons("filtre", "",choices = list("Filtre thématique"=1,"Filtre géographique"=2),inline = T))),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 3",uiOutput("themes_presse"))),
                                                conditionalPanel(condition="input.doc_type == 3",uiOutput("titres")),
                                                
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 32",
                                                                 div(style = "margin-top: -15px"),
                                                                 selectInput("cairn", "Discipline",choices = list("_"=0,"Art" = 70, "Droit" = 2, "Economie, Gestion"=1, "Gégraphie"=30, "Histoire"=3, "Info.-Com."=9, "Intérêt général"=4, "Lettres et linguistique"=5, "Médecine"=139, "Philosophie"=6, "Psychologie"=7,"Santé publique"=141,"Sciences de l'éducation"=8, "Sciences politiques"=10, "Sociologie et société"=11, "Sport et société"=12),selected = 0))),
                                                div(style = "margin-top: -15px"),
                                                conditionalPanel(condition="input.doc_type == 4",fileInput('target_upload','', 
                                                                                                           accept = c(
                                                                                                               'text/csv',
                                                                                                               'text/comma-separated-values',
                                                                                                               '.csv'
                                                                                                           ),buttonLabel='Importer', placeholder='un rapport de recherche')),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",conditionalPanel(condition = "input.doc_type != 30 && input.doc_type != 31",numericInput("beginning","Début",1914))),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",conditionalPanel(condition = "input.doc_type != 30 && input.doc_type != 31",numericInput("end","Fin",1920))),
                                                conditionalPanel(condition = "input.doc_type == 30 || input.doc_type == 31",div(style = "margin-top: -60px")),
                                                conditionalPanel(condition = "input.doc_type == 30 || input.doc_type == 31", dateRangeInput('dateRange',
                                                                                                                                label = '\n',
                                                                                                                                start = as.Date.character("2021-01-01"), end = as.character(Sys.Date()),
                                                                                                                                separator="à", startview = "decade")),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",radioButtons("resolution", label = "Résolution", choices = c("Année","Mois"),inline=T)),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 2 && input.search_mode==1",uiOutput("theme"))),
                                                conditionalPanel(condition="input.search_mode == 3 || input.doc_type == 1 || (input.doc_type == 3 && input.search_mode == 1) || input.doc_type == 5 || input.doc_type == 6 || input.doc_type == 7 || input.doc_type == 8 || input.doc_type == 9 || input.doc_type == 10 || input.doc_type == 11 || input.doc_type == 12 || input.doc_type == 13 || input.doc_type == 14 || input.doc_type == 15 || input.doc_type == 16 || input.doc_type == 17 || input.doc_type == 18 || input.doc_type == 19 || input.doc_type == 20 || input.doc_type == 21 || input.doc_type == 22 || input.doc_type == 23 || input.doc_type == 24 || input.doc_type == 25 || input.doc_type == 26 || input.doc_type == 27 || input.doc_type == 28 || input.doc_type == 29 || input.doc_type == 30 || input.doc_type == 31 || input.doc_type == 32 || (input.doc_type == 2 && input.search_mode == 1) || (input.doc_type == 4 && output.fileUploaded == 1 && output.avertissement.includes('Modifiez')==false) || ((input.doc_type == 2 || (input.doc_type == 3  && input.titres.length <= 15)) && input.search_mode == 2 && output.avertissement.includes('Modifiez')==false)",actionButton("do","Générer le graphique"))
                                                
                                            )),
                                            
                                            column(8,rclipboardSetup(),
                                                      div(style="display: inline-block;vertical-align:bottom",dropdownButton(tags$h3("Options avancées"),
                                                                     checkboxInput("barplot", "Afficher la distribution des documents de la base de données ", value = FALSE),
                                                                     checkboxInput("correlation_test", "Afficher les matrices de corrélation", value = FALSE),
                                                                     checkboxInput("delta", "Représenter la différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE),
                                                                     checkboxInput("scale", "Rééchelonner les résultats", value = FALSE),
                                                                     checkboxInput("multicourbes", "Afficher toutes les données de la session dans le graphique", value = FALSE),
                                                                     checkboxInput("loess", "Lissage loess pour afficher des tendances", value = FALSE),
                                                                     checkboxInput("histogramme", "Mode histogramme", value = FALSE),
                                                                     downloadButton("data_session","Télécharger les données de la session"),
                                                                     circle = TRUE, status = "default",
                                                                     icon = icon("sliders"), width = "300px",
                                                                     tooltip = tooltipOptions(title = "Afficher les options avancées")
                                                                     )),
                                                      div(style="display: inline-block;vertical-align:top;float:right",rclipButton("clipbtn", "Citer cet outil",clipText = "Gallicagram par Benjamin Azoulay et Benoît de Courson",icon = icon("clipboard"))),
                                                      plotlyOutput("plot")),
                                            column(4,
                                                   sliderInput("span","Lissage de la courbe",min = 0,max = 10,value = 0),
                                                   p(""),
                                                   div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadData', 'Données')),
                                                   div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadPlot', 'Graphique interactif')),
                                                   div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadSPlot', 'Graphique scientifique')),
                                                   p(""),
                                                   
                                                   h2(textOutput("currentTime"), style="color:white")),
                                              column(4,
                                                      fluidRow(uiOutput("legende"),align="right"),
                                                      fluidRow(textOutput("legende0"),align="right"),
                                                      fluidRow(textOutput("legende1"),align="right"),
                                                      fluidRow(textOutput("legende4"),align="right"),
                                                      fluidRow(textOutput("legende3"),align="right"),
                                                      fluidRow(textOutput("legende2"),align="right"),
                                                      conditionalPanel(condition="input.correlation_test",p("")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr2"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(textOutput("pvalue"),align="right"))
                                                     )
                                              )),
                   tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                   tabPanel("Distributions",fluidPage(),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                              selectInput("d_language", "Langue",choices = list("Français" = 1, "Allemand" = 2, "Néerlandais"=3, "Anglais"=4, "Espagnol"=5),selected = 1),
                                              conditionalPanel(condition="input.d_language == 1",radioButtons("d_bibli", "",choices = list("Gallica"=1,"Bibliothèques nationales"=2,"Bibliothèques régionales, locales ou spécialisées"=3),inline = T)),
                                              selectizeInput("distribution","Corpus",choices = list("Presse française / Gallica" = 1, "Livres français / Gallica" = 2,"Livres français / Ngram Viewer" = 5, "Presse allemande / Europeana" = 6, "Presse néerlandaise / Europeana" = 7, "Presse britannique / BNA" = 8, "Livres / Ngram Viewer Allemand" = 9, "Livres / Ngram Viewer Anglais" = 10, "Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12, "Presse québécoise / BAnQ"=28 , "Presse wallonne / KBR"=13, "Presse flamande / KBR"=14, "Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse suisse-allemande / Bibliothèque nationale suisse"=16, "Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27), selected=1),
                                            ),
                                              mainPanel(
                                                plotlyOutput("d_plot")
                                              )
                            )),
                   tabPanel("Corpus de presse",fluidPage(),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                                radioButtons("corpus_structure_p", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Mode d'accès"=3,"Bibliothèque d'origine"=4, "Classement thématique de Dewey" = 5,"Périodicité" = 6),selected = 1),
                                                conditionalPanel(condition="input.corpus_structure_p!=0",checkboxInput("corpus_relative_p", "Afficher les résultats en valeurs relatives", value = FALSE))
                                            ),
                                            mainPanel(
                                                fluidRow(plotlyOutput("corpus1")),
                                                conditionalPanel(condition="input.corpus_structure_p!=1",fluidRow(tableOutput('total_table'))),
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
                                                conditionalPanel(condition="input.corpus_structure_l!=1 && input.corpus_structure_l!=5 && input.corpus_structure_l!=8 && input.corpus_structure_l!=9",fluidRow(tableOutput('total_table_bis'),
                                                                                                                  p(""))),
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
                   tabPanel("Tutoriel",
                            fluidPage(
                              h3("Tutoriel et Séminaire de présentation"),
                              div(style="display: inline-block;vertical-align:bottom",fluidRow(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/SujS4t-ZGhQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                              div(style="display: inline-block;vertical-align:bottom",fluidRow(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/jMyeFT5Ny3s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                              p(HTML("<li><a href='http://savoirs.ens.fr/expose.php?id=3989' target='_blank'>Lien vers la vidéo du séminaire DHAI du 11 mai 2021</a>")),
                              h3("Présentation de Gallicagram"),
                              fluidRow(uiOutput("pdfview"))
                              )),
                   tabPanel(title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/app/gallicapresse' target='_blank'>Gallicapresse</a>")),
                   tabPanel(title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/app/gallicanet' target='_blank'>Gallicanet</a>"))
))