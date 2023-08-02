library(shiny)
library(plotly)
library(stringr)
library(xml2)
library(markdown)
library(shinythemes)
library(htmlwidgets)
library(shinybusy)
library(httr)
library(dplyr)
library(htmltools)
library(shinyWidgets)
library(rclipboard)
library(lubridate)
library(leaflet)
library(shinyjs)
library(DT)
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

shinyUI(bootstrapPage(
  theme=NULL,
  uiOutput("style"),
  #tags$head(tags$script(src = "enter_button.js")),
  tags$head(includeHTML(("google-analytics.html"))),
  mobileDetect('isMobile'),
  useShinyjs(),
  navbarPage(id="#navbar",title=div(img(src="Logoo.png",
                                        height = "50px",
                                        style = "position: relative;top: -17px;")),collapsible=TRUE,
                   tabPanel("Graphique",
                            use_busy_spinner(spin = "fingerprint",position="bottom-right",color="#FF0000",spin_id = "contexte"),
                            use_busy_spinner(spin = "fingerprint",position="full-page",color="#FF0000",spin_id = "ngram"),
                              div(id="Sidebar",column(4,wellPanel(
                                div(style="display: inline-block;vertical-align:top;",id="menumob2",actionButton("showSidebar2", "",icon = icon("bars"))),
                                                div(style="display: inline-block;vertical-align:bottom;width: 78%;",textInput("mot","Recherche","liberté&république")),
                                                div(style="display: inline-block;vertical-align:bottom;width: 20%;",
                                                    conditionalPanel(condition="input.cooccurrences==1 && (input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",numericInput("prox","Distance",20))
                                                    ),
                                                conditionalPanel(condition="(input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",div(style = "margin-top: -20px")),
                                                conditionalPanel(condition="(input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",checkboxInput("cooccurrences", "Explorer les cooccurrences", value = FALSE)),
                                                
                                                conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2 || input.doc_type==30) && input.search_mode == 3 && input.joker == 1",
                                                                 div(style="display: inline-block;vertical-align:bottom;width: 38%;",numericInput("stpw","Mots vides ignorés",500)),
                                                                 div(style="display: inline-block;vertical-align:bottom;width: 39%;",numericInput("nbJoker","Nombre de jokers",5))
                                                                 ),
                                                conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2 || input.doc_type==30) && input.search_mode == 3",div(style = "margin-top: -20px")),
                                                conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2 || input.doc_type==30) && input.search_mode == 3 && input.joker == 0",div(style = "margin-top: -20px")),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2 || input.doc_type==30) && input.search_mode == 3",checkboxInput("joker", "Mode joker", value = FALSE))),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2 || input.doc_type==30) && input.search_mode == 3",checkboxInput("gallicloud", "Gallicloud", value = FALSE))),
                                                div(style = "margin-top: -15px"),
                                                uiOutput("instructions"),
                                                conditionalPanel(condition="input.doc_type == 4",p('Recherche limitée à un seul syntagme')),
                                                conditionalPanel(condition="(input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2",p('Recherche limitée à un seul syntagme dans 5 000 documents au maximum')),
                                                conditionalPanel(condition="((input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2) || input.doc_type == 4",textOutput("avertissement")),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",selectInput("language", "Langue",choices = list("Français" = 1, "Allemand" = 2, "Anglais"=4, "Néerlandais"=3, "Espagnol"=5),selected = 1)),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.language == 1",selectInput("bibli", "Source",choices = list("Gallicagram"=0,"Gallica"=1,"Presse contemporaine"=4,"Google"=8,"Bibliothèques nationales"=2,"Bibliothèques régionales, locales ou spécialisées"=3,"Corpus scientifiques"=5,"Paroles de chansons"=7),selected=0))),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",selectInput("doc_type", "Corpus",choices = list("Presse française / Gallica" = 1,"Recherche par titre de presse / Gallica" = 3,
                                                                                                                                                                 "Corpus personnalisé / Gallica"=4, "Livres / Gallica" = 2,
                                                                                                                                                                 "Livres / Ngram Viewer - Google Books" = 5, "Presse allemande / Europeana" = 6,
                                                                                                                                                                 "Presse néerlandaise / Europeana" = 7, "Presse britannique / BNA" = 8,
                                                                                                                                                                 "Livres / Ngram Viewer Allemand" = 9, "Livres / Ngram Viewer Anglais" = 10,
                                                                                                                                                                 "Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12,
                                                                                                                                                                 "Presse wallonne / KBR"=13, "Presse flamande / KBR"=14,
                                                                                                                                                                 "Presse suisse-romande / Bibliothèque nationale suisse"=15,
                                                                                                                                                                 "Presse suisse-allemande / Bibliothèque nationale suisse"=16,
                                                                                                                                                                 "Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18,
                                                                                                                                                                 "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20,
                                                                                                                                                                 "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22,
                                                                                                                                                                 "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24,
                                                                                                                                                                 "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26,
                                                                                                                                                                 "Presse de Roubaix / BN-R"=27, "Presse québécoise / BAnQ"=28,
                                                                                                                                                                 "Presse austro-hongroise / ANNO"=29, "Le Monde"=30, "Le Figaro"=31,
                                                                                                                                                                 "Cairn.info"=32,"Theses.fr"=33,"Persée"=34,"Presse australienne / Trove"=35,
                                                                                                                                                                 "Isidore"=36,"Presse américaine / newspapers.com"=37,"Presse canadienne / newspapers.com"=38,
                                                                                                                                                                 "Presse britannique / newspapers.com"=39,"Presse australienne / newspapers.com"=40,
                                                                                                                                                                 "Presse américaine / Library of Congress"=42,"Presse allemande / Deutsche digitale bibliothek"=43,
                                                                                                                                                                 "Google Trends / France"=44, "MusixMatch / Français"=45, "MusixMatch / Anglais"=46,
                                                                                                                                                                 "MusixMatch / Allemand"=47, "MusixMatch / Néerlandais"=48, "MusixMatch / Espagnol"=49,
                                                                                                                                                                 "Presse française / MediaCloud"=50,  "Presse anglophone / MediaCloud"=51,
                                                                                                                                                                 "Presse germanophone / MediaCloud"=52,  "Presse néerlandophone / MediaCloud"=53,
                                                                                                                                                                 "Presse hispanophone / MediaCloud"=54,"Le Marin"=55, "Livres+Presse / Gallica"=56,
                                                                                                                                                                 "Presse luxembourgeoise / eLuxemburgensia"=57,"Google Trends / Grande-Bretagne"=58,
                                                                                                                                                                 "Google Trends / Etats-Unis"=59,"Google Trends / Australie"=60,"Google Trends / Espagne"=61,
                                                                                                                                                                 "Google Trends / Allemagne"=62,"Google Trends / Autriche"=63,"Google Trends / Pays-Bas"=64,
                                                                                                                                                                 "The New York Times"=65,
                                                                                                                                                                 "Le Figaro (1854-1952)"=66,"L'Humanité (1904-1952)"=67,"Le Constitutionnel (1821-1913)"=68,
                                                                                                                                                                 "Le Journal de Paris (1777-1827)"=69,"Le Moniteur universel (1789-1901)"=70,
                                                                                                                                                                 "Le Temps (1861-1942)"=71
                                                                                                                                                                 ),selected = 1)),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",selectInput("search_mode", "Mode de recherche",choices = list("Par n-gramme" = 3),selected = 3)),

                                                conditionalPanel(condition="input.doc_type == 3",
                                                  div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 3",radioButtons("filtre", "",choices = list("Filtre thématique"=1,"Filtre géographique"=2),inline = T))),
                                                  div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 3",uiOutput("themes_presse"))),
                                                  conditionalPanel(condition="input.doc_type == 3",uiOutput("titres"))),
                                                
                                                conditionalPanel(condition="input.doc_type == 32 || input.doc_type == 33 || input.doc_type == 36 || input.doc_type == 45 || input.doc_type == 46 || input.doc_type == 47 || input.doc_type == 48 || input.doc_type == 49",
                                                  div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 32",
                                                                 selectInput("cairn", "Discipline",choices = list("_"=0,"Art" = 70, "Droit" = 2, "Economie, Gestion"=1, "Gégraphie"=30, "Histoire"=3, "Info.-Com."=9, "Intérêt général"=4, "Lettres et linguistique"=5, "Médecine"=139, "Philosophie"=6, "Psychologie"=7,"Santé publique"=141,"Sciences de l'éducation"=8, "Sciences politiques"=10, "Sociologie et société"=11, "Sport et société"=12),selected = 0))),
                                                  div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 33",
                                                                                                                                  selectInput("theses", "Discipline",choices = list("_","Médecine","Physique","Informatique","Chimie","Sciences économiques","Histoire","Sciences biologiques et fondamentales appliquées. Psychologie","Sciences appliquées","Sciences de gestion","Psychologie","Pharmacie","Sociologie","Droit public","Droit privé","Philosophie","Droit","Mathématiques","Géographie","Sciences de l'éducation","Sciences juridiques","Mécanique","Science politique","Terre, océan, espace","Littérature française","Neurosciences","Sciences du langage","Sciences médicales","Mathématiques appliquées","Sciences de l'information et de la communication","Biologie","Sciences de la vie et de la sante","Sciences","Linguistique","Aspects moléculaires et cellulaires de la biologie","Chimie organique","Génie électrique","Histoire de l'art","Médecine générale","Génie des procédés","Électronique","Immunologie","Sciences de la vie et de la santé","Sciences et techniques","Génie civil","Sciences des matériaux","Aspects moleculaires et cellulaires de la biologie","Lettres","Sciences de la vie","Arts","Sciences biologiques fondamentales et appliquées","Sciences de l'univers","Biologie Santé","Automatique","Art et archéologie","Biologie cellulaire et moléculaire","Ethnologie","Mécanique des fluides","Littérature comparée","Chimie des matériaux","Histoire et civilisations","Chimie - Physique","Génie mécanique","Sciences de Gestion","Sciences et techniques communes","Sciences economiques","Biologie cellulaire","Langues et litteratures etrangeres","Geographie","Mathematiques","Electronique, microelectronique, optique et lasers, optoelectronique microondes robotique","Sciences politiques","Sciences et techniques des activités physiques et sportives","Archéologie","Études anglophones","Biochimie","Sciences de la vie et de la santé","Sciences de l'education","Electronique","Langues et litteratures francaises","Droit privé et sciences criminelles","Acoustique","Sciences pour l'ingénieur","Microbiologie","Physiologie et biologie des organismes - populations - interactions","Sciences physiques","Musicologie","Anthropologie","Gestion","Sciences de la Terre","Sociologie, demographie","Sciences agronomiques","Histoire du droit","Sciences du langage - linguistique","Géosciences","Histoire moderne et contemporaine","Biochimie et biologie moléculaire","Sciences biologiques","Études germaniques","Histoire contemporaine","Études anglaises","Sciences et génie des matériaux","Littérature générale et comparée","Sciences biologiques et fondamentales appliquées","Énergétique","Anthropologie sociale et ethnologie","Chimie organique, minérale, industrielle","Sciences de la Vie et de la Santé","Sciences biologiques","Philosophie, epistemologie","Sciences et technologie industrielles","Droit international","Biologie des populations et écologie","Microélectronique","Langue et littérature françaises","Energétique","Archeologie, ethnologie, prehistoire","Matériaux","Lettres modernes","Chimie analytique","Mécanique et énergétique","Dynamique des fluides","Odontologie","Medecine","Littératures française et francophone","Mathématiques et applications","Génie industriel","Science des matériaux","Microbiologie","Pharmacologie","Physique théorique","Sciences de l'ingénieur","Physique des particules","Sciences chimiques","Architecture","Études ibériques","Physico-Chimie de la Matière Condensée","Robotique","Sciences religieuses","Génie Civil","Génie des matériaux","Chimie physique","Économie","Sciences appliquées. Physique","Informatique et applications","Astrophysique","Sciences cognitives","Traitement du signal et des images","Génétique","Nanophysique","Recherche clinique, innovation technologique, sante publique","Milieux denses et matériaux","Sciences des religions","Génie biologique et médical","Littérature et civilisation françaises","Géophysique","Traitement du signal et télécommunications","Mecanique des fluides, energetique, thermique, combustion, acoustique","Etudes anglophones","Mécanique des solides, des matériaux, des structures et des surfaces","Géologie","Lettres, sciences humaines et sociales","Physiologie et physiopathologie","Informatique et réseaux","Sciences naturelles","Histoire du droit et des institutions","Musique et musicologie","Physiologie","Chimie Physique","Matériaux, mécanique, génie civil, électrochimie","Matériaux, Mécanique, Génie civil, Electrochimie","Droit Public","?","Arts plastiques","Génie des procédés et des produits","Sciences Economiques","Mathématiques pures","Traitement du signal et télécommunications","Électronique, optronique et systèmes","Science et génie des matériaux","Astronomie et Astrophysique","Histoire médiévale","Études grecques","Électrochimie","Genie electrique","Biochimie. Biologie moléculaire et cellulaire","Études nord-américaines","Staps","Anglais","Litterature generale et comparee","Informatique, données, IA","Études latines","Cinéma","Chimie théorique, physique, analytique","Études théâtrales","Préhistoire","Sciences de l'information","Études italiennes","Cancérologie","Mathématiques fondamentales","Sciences de l'environnement"),selected = "_"))),
                                                  div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 36",
                                                                                                                                  div(style = "margin-top: -15px"),
                                                                                                                                  selectInput("isidore", "Discipline",choices = list("_","Archéologie et Préhistoire"="archeo","Architecture, aménagement de l'espace"="archi","Art et histoire de l'art"="art","Anthropologie biologique"="anthro-bio","Etudes classiques"="class","Sciences de l'information et de la communication"="info","Héritage culturel et muséologie"="museo","Démographie"="demo","Economies et finances"="eco","Education"="edu","Etudes de l'environnement"="envir","Etudes sur le genre"="genre","Géographie"="geo","Histoire"="hist","Histoire, Philosophie et Sociologie des sciences"="hisphilso","Droit"="droit","Linguistique"="langue","Littératures"="litt","Gestion et management"="gestion","Méthodes et statistiques"="stat","Musique, musicologie et arts de la scène"="musiq","Philosophie"="phil","Science politique"="scipo","Psychologie"="psy","Religions"="relig","Anthropologie sociale et ethnologie"="anthro-se","Sociologie"="socio"),selected = "_"))),
                                                  div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 45 || input.doc_type == 46 || input.doc_type == 47 || input.doc_type == 48 || input.doc_type == 49",
                                                                                                                                    div(style = "margin-top: -30px"),
                                                                                                                                    selectInput("musixmatch_genre", "Genre",choices = list("_"=0, "Blues"=2, "Country"=6, "Electronic"=7,"Jazz"=11, "Pop"=14, "R&B/Soul"=15, "Dance"=17, "Soundtrack"=16, "Hip-Hop/Rap"=18, "World"=19, "Alternative"=20, "Rock"=21, "Vocal"=23, "Reggae"=24, "Latin"=12),selected = 0)))
                                                ),
                                                div(style = "margin-top: -15px"),
                                                conditionalPanel(condition="input.doc_type == 4",fileInput('target_upload','', 
                                                                                                           accept = c(
                                                                                                               'text/csv',
                                                                                                               'text/comma-separated-values',
                                                                                                               '.csv'
                                                                                                           ),buttonLabel='Importer', placeholder='un rapport de recherche')),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",conditionalPanel(condition = "input.resolution != 'Semaine'",numericInput("beginning","Début",1788))),
                                                div(style="display: inline-block;vertical-align:bottom;width: 49%;",conditionalPanel(condition = "input.resolution != 'Semaine'",numericInput("end","Fin",1805))),
                                                conditionalPanel(condition = "input.doc_type == 30 || input.doc_type == 31",div(style = "margin-top: -15px")),
                                                conditionalPanel(condition = "input.resolution == 'Semaine'", dateRangeInput('dateRange',
                                                                                                                                label = '\n',
                                                                                                                                start = as.Date.character("2021-01-01"), end = as.character(Sys.Date()),
                                                                                                                                separator="à", startview = "decade")),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",radioButtons("resolution", label = "Résolution", choices = c("Année","Mois"),inline=T)),
                                                div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 2 && input.search_mode==1",uiOutput("theme"))),
                                                actionButton("do","Générer le graphique")
                                                # conditionalPanel(condition="input.search_mode == 3 || input.doc_type == 1 || (input.doc_type == 3 && input.search_mode == 1) || input.doc_type == 5 || input.doc_type == 6 || input.doc_type == 7 || input.doc_type == 8 || input.doc_type == 9 || input.doc_type == 10 || input.doc_type == 11 || input.doc_type == 12 || input.doc_type == 13 || input.doc_type == 14 || input.doc_type == 15 || input.doc_type == 16 || input.doc_type == 17 || input.doc_type == 18 || input.doc_type == 19 || input.doc_type == 20 || input.doc_type == 21 || input.doc_type == 22 || input.doc_type == 23 || input.doc_type == 24 || input.doc_type == 25 || input.doc_type == 26 || input.doc_type == 27 || input.doc_type == 28 || input.doc_type == 29 || input.doc_type == 30 || input.doc_type == 31 || input.doc_type == 32 ||input.doc_type == 33 ||input.doc_type == 34 ||input.doc_type == 35 ||input.doc_type == 36 ||input.doc_type == 37 ||input.doc_type == 38 ||input.doc_type == 39 ||input.doc_type == 40 ||input.doc_type == 42 ||input.doc_type == 43 || input.doc_type == 44 || input.doc_type == 45 || input.doc_type == 46 || input.doc_type == 47 || input.doc_type == 48 || input.doc_type == 49 || input.doc_type == 50 || input.doc_type == 51 || input.doc_type == 52 || input.doc_type == 53 || input.doc_type == 54 || input.doc_type == 55 || input.doc_type == 56 || input.doc_type == 57 || input.doc_type == 58 || input.doc_type == 59 || input.doc_type == 60 || input.doc_type == 61 || input.doc_type == 62 || input.doc_type == 63 || input.doc_type == 64 || input.doc_type == 65 || (input.doc_type == 2 && input.search_mode == 1) || (input.doc_type == 4 && output.avertissement.includes('Modifiez')==false) || ((input.doc_type == 2 || (input.doc_type == 3  && input.titres.length <= 15)) && input.search_mode == 2 && output.avertissement.includes('Modifiez')==false)",actionButton("do","Générer le graphique"))
                                                
                                            ))),
                                            
                                            div(column(8,rclipboardSetup(),
                                                       fixedRow(
                                                      div(id="menumob",style="display: inline-block;vertical-align:middle;",actionButton("showSidebar", "",icon = icon("bars"))),
                                                      div(style="display: inline-block;vertical-align:middle;",dropdownButton(tags$h3("Options avancées"),
                                                                    div(style="display: inline-block;vertical-align:bottom;",materialSwitch(inputId = "modemob", label = icon("moon"),
                                                                                                                                   right=TRUE,status = "default",inline = T,width = "150%")),
                                                                    div(style = "margin-top: -20px"),
                                                                    div(id="modemob2",style = "margin-top: -40px"), 
                                                                    checkboxInput("afcline", "Epurer l'ACP-AFC", value = FALSE),
                                                                    div(style = "margin-top: -20px"),
                                                                    div(id="afcspace",style = "margin-top: -30px"), 
                                                                    checkboxInput("afcmois", "Saisonnaliser l'AFC", value = FALSE),
                                                                    div(style = "margin-top: -20px"),
                                                                    div(id="afcspace1",style = "margin-top: -30px"), 
                                                                    checkboxInput("barplot", "Distribution des documents de la base de données ", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("correlation_test", "Matrices de corrélation", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("delta", "Différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("fraction", "Rapport de fréquence entre les deux premiers termes F(a)/F(b)", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("scale", "Rééchelonnement des résultats", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("multicourbes", "Afficher toutes les données de la session dans le graphique", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("loess", "Lissage loess (tendances)", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("saisons", "Moyennes mensuelles/saisonnalité", value = FALSE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("spline", "Spline dans le graphe scientifique", value = TRUE),
                                                                     div(style = "margin-top: -15px"),
                                                                     checkboxInput("points", "Points dans le graphe scientifique", value = TRUE),
                                                                     div(style = "margin-top: -15px"),
                                                                     actionButton("lemmatiseur","Formes fléchies"),
                                                                     downloadButton("data_session","Données de la session"),
                                                                     circle = TRUE, status = "default",
                                                                     icon = icon("sliders"), width = "300px",
                                                                     tooltip = tooltipOptions(title = "Afficher les options avancées")
                                                                     )),
                                                      
                                                      div(style="display: inline-block;vertical-align:middle;float:right",actionButton("flag",label = img (src="English version.png", width="15", height="15"),onclick ="window.open('https://shiny.ens-paris-saclay.fr/app/gallicagram_en', '_blank')")),
                                                      div(style="display: inline-block;vertical-align:middle;float:right",actionButton("twitter",label = img (src="twitter.png", width="15", height="15"),onclick ="window.open('https://twitter.com/gallicagram', '_blank')")),
                                                      div(style="display: inline-block;vertical-align:middle;float:right",actionButton("fb",label = img (src="facebook.png", width="15", height="15"),onclick ="window.open('https://www.facebook.com/gallicagram', '_blank')")),
                                                      div(id="clip",style="display: inline-block;vertical-align:middle;float:right",rclipButton("clipbtn", "Citation",clipText = "Azoulay, B., & de Courson, B. (2021, December 8). Gallicagram : un outil de lexicométrie pour la recherche. https://doi.org/10.31235/osf.io/84bf3",icon = icon("clipboard"))),
                                                      div(id="article",style="display: inline-block;vertical-align:middle;float:right",actionButton("link", "Article de recherche",onclick ="window.open('https://osf.io/preprints/socarxiv/84bf3/', '_blank')")),
                                                      div(id="API",style="display: inline-block;vertical-align:middle;float:right",actionButton("link", "API",onclick ="window.open('https://regicid.github.io/api', '_blank')")),
                                                      div(style="display: inline-block;vertical-align:bottom;",materialSwitch(inputId = "mode", label = icon("moon"),
                                                                                                                              right=TRUE,status = "default",inline = T,width = "150%")),
                                                      div(style="display: inline-block;vertical-align:middle;float:right;width:100px;margin-top:-20px",selectInput("visualiseur", "",choices = list("Courbes"=1, "Sommes"=2, "Histogramme"=3, "Bulles"=4,"Aires"=5,"Nuage de mots"=7,"Polaires"=8,"ACP"=6,"AFC"=9,"Ctre de gravité"=10,"Rayures"=11),selected = 1))),
                                                      
                                                      plotlyOutput("plot"),
                                                      plotOutput("cloud")
                                                      )),
                                            div(id="outils",column(4,
                                                   sliderInput("span","Lissage de la courbe",min = 0,max = 12,value = 2),
                                                   p(""),
                                                   div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadData', 'Données')),
                                                   div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadPlot', 'Graphique interactif')),
                                                   div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadSPlot', 'Graphique scientifique')),
                                                   p(""))),
                              div(id="leg",column(4,
                                                      fluidRow(uiOutput("legende"),align="right"),
                                                      fluidRow(uiOutput("legende0"),align="right"),
                                                      fluidRow(textOutput("legende1"),align="right"),
                                                      fluidRow(textOutput("legende4"),align="right"),
                                                      fluidRow(textOutput("legende3"),align="right"),
                                                      fluidRow(textOutput("legende2"),align="right"),
                                                      conditionalPanel(condition="input.correlation_test",p("")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr2"),align="right")),
                                                      conditionalPanel(condition="input.correlation_test",fluidRow(textOutput("pvalue"),align="right"))
                                                     )),
                            fluidRow(column(12,
                                    conditionalPanel(condition="input.doc_type==1 || input.doc_type==2 || input.doc_type==56",htmlOutput("lien")),         
                                   #conditionalPanel(condition="input.doc_type==1 || input.doc_type==2 || input.doc_type==56",htmlOutput("frame")),
                                   conditionalPanel(condition="input.doc_type==1 || input.doc_type==2 || input.doc_type==56",dataTableOutput("frame")),
                                   useShinyjs(),
                                   extendShinyjs(text = "const target = document.querySelector('#legende');
                                                 target.scrollIntoView(behavior='smooth');", functions = c()),
                                   conditionalPanel(condition="input.doc_type==1 || input.doc_type==2 || input.doc_type==56",textInput("apikey",label = "OpenAI API key")),
                                   conditionalPanel(condition="input.doc_type==1 || input.doc_type==2 || input.doc_type==56",htmlOutput("gpt")),
                                   h6(textOutput("currentTime"), style="color:white")))
                                              ),
                   tabPanel("Analyse",
                            column(4,wellPanel(
                              textInput("cartoMot","Recherche dans Gallica/presse","Général Boulanger"),
                              div(style = "margin-top: -15px"),
                              dateRangeInput('cartoRange',
                                             label = '\n',
                                             start = as.Date.character("1885-01-01"), end = as.Date.character("1890-01-01"),
                                             separator="à", startview = "decade"),
                              actionButton("topButton","Générer l'analyse"),
                              actionButton("cartoButton","Générer la carte")
                            )),
                            column(8,
                                   #fluidRow(leafletOutput("carto")),
                                   # fluidRow(
                                   #   div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadCarto', 'Carte interactive')),
                                   #   div(style="display: inline-block;vertical-align:bottom",downloadButton('cartoPng', 'Carte .png')),
                                   #   div(style="display: inline-block;vertical-align:bottom",downloadButton('cartogramme', 'Cartogramme .png'))
                                   #   ),
                                   fluidRow(plotlyOutput("top_presse")),
                                   use_busy_spinner(spin = "fingerprint",position="full-page",color="#FF0000",spin_id = "gpresse"),
                                   fluidRow(plotlyOutput("carto2")),
                                   div(style="display: inline-block;vertical-align:bottom",downloadButton('cartogramme', 'Cartogramme .png'))
                                   
                                   )
                            ),
             navbarMenu("Notice",
                        tabPanel("Notice",shiny::includeMarkdown("Notice.md"),dataTableOutput("notice_corp")),
                        tabPanel("Tutoriel",
                                 fluidPage(
                                   h3("Tutoriel et Séminaire de présentation"),
                                   div(style="display: inline-block;vertical-align:bottom",fluidRow(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/SujS4t-ZGhQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                                   div(style="display: inline-block;vertical-align:bottom",fluidRow(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/jMyeFT5Ny3s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                                   p(HTML("<li><a href='http://savoirs.ens.fr/expose.php?id=3989' target='_blank'>Lien vers la vidéo du séminaire DHAI du 11 mai 2021</a>")),
                                   h3("Présentation de Gallicagram"),
                                   fluidRow(uiOutput("pdfview"))
                                 )),
                        tabPanel("Bibliographie",shiny::includeMarkdown("Bibliographie.md")),
                        tabPanel(title=HTML("<li><a href='https://github.com/regicid/pyllicagram' target='_blank'>API Pyllicagram</a>")),
                        tabPanel(title=HTML("<li><a href='https://github.com/nicolrx/gallicagram' target='_blank'>Ruby Gem</a>"))
                        ),
                   navbarMenu("Distributions",
                   tabPanel("Distributions",fluidPage(),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                              selectInput("d_language", "Langue",choices = list("Français" = 1, "Allemand" = 2, "Néerlandais"=3, "Anglais"=4, "Espagnol"=5),selected = 1),
                                              conditionalPanel(condition="input.d_language == 1",radioButtons("d_bibli", "",choices = list("Gallica"=1,"Bibliothèques nationales"=2,"Bibliothèques régionales, locales ou spécialisées"=3,"Presse contemporaine"=4,"Corpus scientifiques"=5),inline = T)),
                                              selectizeInput("distribution","Corpus",choices = list("Presse française / Gallica" = 1, "Livres français / Gallica" = 2,"Livres français / Ngram Viewer" = 5, "Presse allemande / Europeana" = 6, "Presse néerlandaise / Europeana" = 7, "Presse britannique / BNA" = 8, "Livres / Ngram Viewer Allemand" = 9, "Livres / Ngram Viewer Anglais" = 10, "Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12, "Presse québécoise / BAnQ"=28 , "Presse wallonne / KBR"=13, "Presse flamande / KBR"=14, "Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse suisse-allemande / Bibliothèque nationale suisse"=16, "Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27, "Le Monde"=30, "Le Figaro"=31,"Isidore"=36), selected=1),
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
                   )),
                   
             
             tabPanel(value="Gallicapresse",title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/app/gallicapresse' target='_blank'>Gallicapresse</a>")),
             tabPanel(value="Gallicanet",title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/app/gallicanet' target='_blank'>Gallicanet</a>"))
)))
