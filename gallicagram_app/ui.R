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
data = list()

js <- "
function(el, x) {
el.on('plotly_click', function(d) {
var point = d.points[0];
var url = point.data.customdata[point.pointIndex];
window.open(url);
});
}"

Plot <- function(data,input){
  
  if((input$search_mode==2 & input$doc_type==1)|input$search_mode!=2)
  {
  tableau = data[["tableau"]]
  if(input$doc_type==4 & input$occurrences_page==TRUE){
    tableau$ratio_temp<-tableau$ratio_count
    }
  Title = paste("")
  width = length(unique(tableau$date))
  span = 2/width + input$span*(width-2)/(10*width)
  tableau$loess = tableau$nb_temp
  for(mot in str_split(data$mot,"&")[[1]]){
    z = which(tableau$mot==mot)
    x = 1:length(z)
    tableau$loess[z] = loess(tableau$ratio_temp[z]~x,span=span)$fitted
  }
  tableau$hovers = str_c(tableau$date,": x/N = ",tableau$nb_temp,"/",tableau$base_temp,"\n                 = ",round(tableau$ratio_temp*100,digits = 1),"%")
  plot = plot_ly(tableau, x=~date,y=~loess,text=~hovers,color =~mot,type='scatter',mode='spline',hoverinfo="text",customdata=tableau$url)
  #plot = onRender(plot,js)
  y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41,tickformat = ".1%")
  x <- list(title = data[["resolution"]],titlefont = 41)
  plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  if(length(grep(",",data$mot))==0){plot = layout(plot,showlegend=TRUE)}
  
  if(input$delta==TRUE){
    mots<-str_split(input$mot,"&")
    x = 1:sum(tableau$mot==unlist(mots)[1])
    tableau$delta[tableau$mot==unlist(mots)[1]]<-loess((tableau$ratio_temp[tableau$mot==unlist(mots)[1]]-tableau$ratio_temp[tableau$mot==unlist(mots)[2]]~x),span=span)$fitted
    tableau$hovers2 = str_c(tableau$date,": delta = ",round(tableau$delta*100,digits=2),"%, N = ",tableau$base_temp)
    plot = plot_ly(filter(tableau,mot==unlist(mots)[[1]]), x=~date,y=~delta,text=~hovers2,type='scatter',mode='spline',hoverinfo="text")
    y <- list(title = "Différence de fréquence\nd'occurrence dans le corpus",titlefont = 41,tickformat = ".1%")
    x <- list(title = data[["resolution"]],titlefont = 41)
    Title = paste("Freq(",unlist(mots)[1],") – Freq(",unlist(mots)[2],")")
    Title=str_remove_all(Title," ")
    plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  }
  if(input$barplot){
    width = nrow(tableau)
    span = 2/width + input$span*(width-2)/(10*width)
    tableau$hovers = str_c(tableau$date,": N = ",tableau$base_temp)
    plot1 = plot_ly(tableau, x=~date[tableau$mot==mot[1]],y=~base_temp[tableau$mot==mot[1]],text=~hovers[tableau$mot==mot[1]],type='bar',hoverinfo="text",marker = list(color='rgba(31, 119, 180,1)'))
    y <- list(title = "Nombre de numéros dans Gallica-presse",titlefont = 41)
    x <- list(title = data[["resolution"]],titlefont = 41)
    plot1 = layout(plot1, yaxis = y, xaxis = x,title = Title,showlegend = FALSE)
    plot= plot%>%add_lines()
    plot = plotly::subplot(plot,plot1,nrows = 2,legend=NULL,shareX = T)
    return(onRender(plot,js))
  } else{
    plot=layout(plot)
    return(onRender(plot,js))
  }
  }
  if(input$search_mode==2 &input$doc_type==2)
  {
    mots = str_split(input$mot,"&")[[1]]
    tableau=ngram(mots,corpus = "fre_2019",year_start = input$beginning,year_end = input$end,smoothing = 0 ,case_ins = TRUE,aggregate = TRUE)
    colnames(tableau)=c("date","mot","ratio_temp","corpus")
    tableau$mot<-str_remove_all(tableau$mot," \\(All\\)")
    tableau$mot<-str_remove_all(tableau$mot,"\\(")
    tableau$mot<-str_remove_all(tableau$mot,"\\)")
    tableau$mot<-str_replace_all(tableau$mot," \\+ ","\\+")
    Title = paste("")
    width = length(unique(tableau$date))
    span = 2/width + input$span*(width-2)/(10*width)
    tableau$loess = NA
    for(mot in mots){
      z = which(tableau$mot==mot)
      x = 1:length(z)
      tableau$loess[z] = loess(tableau$ratio_temp[z]~x,span=span)$fitted
    }
    tableau$hovers = str_c(tableau$date," : ",round(tableau$ratio_temp*100,digits = 5),"%")
    ngram=plot_ly(tableau,x=~date,y=~loess,color=~mot,text=~hovers,type='scatter',mode='spline',hoverinfo="text")
    y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41,tickformat = ".5%")
    x <- list(title = data[["resolution"]],titlefont = 41)
    ngram = layout(ngram, yaxis = y, xaxis = x,title = Title,showlegend = TRUE)
    return(ngram)
  }
  
}

ui <- navbarPage("Gallicagram",
                 tabPanel("Graphique",fluidPage(),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(headerPanel(''),
                                          sidebarPanel(
                                            textInput("mot","Terme(s) à chercher","Joffre&Pétain&Foch"),
                                            conditionalPanel(condition="input.doc_type != 4",p('Séparer les termes par un "&" pour une recherche multiple')),
                                            conditionalPanel(condition="input.doc_type != 4",p('Utiliser "a+b" pour rechercher a OU b')),
                                            conditionalPanel(condition="input.doc_type == 4",p('Recherche limitée à un seul syntagme')),
                                            radioButtons("doc_type", "Corpus",choices = list("Presse" = 1,"Recherche par titre de presse" = 3, "Livres" = 2, "Corpus personnalisé"=4),selected = 1),
                                            conditionalPanel(condition="input.doc_type == 3",selectizeInput("titres","Titre des journaux",choices = "",selected=NULL,multiple = TRUE)),
                                            conditionalPanel(condition="input.doc_type == 2",fluidRow(column(1,p("")),column(3,radioButtons("search_mode", "Etudier_avec",choices = list("Gallicagram" = 1,"Google_Ngram" = 2),selected = 1)))),
                                            conditionalPanel(condition="input.doc_type == 4",checkboxInput("occurrences_page", "Compter le nombre de pages correspondant à la recherche", value = FALSE)),
                                            conditionalPanel(condition="input.doc_type == 4",fileInput('target_upload','', 
                                                      accept = c(
                                                        'text/csv',
                                                        'text/comma-separated-values',
                                                        '.csv'
                                                      ),buttonLabel='Importer', placeholder='un rapport de recherche')),
                                            div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("beginning","Début",1914)),
                                            div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("end","Fin",1920)),
                                            conditionalPanel(condition="input.doc_type != 2",
                                                              selectInput("resolution", label = "Résolution", choices = c("Année","Mois"))),
                                            conditionalPanel(condition="input.doc_type == 2",
                                                             selectInput("resolution", label = "Résolution", choices = c("Année"))),
                                            actionButton("do","Générer le graphique"),
                                            checkboxInput("barplot", "Afficher la distribution des documents\nde la base Gallica sur la période", value = FALSE),
                                            checkboxInput("correlation_test", "Afficher la matrice de corrélation", value = FALSE),
                                            checkboxInput("delta", "Représenter la différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE)
                                          ),
                                          
                                          mainPanel(plotlyOutput("plot"),
                                                    fluidRow(textOutput("legende"),align="right"),
                                                    conditionalPanel(condition="(input.doc_type == 2 && input.search_mode==1) || input.doc_type != 2",fluidRow(textOutput("legende0"),align="right")),
                                                    fluidRow(textOutput("legende1"),align="right"),
                                                    conditionalPanel(condition="(input.doc_type == 2 && input.search_mode==1) || input.doc_type != 2",fluidRow(textOutput("legende2"),align="right")),
                                                    conditionalPanel(condition="(input.doc_type == 2 && input.search_mode==1) || input.doc_type != 2",fluidRow(textOutput("legende3"),align="right")),
                                                    conditionalPanel(condition="input.correlation_test",p("")),
                                                    conditionalPanel(condition="input.correlation_test",fluidRow(tableOutput("corr"),align="right")),
                                                    conditionalPanel(condition="input.correlation_test",fluidRow(textOutput("pvalue"),align="right")),
                                                    div(style="display: inline-block;vertical-align:bottom",sliderInput("span","Lissage de la courbe",min = 0,max = 10,value = 0)),
                                                    div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadData', 'Télécharger les données')),
                                                    div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadPlot', 'Télécharger le graphique interactif')),
                                                    p(""),
                                                    h2(textOutput("currentTime"), style="color:white")
                                                ))),
                 tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                 tabPanel("Corpus de presse",fluidPage(),
                          pageWithSidebar(headerPanel(''),
                                          sidebarPanel(
                                            radioButtons("corpus_structure_p", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Droits d'auteur"=3,"Bibliothèque d'origine"=4, "Classement thématique de Dewey" = 5,"Périodicité" = 6,"Titre de presse" = 7),selected = 1),
                                            conditionalPanel(condition="input.corpus_structure_p!=1",checkboxInput("corpus_relative_p", "Afficher les résultats en valeurs relatives", value = FALSE))
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
                                            radioButtons("corpus_structure_l", "Données à analyser :",choices = list("Distribution"=1,"Ville de publication" = 2,"Droits d'auteur" = 3, "Bibliothèque d'origine" = 4,"Volume (nombre de pages moyen)" = 5,"Volume (nombre de pages médian)" = 6,"Etat de la numérisation"=7,"Qualité d'océrisation"=8,"Date de numérisation"=9),selected = 1),
                                            conditionalPanel(condition="(input.corpus_structure_l==2 || input.corpus_structure_l==3 || input.corpus_structure_l==4 || input.corpus_structure_l==7)",
                                                             checkboxInput("corpus_relative_l", "Afficher les résultats en valeurs relatives", value = FALSE)
                                                             )
                                          ),
                                          mainPanel(
                                            conditionalPanel(condition="input.corpus_structure_l!=8",fluidRow(plotlyOutput("corpus2")),
                                                             p("")),
                                            conditionalPanel(condition="input.corpus_structure_l==8",img(src = "nqamoyen.png", height = 589, width = 681)),
                                            conditionalPanel(condition="input.corpus_structure_l==9",img(src = "numerisation.png", height = 589, width = 681)),
                                            p("")
                                          )
                          )
                 ),
                 tabPanel("Tutoriel",headerPanel("Tutoriel"),
                          fluidPage(HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/SujS4t-ZGhQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                 tabPanel(title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/gallicapresse' target='_blank'>Gallicapresse"))
)



