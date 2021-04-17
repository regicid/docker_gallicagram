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

  if(input$doc_type!=4){tableau = data[["tableau"]]}
  if(input$doc_type==4 & input$occurrences_page==TRUE){
      tableau = data[["tableau_page"]]
  }
  if(input$doc_type==4 & input$occurrences_page==FALSE){
    tableau = data[["tableau_volume"]]
  }
    Title = paste("")
    width = length(unique(tableau$date))
    span = 2/width + input$span*(width-2)/(10*width)
    tableau$loess = tableau$ratio
    if(input$span >0){
      for(mot in str_split(data$mot,"&")[[1]]){
        z = which(tableau$mot==mot)
        x = 1:length(z)
        tableau$loess[z] = loess(tableau$ratio[z]~x,span=span)$fitted
      }}
    tableau$hovers = str_c(tableau$date,": x/N = ",tableau$count,"/",tableau$base,"\n                 = ",round(tableau$ratio*100,digits = 1),"%")
    y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41,tickformat = ".1%")
    x <- list(title = data[["resolution"]],titlefont = 41)
    if(input$search_mode==2 &input$doc_type==2){
      tableau$hovers = str_c(tableau$date," : ",round(tableau$ratio*100,digits = 5),"%")
      y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41,tickformat = ".5%")
      }
    plot = plot_ly(tableau, x=~date,y=~loess,text=~hovers,color =~mot,type='scatter',mode='spline',hoverinfo="text",customdata=tableau$url)
    plot = layout(plot, yaxis = y, xaxis = x,title = Title)
    if(length(grep(",",data$mot))==0){plot = layout(plot,showlegend=TRUE)}
    
    if(input$delta==TRUE){
      mots<-str_split(input$mot,"&")
      x = 1:sum(tableau$mot==unlist(mots)[1])
      tableau$delta[tableau$mot==unlist(mots)[1]]<-loess((tableau$ratio[tableau$mot==unlist(mots)[1]]-tableau$ratio[tableau$mot==unlist(mots)[2]]~x),span=span)$fitted
      tableau$hovers2 = str_c(tableau$date,": delta = ",round(tableau$delta*100,digits=2),"%, N = ",tableau$base)
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
      tableau$hovers = str_c(tableau$date,": N = ",tableau$base)
      plot1 = plot_ly(tableau, x=~date[tableau$mot==mot[1]],y=~base[tableau$mot==mot[1]],text=~hovers[tableau$mot==mot[1]],type='bar',hoverinfo="text",marker = list(color='rgba(31, 119, 180,1)'))
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
get_corpus_perso <- function(mot,from,to,resolution,tot_df){
  mot=str_remove(mot,"&.+")
  mot=str_remove(mot,"[+].+")
  mot=str_replace_all(mot,"[:punct:]"," ")
  mot_init<-mot
  mot=str_replace_all(mot,"[:space:]","%20")
  
  if(resolution=="Année"){
    tot_df$date<-str_remove_all(tot_df$date,"-.+")
  }
  if (resolution=="Mois"){
    from=str_c(from,"-01")
    to=str_c(to,"-12")
    tot_df$date<-str_extract(tot_df$date,".......")
  }
  tot_df<-tot_df[is.na(tot_df$date)==FALSE,]
  tot_df$date<-as.character(tot_df$date)
  tot_df<-tot_df[tot_df$date>=from & tot_df$date<=to,]
  tableau<-tot_df%>%count(date)
  tot_df$ark=str_extract_all(tot_df$identifier,"12148/.+")
  tot_df$ark=str_remove_all(tot_df$ark,"12148/")
  tot_df$resultats<-0
  tot_df$resultats_base<-0
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  
  tot_df$detect<-FALSE
  for (i in 1:length(tot_df$ark)) {
    url<-str_c("https://gallica.bnf.fr/services/ContentSearch?ark=",tot_df$ark[i],"&query=",mot)
    resultat<-as.character(read_html(RETRY("GET",url,times = 3)))
    resultat=str_remove_all(resultat,"[:space:]")
    resultat=str_remove_all(resultat,".+countresults")
    resultat=str_remove_all(resultat,"searchtime.+")
    resultat=str_extract(resultat,"[:digit:]+")
    tot_df$resultats[i]<-as.integer(resultat)
    
    url_base<-str_c("https://gallica.bnf.fr/services/ContentSearch?ark=",tot_df$ark[i],"&query=%20")
    resultat_base<-as.character(read_html(RETRY("GET",url_base,times = 3)))
    resultat_base=str_remove_all(resultat_base,"[:space:]")
    resultat_base=str_remove_all(resultat_base,".+countresults")
    resultat_base=str_remove_all(resultat_base,"searchtime.+")
    resultat_base=str_extract(resultat_base,"[:digit:]+")
    tot_df$resultats_base[i]<-as.integer(resultat_base)
    if(as.integer(resultat)>0){tot_df$detect[i]<-TRUE}
    progress$inc(1/length(tot_df$ark), detail = paste(as.integer(i*100/length(tot_df$ark)),"% traités"))
  }
  tableau$count<-0
  tableau$detect<-0
  tableau$count_base<-0
  for (j in 1:length(tableau$date)) {
    tableau$count[j]<-sum(tot_df$resultats[tot_df$date==tableau$date[j]])
    tableau$count_base[j]<-sum(tot_df$resultats_base[tot_df$date==tableau$date[j]])
    tableau$detect[j]<-sum(tot_df$detect[tot_df$date==tableau$date[j]])
  }
  tableau$mot<-mot_init
  tableau$url<-"https://gallica.bnf.fr/"
  colnames(tableau)<-c("date","base","page_count","count","page_base","mot","url")
  tableau$ratio<-tableau$count/tableau$base
  tableau$ratio_page<-tableau$page_count/tableau$page_base
  tableau$ratio[is.na(tableau$ratio)]<-0
  tableau$ratio_page[is.na(tableau$ratio_page)]<-0
  tableau$date<-str_replace_all(tableau$date,"-","/")
  
  tableau_page<-select(tableau,date,page_count,page_base,mot,url,ratio_page)
  tableau_page$resolution<-resolution
  tableau_page$corpus<-"corpus_perso"
  tableau_page$search_mode<-"page"
  colnames(tableau_page)<-c("date",	"count",	"base",	"mot",	"url",	"ratio",	"resolution",	"corpus",	"search_mode")
  
  tableau_volume<-select(tableau,date,count,base,mot,url,ratio)
  tableau_volume$resolution<-resolution
  tableau_volume$corpus<-"corpus_perso"
  tableau_volume$search_mode<-"volume"
  colnames(tableau_volume)<-c("date",	"count",	"base",	"mot",	"url",	"ratio",	"resolution",	"corpus",	"search_mode")

  memoire<<-bind_rows(tableau_volume,memoire)
  memoire<<-bind_rows(tableau_page,memoire)
  
  data = list(tableau_volume,tableau_page,paste(mot),resolution)
  names(data) = c("tableau_volume","tableau_page","mot","resolution")
  return(data)
}

get_data <- function(mot,from,to,resolution,doc_type,titres,search_mode){
  mots = str_split(mot,"&")[[1]]
  tableau<-as.data.frame(matrix(nrow=0,ncol=5),stringsAsFactors = FALSE)
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  if(doc_type==1 & resolution=="Année"){
    base=read.csv("base_presse_annees.csv")
  } else  if(doc_type==1 & resolution=="Mois"){
    base=read.csv("base_presse_mois.csv")
  } else if(doc_type==2 & search_mode==1){
    base=read.csv("base_livres_annees.csv")
  }
  
  
  for (i in from:to){
    for(mot in mots){
      mot2 = str_replace_all(mot," ","%20")
      ###
      or<-""
      or_end<-""
      if(str_detect(mot2,"[+]")){
        mots_or = str_split(mot2,"[+]")[[1]]
        or1<-NA
        or1_end<-NA
        for (j in 2:length(mots_or)) {
          
          or1[j]<-str_c("or%20text%20adj%20%22",mots_or[j],"%22%20")
          or1_end[j]<-str_c("%20",mots_or[j])
          or<-str_c(or,or1[j])
          or_end<-str_c(or_end,or1_end[j])
        }
        mot1<-mots_or[1]}else{mot1=mot2}
      
      if(doc_type==3 & length(titres)>1){
        ark1<-titres[1]
        ark3<-""
        for (v in 2:length(titres)) 
        {
          ark<-titres[v]
          ark2<-str_c("%20or%20dc.relation%20any%20%22",ark,"%22")
          ark3<-str_c(ark3,ark2)
        }
      }else
      {
        ark1<-titres
        ark3<-""
      }
      ###
      end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
      if(i%%4==0){end_of_month[2]=29} #Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
      y<-as.character(i)
      if(resolution=="Année" | (doc_type==2 & search_mode==1)){beginning = str_c(y,"/01/01")
      end = str_c(y,"/12/31")}
      I = 1
      if(resolution=="Mois"){I=1:12} #Pour faire ensuite une boucle sur les mois
      
      
      if(doc_type !=2){
        for(j in I){
          if(resolution=="Mois"){
            z = as.character(j)
            if(nchar(z)<2){z<-str_c("0",z)}
            beginning = str_c(y,"/",z,"/01")
            end = str_c(y,"/",z,"/",end_of_month[j])}
          url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
          if(doc_type == 3){
            url <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)sortby%20dc.date%20")
          }
          ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
          a<-str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
          
          if(doc_type == 3){
            url_base <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)%20sortby%20dc.date")
            ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
            b<-str_extract(str_extract(ngram_base,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
          }
          if(resolution=="Mois"& doc_type==1){
            date=str_c(y,"/",z)
            b<-as.integer(base$base[base$date==date])}
          else if (resolution=="Année" & doc_type==1){b<-as.integer(base$base[base$date==y])}
          if(length(b)==0L){b=0}
          tableau[nrow(tableau)+1,] = NA
          date=y
          if(resolution=="Mois"){date = paste(y,z,sep="/")}
          tableau[nrow(tableau),]<-c(date,a,b,mot,url)
          progress$inc(1/((to-from+1)*length(I)*length(mots)), detail = paste("Gallicagram ratisse l'an", i))
        }}
      
      if(doc_type==2 & search_mode==1){
        url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)&suggest=10&keywords=",mot1,or_end)
        ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
        a<-str_extract(str_extract(ngram,"numberOfRecords>[:digit:]+"),"[:digit:]+")
        b<-as.integer(base$base[base$date==y])
        if(length(b)==0L){b=0}
        tableau[nrow(tableau)+1,] = NA
        date=y
        tableau[nrow(tableau),]<-c(date,a,b,mot,url)
        progress$inc(1/((to-from+1)*length(I)*length(mots)), detail = paste("Gallicagram ratisse l'an", i))
        
      }
      
    }
  }
  colnames(tableau)<-c("date","count","base","mot","url")
  tableau$url = str_replace(tableau$url,"SRU","services/engine/search/sru")
  tableau$url = str_replace(tableau$url,"maximumRecords=1","maximumRecords=25")
  
  
  
  tableau$count<-as.integer(tableau$count)
  tableau$base<-as.integer(tableau$base)
  tableau$ratio<-tableau$count/tableau$base
  tableau$ratio[is.na(tableau$ratio)]<-0
  #ngram_viewer
  if(doc_type==2 & search_mode==2){
  tableau=ngrami(mots,corpus = "fre_2019",year_start = from, year_end = to, smoothing = 0, aggregate = TRUE)
  tableau$search_mode<-"match"
  colnames(tableau)=c("date","mot","ratio","corpus","search_mode")
  base_ngram<-read.csv("ngram_viewer_fre_20200217.csv",encoding = "UTF-8")
  base_ngram<-select(base_ngram,year,match_count)
  colnames(base_ngram)<-c("date","base")
  tableau<-left_join(tableau,base_ngram,by="date")
  tableau$date<-as.character(tableau$date)
  tableau$count<-as.integer(tableau$base*tableau$ratio)
  tableau$url<-""
  for (i in 1:length(tableau$date)) {
      tableau$url[i]=str_c("https://www.google.fr/search?lr=lang_fr&hl=fr&tbo=p&tbm=bks&q=",tableau$mot[i],"&tbs=,bkt:b,cdr:1,cd_min:1+janv.+",tableau$date[i],",cd_max:31+d%C3%A9c.+",tableau$date[i],"&num=20")
  }
  }
  tableau$resolution<-resolution
  format = "%Y"
  if(resolution=="Mois"){format=paste(format,"%m",sep="/")}
  tableau.date = as.Date(as.character(tableau$date),format=format)
  if(doc_type==1){tableau$corpus="presse_gallica"
  tableau$search_mode<-"volume"}
  if(doc_type==2 & search_mode==1){tableau$corpus="livres_gallica"
  tableau$search_mode<-"volume"}
  if(doc_type==2 & search_mode==2){tableau$corpus="livres_ngram"}
  if(doc_type==3){tableau$corpus="titre_presse_gallica"
  tableau$search_mode<-"volume"}
  if(doc_type==4){tableau$corpus="perso_gallica"
  tableau$search_mode<-"volume"}
  memoire<<-bind_rows(tableau,memoire)
  data = list(tableau,paste(mots,collapse="&"),resolution)
  names(data) = c("tableau","mot","resolution")
  return(data)}

data=list(read.csv("exemple.csv",encoding = "UTF-8"),"Joffre&Pétain&Foch","Années")
names(data)=c("tableau","mot","resolution")

#########
prepare_correlation<-function(df){
  df=df[["tableau"]]
  df=select(df,mot,ratio)
  mots<-unlist(unique(df$mot))
  a<-df$ratio[df$mot==mots[1]]
  for (i in 2:length(mots)) {
    a<-cbind(a,df$ratio[df$mot==mots[i]])
  }
  df=as.data.frame(a)
  colnames(df)=mots
  return(df)
}

#########
prepare_memoire<-function(from,to,resolution){
  df<-memoire
  df<-distinct(df)
  if(resolution=="Mois"){
    from=str_c(from,"/01")
    to=str_c(to,"/12")
    df<-df[str_length(df$date)==7,]
  }
  else{df<-df[str_length(df$date)==4,]}
  df<-df[df$date<=to,]
  df<-df[df$date>=from,]
  mots<-unlist(unique(df$mot))
  df_liste<-list(df[df$mot==mots[1],])
  for (i in 2:length(mots)) {
    a<-list(df[df$mot==mots[i],])
    df_liste<-c(df_liste,a)
  }
  
  for (i in 1:length(df_liste)) {
    df<-as.data.frame(df_liste[i])
    df$mot<-str_c(df$mot,"_",df$search_mode,"_",df$corpus,"_",df$resolution)
    df<-select(df,date,ratio,mot)
    mots<-unlist(unique(df$mot))
    a<-df$ratio[df$mot==mots[1]]
    if (length(mots)>=2){
      for (j in 2:length(mots)) {
        a<-cbind(a,df$ratio[df$mot==mots[j]])
    }}
    df=as.data.frame(a)
    colnames(df)=mots
    df_liste[i]<-list(df)
  }
  return(df_liste)
}
#########
correlation_matrix <- function(df, corr,
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "upper", 
                               show_significance = TRUE, 
                               replace_diagonal = TRUE, 
                               replacement = ""){
  
  mots = colnames(df)
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  if(corr=="corr1" & length(mots)>=3)
  {Rnew<-Rnew[-length(mots),]
  Rnew<-Rnew[,-1]}
  if(corr=="corr2" & nrow(Rnew)>=3)
  {Rnew<-Rnew[-nrow(Rnew),]
  Rnew<-Rnew[,-1]}
  return(Rnew)
}
options(shiny.maxRequestSize = 100*1024^2)

shinyServer(function(input, output,session){
  
  memoire<<-read.csv("exemple.csv",encoding="UTF-8")
  memoire$date<<-as.character(memoire$date)
  recherche_precedente<<-"Joffre&Pétain&Foch_1914_1920_Année"
  
  output$instructions <- renderUI(HTML('<ul><li>Séparer les termes par un "&" pour une recherche multiple</li><li>Utiliser "a+b" pour rechercher a OU b</li><li>Cliquer sur un point du graphique pour accéder aux documents dans Gallica</li></ul>'))
  
  indicator_file <- reactive({
    if(is.null(input$target_upload)) {return(0)}
    else{return(1)}
  })
  output$fileUploaded <- reactive({
    return(indicator_file())
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$legende1<-renderText(str_c("Corpus : presse\n"))
  observeEvent(
    input$doc_type,
    {if(input$doc_type==3 && is.null(input$titres))
    {
      liste_journaux<-read.csv("liste_journaux.csv",encoding="UTF-8")
      updateSelectizeInput(session,"titres",choices = setNames(liste_journaux$ark,liste_journaux$title),selected="cb39294634r")
    }
      
    })
  observeEvent(
    input$do,
    {if(input$doc_type==3)
    {
      liste_journaux<-read.csv("liste_journaux.csv",encoding="UTF-8")
      titres<-reactive({liste_journaux$title[liste_journaux$ark==input$titres]})
      output$legende1<-renderText(paste(titres()))
    }
      
      if(input$doc_type!=3){output$legende1<-renderText(str_c(if(input$doc_type==1){"Corpus : presse\n"} else if (input$doc_type==2){"Corpus : livres\n"}))}
    })
  
  output$plot <- renderPlotly({Plot(data,input)})
  output$corr<-renderTable(correlation_matrix(prepare_correlation(data),"corr1"),rownames = TRUE)
  output$pvalue=renderText("***p<.001 ; **p<.01 ; *p<.05")
  observeEvent(input$search_mode,{observeEvent(input$doc_type,{
    if(input$search_mode==2 & input$doc_type==2)
    {output$legende=renderText("Source : books.google.com/ngrams")}
    else{output$legende=renderText("Source : gallica.bnf.fr")}
  })})
  output$legende0=renderText("Affichage : Gallicagram par Benjamin Azoulay et Benoît de Courson")
  observeEvent(
    input$occurrences_page,{
      output$legende2<-renderText(if(input$doc_type!=4 | input$occurrences_page!=TRUE){str_c(as.character(sum(data[["tableau"]]$base))," documents épluchés\n")}else if(input$doc_type==4 & input$occurrences_page==TRUE){str_c(as.character(sum(data[["tableau"]]$base_count))," pages épluchées\n")})
      output$legende3<-renderText(if(input$doc_type!=4 | input$occurrences_page!=TRUE){str_c(as.character(sum(data[["tableau"]]$count))," résultats trouvés")}else if(input$doc_type==4 & input$occurrences_page==TRUE){str_c(as.character(sum(data[["tableau"]]$count))," pages correspondant à la recherche")})
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_",input$mot,"_",input$beginning,"_",input$end,'.csv', sep='')
    },
    content = function(con) {
      write.csv(data$tableau, con,row.names = F,fileEncoding = "UTF-8")
    })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('plot_',input$mot,"_",input$beginning,"_",input$end,'.html', sep='')
    },
    content = function(con) {
      htmlwidgets::saveWidget(as_widget(Plot(data,input)), con)
    })
  output$data_session <- downloadHandler(
    filename = function() {
      paste('data-session_', Sys.Date(),'.csv', sep='')
    },
    content = function(con) {
      write.csv(memoire, con,row.names = F,fileEncoding = "UTF-8")
    })
  
  observeEvent(input$do,{
    # datasetInput <- reactive({
    #   data$tableau})
    if (input$doc_type!=4){
      df = get_data(input$mot,input$beginning,input$end,input$resolution,input$doc_type,input$titres,input$search_mode)}
    else if(input$doc_type==4){
      inFile<-input$target_upload
      tot_df<- read.csv(inFile$datapath, header = TRUE,sep = ";",encoding = "UTF-8")
      colnames(tot_df)<-str_remove_all(colnames(tot_df),"'")
      colnames(tot_df)<-str_remove_all(colnames(tot_df),"\\.")
      colnames(tot_df)<-str_remove_all(colnames(tot_df)," ")
      colnames(tot_df)<-str_remove_all(colnames(tot_df),"XUFEFF")
      colnames(tot_df)<-iconv(colnames(tot_df),from="UTF-8",to="ASCII//TRANSLIT")
      tot_df<-select(tot_df,URLdaccesaudocument,Typededocument,Titre,Auteurs,Contributeur,Editeurs,Datededition,Description,NombredeVues,Provenance,Droits,ArkCatalogue)
      colnames(tot_df)<-c("identifier","type","title","creator","contributor","publisher","date","description","format","source","rights","relation")
      tot_df$identifier<-str_remove_all(tot_df$identifier," .+")
      df=get_corpus_perso(input$mot,input$beginning,input$end,input$resolution,tot_df)
    }
    
    output$plot <- renderPlotly({Plot(df,input)})
    
    output$legende2<-renderText(str_c(as.character(sum(df[["tableau"]]$base))," numéros épluchés\n"))
    output$legende3<-renderText(str_c(as.character(sum(df[["tableau"]]$count))," résultats trouvés"))
    if(str_detect(input$mot,".+&.+"))
    {output$corr<-renderTable(correlation_matrix(prepare_correlation(df),"corr1"),rownames = TRUE)}
    else{output$corr<-renderTable(as.matrix(NA),colnames = FALSE)}
    
    if(recherche_precedente==str_c(input$mot,"_",input$beginning,"_",input$end,"_",input$resolution)){
    matrice2<-prepare_memoire(input$beginning,input$end,input$resolution)
    j=length(matrice2)
    for (i in j:1) {
      if(length(matrice2[[i]])<=1){
        matrice2<-matrice2[-i]
      }
    }
    b<-correlation_matrix(as.data.frame(matrice2[1]),"corr2")
    b<-rbind(colnames(b),b)
    b<-cbind(rownames(b),b)
    colnames(b)<-NULL
    rownames(b)<-NULL
    if(length(matrice2)>1){
    for (j in 2:length(matrice2)) {
      a<-correlation_matrix(as.data.frame(matrice2[[j]]),"corr2")
      a<-rbind(colnames(a),a)
      a<-cbind(rownames(a),a)
      colnames(a)<-NULL
      rownames(a)<-NULL
      b<-bind_rows(as.data.frame(b),as.data.frame(a))
    }}
    output$corr2<-renderTable(b,colnames = FALSE)
    }
    else{output$corr2<-renderTable(as.matrix(NA),colnames = FALSE)}
    recherche_precedente<<-str_c(input$mot,"_",input$beginning,"_",input$end,"_",input$resolution)
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data_",input$mot,"_",input$beginning,"_",input$end,'.csv', sep='')
      },
      content = function(con) {
        write.csv(df$tableau, con,row.names = F,fileEncoding = "UTF-8")
      })
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste('plot_',input$mot,"_",input$beginning,"_",input$end,'.html', sep='')
      },
      content = function(con) {
        htmlwidgets::saveWidget(as_widget(Plot(df,input)), con)
      })
  })
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  corpus_display_p<-function() {
    
    if(input$corpus_relative_p==FALSE){
      if(input$corpus_structure_p==1){
        table<-read.csv("base_presse_annees.csv",encoding="UTF-8")
        somme<-sum(table$base)
        table$hovers = str_c(table$date,": N = ",table$base)
        plot2<-plot_ly(table, x=~date,y=~base,text=~hovers,type='bar',hoverinfo="text")
        Title = paste("<a href = 'https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&maximumRecords=50&page=1&exactSearch=true&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(gallicapublication_date%3E=%221631/01/01%22%20and%20gallicapublication_date%3C=%222021/12/31%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords='> <b>Répartition des ",somme," numéros de presse océrisés dans Gallica<b> </a>")
        y <- list(title = "Nombre de numéros dans Gallica-presse",titlefont = 41)
        x <- list(title = "Date",titlefont = 41)
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_p==2){
        p_villes<-read.csv("p_villes.csv",encoding = "UTF-8")
        plot7<-plot_ly(p_villes,x=~as.integer(p_villes$date),y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot7<-layout(plot7, title="Distribution des numéros de presse en français \nselon la ville d'édition", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot7)
      }
      else if(input$corpus_structure_p==5){
        p_themes<-read.csv("p_themes.csv",encoding = "UTF-8")
        plot11<-plot_ly(p_themes,x=~as.integer(p_themes$date),y=~n,color=~principaux_themes,type='bar',colors="Dark2")
        plot11<-layout(plot11, title="Distribution des numéros de presse en français \nselon le thème du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot11)
      }
      else if(input$corpus_structure_p==6){
        periodicite<-read.csv("periodicite.csv",encoding = "UTF-8")
        plot16<-plot_ly(periodicite,x=~as.integer(periodicite$date),y=~n,color=~is_quotidien,type='bar',colors="Dark2")
        plot16<-layout(plot16, title="Distribution des numéros de presse en français \nselon la périodicité du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot16)
      }
      else if(input$corpus_structure_p==3){
        p_droits<-read.csv("p_droits.csv",encoding = "UTF-8")
        plot5<-plot_ly(p_droits,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, title="Distribution des numéros de presse en français \nselon leur mode d'accès", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot5)
      }
      else if(input$corpus_structure_p==4){
        p_sources<-read.csv("p_sources.csv",encoding = "UTF-8")
        p_sources$principales_sources<-str_remove_all(p_sources$principales_sources,"[/].+")
        p_sources$principales_sources<-str_remove_all(p_sources$principales_sources,"[-].+")
        p_sources$principales_sources<-str_remove_all(p_sources$principales_sources,"[()].+")
        plot4<-plot_ly(p_sources,x=~date,y=~n,color=~principales_sources,type='bar',colors="Dark2")
        plot4<-layout(plot4, title="Distribution des numéros de presse en français \nselon leur bibliothèque de numérisation d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot4)
      }
    }
    if(input$corpus_relative_p==TRUE){
      if(input$corpus_structure_p==1){
        table<-read.csv("base_presse_annees.csv",encoding="UTF-8")
        somme<-sum(table$base)
        table$hovers = str_c(table$date,": N = ",table$base)
        plot2<-plot_ly(table, x=~date,y=~base,text=~hovers,type='bar',hoverinfo="text")
        Title = paste("<a href = 'https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&maximumRecords=50&page=1&exactSearch=true&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(gallicapublication_date%3E=%221631/01/01%22%20and%20gallicapublication_date%3C=%222021/12/31%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords='> <b>Répartition des ",somme," numéros de presse océrisés dans Gallica<b> </a>")
        y <- list(title = "Nombre de numéros dans Gallica-presse",titlefont = 41)
        x <- list(title = "Date",titlefont = 41)
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_p==2){
        p_villes<-read.csv("p_villes.csv",encoding = "UTF-8")
        plot8<-plot_ly(p_villes,x=~as.integer(p_villes$date),y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot8<-layout(plot8, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon la ville d'édition", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot8)
      }
      else if(input$corpus_structure_p==5){
        p_themes<-read.csv("p_themes.csv",encoding = "UTF-8")
        plot12<-plot_ly(p_themes,x=~as.integer(p_themes$date),y=~n,color=~principaux_themes,type='bar',colors="Dark2")
        plot12<-layout(plot12, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon le thème du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot12)
      }
      else if(input$corpus_structure_p==6){
        periodicite<-read.csv("periodicite.csv",encoding = "UTF-8")
        plot17<-plot_ly(periodicite,x=~as.integer(periodicite$date),y=~n,color=~is_quotidien,type='bar',colors="Dark2")
        plot17<-layout(plot17, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon la périodicité du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",barnorm="percent",bargap=0)
        return(plot17)
      }
      else if(input$corpus_structure_p==3){
        p_droits<-read.csv("p_droits.csv",encoding = "UTF-8")
        plot5<-plot_ly(p_droits,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon leur mode d'accès", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",barnorm="percent",bargap=0)
        return(plot5)
      }
      else if(input$corpus_structure_p==4){
        p_sources<-read.csv("p_sources.csv",encoding = "UTF-8")
        p_sources$principales_sources<-str_remove_all(p_sources$principales_sources,"[/].+")
        p_sources$principales_sources<-str_remove_all(p_sources$principales_sources,"[-].+")
        p_sources$principales_sources<-str_remove_all(p_sources$principales_sources,"[()].+")
        plot4<-plot_ly(p_sources,x=~date,y=~n,color=~principales_sources,type='bar',colors="Dark2")
        plot4<-layout(plot4, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon leur bibliothèque de numérisation d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",barnorm="percent",bargap=0)
        return(plot4)
      }
    }
    
  }
  observeEvent(input$corpus_structure_p,{observeEvent(input$corpus_relative_p,{
    output$corpus1<-renderPlotly({corpus_display_p()})
  })})
  
  
  corpus_display_l<-function() {
    
    if(input$corpus_relative_l==FALSE){
      if(input$corpus_structure_l==1 & input$corpus_ngram_l==FALSE){
        table<-read.csv("base_livres_annees.csv",encoding="UTF-8")
        somme<-sum(table$base)
        table<-table[table$date>=1450,]
        table$hovers = str_c(table$date,": N = ",table$base)
        plot2<-plot_ly(table, x=~date,y=~base,text=~hovers,type='bar',hoverinfo="text")
        Title = paste("<a href = 'https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22)%20and%20(gallicapublication_date%3E=%221380%22%20and%20gallicapublication_date%3C=%222021%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords='> <b>Répartition des ",somme," livres en français océrisés\ndans Gallica<b> </a>")
        y <- list(title = "Nombre de livres dans Gallica",titlefont = 41)
        x <- list(title = "Date",titlefont = 41)
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_l==1 & input$corpus_ngram_l==TRUE){
        ngram<-read.csv("ngram_viewer_fre_20200217.csv",encoding = "UTF-8")
        total_volume_count<-sum(ngram$volume_count)
        plot2<-plot_ly(ngram, x=~year,y=~volume_count,type='bar')
        Title = paste("<b>Répartition des ",total_volume_count," livres océrisés et en français\nexploités dans"," <a href = 'https://books.google.com/ngrams/graph?content=Joffre%2CP%C3%A9tain%2CFoch&year_start=1914&year_end=1920&corpus=30&smoothing=0&direct_url=t1%3B%2CJoffre%3B%2Cc0%3B.t1%3B%2CP%C3%A9tain%3B%2Cc0%3B.t1%3B%2CFoch%3B%2Cc0'>Google Ngram Viewer</a><b>")
        y <- list(title = "Nombre de livres exploités dans Ngram Viewer",titlefont = 41)
        x <- list(title = "Date",titlefont = 41)
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_l==2){
        p_villes_livres<-read.csv("p_villes_livres.csv",encoding = "UTF-8")
        plot3<-plot_ly(p_villes_livres,x=~date,y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot3<-layout(plot3, title="Distribution des livres en français \nselon leur ville de publication", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot3)
      }
      else if(input$corpus_structure_l==3){
        p_droits_livres<-read.csv("p_droits_livres.csv",encoding = "UTF-8")
        plot5<-plot_ly(p_droits_livres,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, title="Distribution des livres en français \nselon leur régime juridique", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot5)
      }
      else if(input$corpus_structure_l==4){
        p_sources_livres<-read.csv("p_sources_livres.csv",encoding = "UTF-8")
        p_sources_livres$principales_sources<-str_remove_all(p_sources_livres$principales_sources,"[/].+")
        p_sources_livres$principales_sources<-str_remove_all(p_sources_livres$principales_sources,"[-].+")
        p_sources_livres$principales_sources<-str_remove_all(p_sources_livres$principales_sources,"[()].+")
        plot4<-plot_ly(p_sources_livres,x=~date,y=~n,color=~principales_sources,type='bar',colors="Dark2")
        plot4<-layout(plot4, title="Distribution des livres en français \nselon leur bibliothèque de numérisation d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot4)
      }
      else if(input$corpus_structure_l==5){
        p_pages_livres<-read.csv("p_pages_livres.csv",encoding = "UTF-8")
        plot7<-plot_ly(p_pages_livres,x=~date,y=~Mean,type='bar',colors="Dark2")
        plot7<-layout(plot7, title="Distribution des livres en français \nselon leur volume (nombre de pages moyen)", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de pages"),barmode="stack",bargap=0)
        return(plot7)
      }
      else if(input$corpus_structure_l==6){
        p_pages_livres<-read.csv("p_pages_livres.csv",encoding = "UTF-8")
        plot6<-plot_ly(p_pages_livres,x=~date,y=~Median,type='bar',colors="Dark2")
        plot6<-layout(plot6, title="Distribution des livres en français \nselon leur volume (nombre de pages médian)", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de pages"),barmode="stack",bargap=0)
        return(plot6)
      }
      else if(input$corpus_structure_l==7){
        tableau<-read.csv("base_livres_annees_bnf.csv",encoding = "UTF-8")
        tableau1<-read.csv("base_livres_annees.csv",encoding = "UTF-8")
        tableau3<-read.csv("base_livres_annees_numerises.csv",encoding = "UTF-8")
        for (i in 1:1379) {
          a<-as.data.frame(cbind(i,0))
          colnames(a)<-c("date","base")
          tableau1<-bind_rows(tableau1,a)
          tableau1<-tableau1[order(tableau1$date),]
          rownames(tableau1)<-NULL
          tableau3<-bind_rows(tableau3,a)
          tableau3<-tableau3[order(tableau3$date),]
          rownames(tableau3)<-NULL
        }
        tableau3$base<-tableau3$base-tableau1$base
        tableau$base<-tableau$base-tableau1$base-tableau3$base
        tableau$corpus<-"Non numérisé"
        tableau1$corpus<-"Numérisé et océrisé"
        tableau3$corpus<-"Numérisé mais pas océrisé"
        tableau2<-bind_rows(tableau,tableau1,tableau3)
        
        plot18<-plot_ly(tableau2,x=~as.integer(tableau2$date),y=~base,color=~corpus,type='bar',colors="Dark2")
        plot18<-layout(plot18, title="Distribution des livres en français \nselon leur état de numérisation", xaxis=list(title="Date",tickangle="-45",range=c("1380","2021")),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        
        return(plot18)
      }
      else if(input$corpus_structure_l==9){
        first_indexation_date<-read.csv("first_indexation_date.csv",encoding = "UTF-8")
        plot4<-plot_ly(first_indexation_date,x=~first_indexation_date,y=~count,type='bar',colors="Dark2")
        plot4<-layout(plot4, title="Distribution des livres en français \nselon leur date de numérisation", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack")
        return(plot4)
      }
      else if(input$corpus_structure_l==10){
        p_dewey_livres<-read.csv("p_themes_livres.csv",encoding = "UTF-8")
        plot4<-plot_ly(p_dewey_livres,x=~date,y=~n,color=~dewey_nom,type='bar',colors="Dark2")
        plot4<-layout(plot4, title="Distribution des livres en français \nselon leur classement thématique Dewey", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot4)
      }
    }
    else if(input$corpus_relative_l==TRUE){
      if(input$corpus_structure_l==1 & input$corpus_ngram_l==FALSE){
        table<-read.csv("base_livres_annees.csv",encoding="UTF-8")
        somme<-sum(table$base)
        table<-table[table$date>=1450,]
        table$hovers = str_c(table$date,": N = ",table$base)
        plot2<-plot_ly(table, x=~date,y=~base,text=~hovers,type='bar',hoverinfo="text")
        Title = paste("<a href = 'https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22)%20and%20(gallicapublication_date%3E=%221380%22%20and%20gallicapublication_date%3C=%222021%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords='> <b>Répartition des ",somme," livres en français océrisés\ndans Gallica<b> </a>")
        y <- list(title = "Nombre de livres dans Gallica",titlefont = 41)
        x <- list(title = "Date",titlefont = 41)
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_l==1 & input$corpus_ngram_l==TRUE){
        ngram<-read.csv("ngram_viewer_fre_20200217.csv",encoding = "UTF-8")
        total_volume_count<-sum(ngram$volume_count)
        plot2<-plot_ly(ngram, x=~year,y=~volume_count,type='bar')
        Title = paste("<b>Répartition des ",total_volume_count," livres océrisés et en français\nexploités dans"," <a href = 'https://books.google.com/ngrams/graph?content=Joffre%2CP%C3%A9tain%2CFoch&year_start=1914&year_end=1920&corpus=30&smoothing=0&direct_url=t1%3B%2CJoffre%3B%2Cc0%3B.t1%3B%2CP%C3%A9tain%3B%2Cc0%3B.t1%3B%2CFoch%3B%2Cc0'>Google Ngram Viewer</a><b>")
        y <- list(title = "Nombre de livres exploités dans Ngram Viewer",titlefont = 41)
        x <- list(title = "Date",titlefont = 41)
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_l==2){
        p_villes_livres<-read.csv("p_villes_livres.csv",encoding = "UTF-8")
        plot3<-plot_ly(p_villes_livres,x=~date,y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot3<-layout(plot3, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des livres en français \nselon leur ville de publication", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot3)
      }
      else if(input$corpus_structure_l==3){
        p_droits_livres<-read.csv("p_droits_livres.csv",encoding = "UTF-8")
        plot5<-plot_ly(p_droits_livres,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des livres en français \nselon leur régime juridique", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot5)
      }
      else if(input$corpus_structure_l==4){
        p_sources_livres<-read.csv("p_sources_livres.csv",encoding = "UTF-8")
        p_sources_livres$principales_sources<-str_remove_all(p_sources_livres$principales_sources,"[/].+")
        p_sources_livres$principales_sources<-str_remove_all(p_sources_livres$principales_sources,"[-].+")
        p_sources_livres$principales_sources<-str_remove_all(p_sources_livres$principales_sources,"[()].+")
        plot4<-plot_ly(p_sources_livres,x=~date,y=~n,color=~principales_sources,type='bar',colors="Dark2")
        plot4<-layout(plot4, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des livres en français \nselon leur bibliothèque de numérisation d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot4)
      }
      else if(input$corpus_structure_l==5){
        p_pages_livres<-read.csv("p_pages_livres.csv",encoding = "UTF-8")
        plot7<-plot_ly(p_pages_livres,x=~date,y=~Mean,type='bar',colors="Dark2")
        plot7<-layout(plot7, title="Distribution des livres en français \nselon leur volume (nombre de pages moyen)", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de pages"),barmode="stack",bargap=0)
        return(plot7)
      }
      else if(input$corpus_structure_l==6){
        p_pages_livres<-read.csv("p_pages_livres.csv",encoding = "UTF-8")
        plot6<-plot_ly(p_pages_livres,x=~date,y=~Median,type='bar',colors="Dark2")
        plot6<-layout(plot6, title="Distribution des livres en français \nselon leur volume (nombre de pages médian)", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de pages"),barmode="stack",bargap=0)
        return(plot6)
      }
      else if(input$corpus_structure_l==7){
        tableau<-read.csv("base_livres_annees_bnf.csv",encoding = "UTF-8")
        tableau1<-read.csv("base_livres_annees.csv",encoding = "UTF-8")
        tableau3<-read.csv("base_livres_annees_numerises.csv",encoding = "UTF-8")
        for (i in 1:1379) {
          a<-as.data.frame(cbind(i,0))
          colnames(a)<-c("date","base")
          tableau1<-bind_rows(tableau1,a)
          tableau1<-tableau1[order(tableau1$date),]
          rownames(tableau1)<-NULL
          tableau3<-bind_rows(tableau3,a)
          tableau3<-tableau3[order(tableau3$date),]
          rownames(tableau3)<-NULL
        }
        tableau3$base<-tableau3$base-tableau1$base
        tableau$base<-tableau$base-tableau1$base-tableau3$base
        tableau$corpus<-"Non numérisé"
        tableau1$corpus<-"Numérisé et océrisé"
        tableau3$corpus<-"Numérisé mais pas océrisé"
        tableau2<-bind_rows(tableau,tableau1,tableau3)
        
        plot19<-plot_ly(tableau2,x=~as.integer(tableau2$date),y=~base,color=~corpus,type='bar',colors="Dark2")
        plot19<-layout(plot19, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),title="Distribution des livres en français \nselon leur état de numérisation", xaxis=list(title="Date",tickangle="-45",range=c("1380","2021")),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        
        return(plot19)
      }
      else if(input$corpus_structure_l==9){
        first_indexation_date<-read.csv("first_indexation_date.csv",encoding = "UTF-8")
        plot4<-plot_ly(first_indexation_date,x=~first_indexation_date,y=~count,type='bar',colors="Dark2")
        plot4<-layout(plot4, title="Distribution des livres en français \nselon leur date de numérisation", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack")
        return(plot4)
      }
      else if(input$corpus_structure_l==10){
        p_dewey_livres<-read.csv("p_themes_livres.csv",encoding = "UTF-8")
        plot4<-plot_ly(p_dewey_livres,x=~date,y=~n,color=~dewey_nom,type='bar',colors="Dark2")
        plot4<-layout(plot4, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des livres en français \nselon leur classement thématique Dewey", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot4)
      }
      
    }
  }
  observeEvent(input$corpus_structure_l,{observeEvent(input$corpus_relative_l,{
    output$corpus2<-renderPlotly({corpus_display_l()})
    if (input$corpus_structure_l==9){
      first_indexation_date<-read.csv("first_indexation_date.csv",encoding = "UTF-8")
      for (i in 2:length(first_indexation_date$count)) {
        first_indexation_date$count[i]<-first_indexation_date$count[i]+first_indexation_date$count[i-1]
      }
      ploplo<-plot_ly(first_indexation_date,x=~first_indexation_date,y=~count,type='bar',colors="Dark2")
      ploplo<-layout(ploplo, title="Distribution des livres en français \nselon leur date de numérisation", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack")
      
      output$corpus3<-renderPlotly(ploplo)
    }
  })})
  
  shinyOptions(progress.style="old")
})