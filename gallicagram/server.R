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
library(purrr)
library(rvest)
library(RSelenium)



js <- "
function(el, x) {
el.on('plotly_click', function(d) {
var point = d.points[0];
var url = point.data.customdata[point.pointIndex];
window.open(url);
});
}"

se="linux"

Plot <- function(data,input){

  tableau = data[["tableau"]]
  if(is.null(data[["tableau_page"]])==FALSE){
  if(input$search_mode==2){
      tableau = data[["tableau_page"]]
  }
  if(input$doc_type==4 & input$search_mode==1){
    tableau = data[["tableau_volume"]]
  }
  }
  if(input$multicourbes==TRUE){
    tableau = memoire
    tableau$mot<-str_c(tableau$mot,"_",tableau$corpus,"_",tableau$search_mode)
    if(input$resolution=="Mois"){
      tableau<-tableau[tableau$resolution=="Mois",]
    }
    if(input$resolution=="Année"){
      tableau<-tableau[tableau$resolution=="Année",]
    }
  }
  
  if(data[["resolution"]]=="Mois"){
    tableau$date<-str_c(tableau$date,"/01")
    tableau$date<-as.Date.character(tableau$date,format = c("%Y/%m/%d"))
  }
  if(data[["resolution"]]=="Année"){
    tableau$date<-str_c(tableau$date,"/01/01")
    tableau$date<-as.Date.character(tableau$date,format = c("%Y/%m/%d"))
  }
    Title = paste("")
    tableau$scale<-tableau$ratio
    
      for(mot in unique(tableau$mot)){
        z = which(tableau$mot==mot)
        tableau$scale[z]=scale(tableau$scale[z],center = T,scale = T)
        }
    
    width = length(unique(tableau$date))
    span = 2/width + input$span*(width-2)/(10*width)
    if(input$scale==TRUE |input$multicourbes==TRUE){tableau$loess = tableau$scale}
    else {tableau$loess = tableau$ratio}
    if(input$span >0){
      for(mot in str_split(data$mot,"&")[[1]]){
        z = which(tableau$mot==mot)
        x = 1:length(z)
        tableau$loess[z] = loess(tableau$loess[z]~x,span=span)$fitted
      }}
    if(input$scale==FALSE & input$multicourbes==FALSE){tableau$loess[tableau$loess<0]<-0}
    dn<-as.character(max(format(tableau$ratio,scientific=FALSE)))
    if(max(tableau$ratio)>=0.1){digit_number=".1%"}
    else{
    digit_number=str_extract(dn,"\\..+")
    digit_number=str_replace(digit_number,"\\.","")
    digit_number=str_extract(digit_number,"0+")
    digit_number<-str_length(digit_number)
    digit_number<-str_c(".",digit_number,"%")
    }
    
    if(data[["resolution"]]=="Mois"){tableau$hovers = str_c(str_extract(tableau$date,"......."),": x/N = ",tableau$count,"/",tableau$base,"\n                 = ",round(tableau$ratio*100,digits = 1),"%")}
    else{tableau$hovers = str_c(str_extract(tableau$date,"...."),": x/N = ",tableau$count,"/",tableau$base,"\n                 = ",round(tableau$ratio*100,digits = 1),"%")}
    y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41,tickformat = digit_number)
    if(input$scale==TRUE | input$multicourbes==TRUE){y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41)}
    x <- list(title = "",titlefont = 41)
    if(input$doc_type==5 | input$doc_type==9 | input$doc_type==10 | input$doc_type==12){
      tableau$hovers = str_c(tableau$date," : ",round(tableau$ratio*100,digits = 5),"%")
      y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41,tickformat = digit_number)
      if(input$scale==TRUE | input$multicourbes==TRUE){y <- list(title = "Fréquence d'occurrence dans\nle corpus",titlefont = 41)}
      }
    plot = plot_ly(tableau, x=~date,y=~loess,text=~hovers,color =~mot,type='scatter',mode='spline',hoverinfo="text",customdata=tableau$url)
    plot = layout(plot, yaxis = y, xaxis = x,title = Title)
    if(length(grep(",",data$mot))==0){plot = layout(plot,showlegend=TRUE,legend = list(orientation = 'h',x=1,xanchor="right"))}
    
    if(input$delta==TRUE){
      mots<-str_split(input$mot,"&")
      x = 1:sum(tableau$mot==unlist(mots)[1])
      tableau$delta[tableau$mot==unlist(mots)[1]]<-loess((tableau$ratio[tableau$mot==unlist(mots)[1]]-tableau$ratio[tableau$mot==unlist(mots)[2]]~x),span=span)$fitted
      tableau$hovers2 = str_c(tableau$date,": delta = ",round(tableau$delta*100,digits=2),"%, N = ",tableau$base)
      plot = plot_ly(filter(tableau,mot==unlist(mots)[[1]]), x=~date,y=~delta,text=~hovers2,type='scatter',mode='spline',hoverinfo="text")
      y <- list(title = "Différence de fréquence\nd'occurrence dans le corpus",titlefont = 41,tickformat = digit_number)
      x <- list(title = "",titlefont = 41)
      Title = paste("Freq(",unlist(mots)[1],") – Freq(",unlist(mots)[2],")")
      Title=str_remove_all(Title," ")
      plot = layout(plot, yaxis = y, xaxis = x,title = Title)
    }
    if(input$barplot){
      width = nrow(tableau)
      span = 2/width + input$span*(width-2)/(10*width)
      tableau$hovers = str_c(tableau$date,": N = ",tableau$base)
      plot1 = plot_ly(tableau, x=~date[tableau$mot==mot[1]],y=~base[tableau$mot==mot[1]],text=~hovers[tableau$mot==mot[1]],type='bar',hoverinfo="text",marker = list(color='rgba(31, 119, 180,1)'))
      y <- list(title = "",titlefont = 41)
      x <- list(title = "",titlefont = 41)
      plot1 = layout(plot1, yaxis = y, xaxis = x,title = Title,showlegend = FALSE)
      plot= plot%>%add_lines()
      plot = plotly::subplot(plot,plot1,nrows = 2,legend=NULL,shareX = T)
      return(onRender(plot,js))
    } else{
      plot=layout(plot)
      return(onRender(plot,js))
    }
  
}

message<-function(mot,from,to,doc_type,titres){
  
  mot=str_remove(mot,"&.+")
  mot=str_remove(mot,"[+].+")
  mot=str_replace_all(mot,"[:punct:]"," ")
  
  
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
    mot1<-mots_or[1]} else{mot1=mot2}
  
  if(doc_type == 3 & length(titres)>1){
    ark1<-titres[1]
    ark3<-""
    for (v in 2:length(titres)) 
    {
      ark<-titres[v]
      ark2<-str_c("%20or%20dc.relation%20any%20%22",ark,"%22")
      ark3<-str_c(ark3,ark2)
    }
  }else if(doc_type == 3 & length(titres)==1)
  {
    ark1<-titres
    ark3<-""
  }
  ###
  i=1
  page<-function(i,maxR)
  {
    
    beginning=from
    end=to
    if(doc_type==2){
      url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=",maxR,"&startRecord=",i,"&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
    }
    if(doc_type==1){
      url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=",maxR,"&startRecord=",i,"&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"/01/01%22%20and%20gallicapublication_date%3C=%22",end,"/12/01%22)&suggest=10&keywords=",mot1,or_end)
    }
    if(doc_type == 3){
      url <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=",maxR,"&page=",i,"&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"/01/01%22%20and%20gallicapublication_date%3C=%22",end,"/12/01%22)sortby%20dc.date%20")
    }
    read_xml(RETRY("GET",url,times = 3))
  }
  tot<-page(1,50)
  te <- xml2::as_list(tot)
  nmax <- as.integer(unlist(te$searchRetrieveResponse$numberOfRecords))
  
  texte<-str_c("Votre recherche apparait dans ",nmax," documents.")
  if(doc_type == 3){texte<-str_c("Votre recherche va s'appliquer à ",nmax," documents.")}
  
  if(nmax>5000){texte<-str_c(texte, " C'est trop pour Gallicagram ! Modifiez votre recherche.")}
  return(texte)
}

rapport <- function(mot,from,to,doc_type,titres){
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  
  mot=str_remove(mot,"&.+")
  mot=str_remove(mot,"[+].+")
  mot=str_replace_all(mot,"[:punct:]"," ")
  
  
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
    mot1<-mots_or[1]} else{mot1=mot2}
  
  
  if(doc_type == 3 & length(titres)>1){
    ark1<-titres[1]
    ark3<-""
    for (v in 2:length(titres)) 
    {
      ark<-titres[v]
      ark2<-str_c("%20or%20dc.relation%20any%20%22",ark,"%22")
      ark3<-str_c(ark3,ark2)
    }
  }else if(doc_type == 3 & length(titres)==1)
  {
    ark1<-titres
    ark3<-""
  }
  ###
  i=1
  page<-function(i,maxR)
  {
    
    beginning=from
    end=to
    if(doc_type==2){
    url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=",maxR,"&startRecord=",i,"&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
    }
    if(doc_type==1){
      url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=",maxR,"&startRecord=",i,"&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"/01/01%22%20and%20gallicapublication_date%3C=%22",end,"/12/31%22)&suggest=10&keywords=",mot1,or_end)
    }
    if(doc_type == 3){
      url <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=",maxR,"&page=",i,"&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"/01/01%22%20and%20gallicapublication_date%3C=%22",end,"/12/31%22)sortby%20dc.date%20")
    }
    read_xml(RETRY("GET",url,times = 3))
  }
  tot<-page(1,50)
  te <- xml2::as_list(tot)
  nmax <- as.integer(unlist(te$searchRetrieveResponse$numberOfRecords))
  if(nmax<=50){tot <- page(1,5)
  for (j in seq(6, nmax, by = 5)){
    temp <- page(j,5)
    for (l in xml2::xml_children(temp)){
      xml2::xml_add_child(tot, l)
    }
    progress$inc(5/nmax, message = paste("Téléchargement en cours...",as.integer((j/nmax)*100),"%"))
  }}
  if(nmax>50){
    for (j in seq(51, nmax, by = 50)){
      temp <- page(j,50)
      for (l in xml2::xml_children(temp)){
        xml2::xml_add_child(tot, l)
      }
      progress$inc(50/nmax, message = paste("Téléchargement en cours...",as.integer((j/nmax)*100),"%"))
    }}
  progress$set(message = "Traitement en cours ; cette étape va être longue...", value = 100)
  xml_to_df <- function(doc, ns = xml_ns(doc)) {
    split_by <- function(.x, .f, ...) {
      vals <- map(.x, .f, ...)
      split(.x, simplify_all(transpose(vals)))
    }
    node_to_df <- function(node) {
      # Filter the attributes for ones that aren't namespaces
      # x <- list(.index = 0, .name = xml_name(node, ns))
      x <- list(.name = xml_name(node, ns))
      # Attributes as column headers, and their values in the first row
      attrs <- xml_attrs(node)
      if (length(attrs) > 0) {attrs <- attrs[!grepl("xmlns", names(attrs))]}
      if (length(attrs) > 0) {x <- c(x, attrs)}
      # Build data frame manually, to avoid as.data.frame's good intentions
      children <- xml_children(node)
      if (length(children) >= 1) {
        x <- 
          children %>%
          # Recurse here
          map(node_to_df) %>%
          split_by(".name") %>%
          map(bind_rows) %>%
          map(list) %>%
          {c(x, .)}
        attr(x, "row.names") <- 1L
        class(x) <- c("tbl_df", "data.frame")
      } else {
        x$.value <- xml_text(node)
      }
      x
    }
    node_to_df(doc)
  }
  x = 1:3
  parse_gallica <- function(x){
    xml2::xml_find_all(tot, ".//srw:recordData")[x] %>% 
      xml_to_df() %>% 
      select(-.name) %>% 
      .$`oai_dc:dc` %>% 
      .[[1]] %>% 
      mutate(recordId = 1:nrow(.)) %>% 
      #    tidyr::unnest() %>% 
      tidyr::gather(var, val, - recordId) %>% 
      group_by(recordId, var) %>% 
      mutate(value = purrr::map(val, '.value') %>% purrr::flatten_chr() %>% paste0( collapse = " -- ")) %>% 
      select(recordId, var, value) %>% 
      ungroup() %>% 
      mutate(var = stringr::str_remove(var, 'dc:')) %>% 
      tidyr::spread(var, value) %>% 
      select(-.name)
  }
  # h=1
  # tot_df<-h%>%parse_gallica
  # for (h in 2:nmax) {
  #   ligne<-h%>%parse_gallica
  #   tot_df<-bind_rows(tot_df,ligne)
  #   progress$inc((1/nmax), detail = paste("Traitement des données",as.integer((h/nmax)*100),"%"))
  # }
  tot_df <- 1:nmax %>% 
  
    parse_gallica %>% 
    bind_rows()
  
  return(tot_df)
}

page_search <- function(mot,from,to,resolution,tot_df,doc_type,search_mode,titres){
  mot=str_remove(mot,"&.+")
  mot=str_remove(mot,"[+].+")
  mot=str_replace_all(mot,"[:punct:]"," ")
  mot_init<-mot
  mot=str_replace_all(mot,"[:space:]","%20")
  
  mot2 = mot
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
    mot1<-mots_or[1]} else{mot1=mot2}
  
  
  if(doc_type == 3 & length(titres)>1){
    ark1<-titres[1]
    ark3<-""
    for (v in 2:length(titres)) 
    {
      ark<-titres[v]
      ark2<-str_c("%20or%20dc.relation%20any%20%22",ark,"%22")
      ark3<-str_c(ark3,ark2)
    }
  }else if(doc_type == 3 & length(titres)==1)
  {
    ark1<-titres
    ark3<-""
  }
  
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
  
  if(doc_type==2){base_pages<-read.csv("base_pages_livres_annees.csv",encoding = "UTF-8")}
  
  tot_df$detect<-FALSE
  for (i in 1:length(tot_df$ark)) {
    url<-str_c("https://gallica.bnf.fr/services/ContentSearch?ark=",tot_df$ark[i],"&query=",mot)
    resultat<-as.character(read_html(RETRY("GET",url,times = 3)))
    resultat=str_remove_all(resultat,"[:space:]")
    resultat=str_remove_all(resultat,".+countresults")
    resultat=str_remove_all(resultat,"searchtime.+")
    resultat=str_extract(resultat,"[:digit:]+")
    tot_df$resultats[i]<-as.integer(resultat)
    
    if(doc_type==4 | doc_type == 3){
    url_base<-str_c("https://gallica.bnf.fr/services/ContentSearch?ark=",tot_df$ark[i],"&query=%20")
    resultat_base<-as.character(read_html(RETRY("GET",url_base,times = 3)))
    resultat_base=str_remove_all(resultat_base,"[:space:]")
    resultat_base=str_remove_all(resultat_base,".+countresults")
    resultat_base=str_remove_all(resultat_base,"searchtime.+")
    resultat_base=str_extract(resultat_base,"[:digit:]+")
    tot_df$resultats_base[i]<-as.integer(resultat_base)
    }
    if(doc_type==2){
      base<-base_pages$count[tot_df$date[i]==base_pages$date]
      tot_df$resultats_base[i]<-as.integer(base)
    }
    
    
    if(as.integer(resultat)>0){tot_df$detect[i]<-TRUE}
    progress$inc(1/length(tot_df$ark), detail = paste(as.integer(i*100/length(tot_df$ark)),"% traités"))
  }
  tableau$count<-0
  tableau$detect<-0
  tableau$count_base<-0
  tableau$mot<-mot_init
  for (j in 1:length(tableau$date)) {
    tableau$count[j]<-sum(tot_df$resultats[tot_df$date==tableau$date[j]])
    tableau$count_base[j]<-sum(tot_df$resultats_base[tot_df$date==tableau$date[j]])
    tableau$detect[j]<-sum(tot_df$detect[tot_df$date==tableau$date[j]])
    
    if(doc_type==2){tableau$url[j]<-str_c(url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",tableau$date[j],"%22%20and%20gallicapublication_date%3C=%22",tableau$date[j],"%22)&suggest=10&keywords=",mot1,or_end))}
    if(doc_type == 3){
      beginning<-str_replace_all(tableau$date[j],"-","/")
      end<-str_replace_all(tableau$date[j],"-","/")
      if(resolution=="Années"){
        beginning=str_c(beginning,"/01")
        end=str_c(end,"/12")}
      tableau$url[j]<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=20&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(text%20adj%20%22",mot,"%22%20)%20and%20(gallicapublication_date%3E=%22",beginning,"/01%22%20and%20gallicapublication_date%3C=%22",end,"/31%22)sortby%20dc.date%20")}
    if(doc_type==4){tableau$url[j]<-"https://gallica.bnf.fr/"}
  }

  
  colnames(tableau)<-c("date","base","page_count","count","page_base","mot","url")
  tableau$ratio<-tableau$count/tableau$base
  tableau$ratio_page<-tableau$page_count/tableau$page_base
  tableau$ratio[is.na(tableau$ratio)]<-0
  tableau$ratio_page[is.na(tableau$ratio_page)]<-0
  tableau$date<-str_replace_all(tableau$date,"-","/")
  
  tableau_page<-select(tableau,date,page_count,page_base,mot,url,ratio_page)
  tableau_page$resolution<-resolution
  if(doc_type==2){tableau_page$corpus<-"livres_gallica"}
  if(doc_type == 3){tableau_page$corpus<-"titre_presse_gallica"}
  if(doc_type==4){tableau_page$corpus<-"perso_gallica"}
  tableau_page$search_mode<-"page"
  colnames(tableau_page)<-c("date",	"count",	"base",	"mot",	"url",	"ratio",	"resolution",	"corpus",	"search_mode")
  
  tableau_volume<-select(tableau,date,count,base,mot,url,ratio)
  tableau_volume$resolution<-resolution
  if(doc_type==2){tableau_volume$corpus<-"livres_gallica"}
  if(doc_type == 3){tableau_volume$corpus<-"titre_presse_gallica"}
  if(doc_type==4){tableau_volume$corpus<-"perso_gallica"}
  tableau_volume$search_mode<-"volume"
  colnames(tableau_volume)<-c("date",	"count",	"base",	"mot",	"url",	"ratio",	"resolution",	"corpus",	"search_mode")

  if(doc_type==4){memoire<<-bind_rows(tableau_volume,memoire)}
  memoire<<-bind_rows(tableau_page,memoire)
  
  data = list(tableau_volume,tableau_page,paste(mot),resolution)
  names(data) = c("tableau_volume","tableau_page","mot","resolution")
  return(data)
}

get_data <- function(mot,from,to,resolution,doc_type,titres){
  if(doc_type==6 | doc_type==7){mot <- iconv(mot, from="UTF-8",to="ASCII//TRANSLIT//IGNORE")}
  mots = str_split(mot,"&")[[1]]
  tableau<-as.data.frame(matrix(nrow=0,ncol=5),stringsAsFactors = FALSE)
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  if(doc_type==1 & resolution=="Année"){
    base=read.csv("base_presse_annees.csv")
  } else  if(doc_type==1 & resolution=="Mois"){
    base=read.csv("base_presse_mois.csv")
  } else if(doc_type==2){
    base=read.csv("base_livres_annees.csv")
  }else  if(doc_type==6 & resolution=="Année"){
    base=read.csv("base_presse_annees_europeana_de.csv")
  }else  if(doc_type==6 & resolution=="Mois"){
    base=read.csv("base_presse_mois_europeana_de.csv")
  }else  if(doc_type==7 & resolution=="Année"){
    base=read.csv("base_presse_annees_europeana_nl.csv")
  }else  if(doc_type==7 & resolution=="Mois"){
    base=read.csv("base_presse_mois_europeana_nl.csv")
  }else  if(doc_type==8 & resolution=="Année"){
    base=read.csv("base_presse_annees_bna_en.csv")
  }else  if(doc_type==8 & resolution=="Mois"){
    base=read.csv("base_presse_mois_bna_en.csv")
  }else  if(doc_type==11 & resolution=="Année"){
    base=read.csv("base_presse_annees_bne_es.csv")
  }else  if(doc_type==11 & resolution=="Mois"){
    base=read.csv("base_presse_mois_bne_es.csv")
  }else  if(doc_type==13 & resolution=="Année"){
    base=read.csv("base_presse_annees_kbr_fr.csv")
  }else  if(doc_type==13 & resolution=="Mois"){
    base=read.csv("base_presse_mois_kbr_fr.csv")
  }else  if(doc_type==14 & resolution=="Année"){
    base=read.csv("base_presse_annees_kbr_nl.csv")
  }else  if(doc_type==14 & resolution=="Mois"){
    base=read.csv("base_presse_mois_kbr_nl.csv")
  }else  if(doc_type==15 & resolution=="Année"){
    base=read.csv("base_presse_annees_e-newspaperarchives_fr.csv")
  }else  if(doc_type==15 & resolution=="Mois"){
    base=read.csv("base_presse_mois_e-newspaperarchives_fr.csv")
  }else  if(doc_type==16 & resolution=="Année"){
    base=read.csv("base_presse_annees_e-newspaperarchives_de.csv")
  }else  if(doc_type==16 & resolution=="Mois"){
    base=read.csv("base_presse_mois_e-newspaperarchives_de.csv")
  }else  if(doc_type==17 & resolution=="Année"){
    base=read.csv("base_presse_annees_lectura_fr.csv")
  }else  if(doc_type==17 & resolution=="Mois"){
    base=read.csv("base_presse_mois_lectura_fr.csv")
  }else  if(doc_type==18 & resolution=="Année"){
    base=read.csv("base_presse_annees_limedia_fr.csv")
  }else  if(doc_type==18 & resolution=="Mois"){
    base=read.csv("base_presse_mois_limedia_fr.csv")
  }else  if(doc_type==19 & resolution=="Année"){
    base=read.csv("base_presse_annees_memonum_fr.csv")
  }else  if(doc_type==19 & resolution=="Mois"){
    base=read.csv("base_presse_mois_memonum_fr.csv")
  }else  if(doc_type==20 & resolution=="Année"){
    base=read.csv("base_presse_annees_communpatrimoine_fr.csv")
  }else  if(doc_type==20 & resolution=="Mois"){
    base=read.csv("base_presse_mois_communpatrimoine_fr.csv")
  }else  if(doc_type==21 & resolution=="Année"){
    base=read.csv("base_presse_annees_yroise_fr.csv")
  }else  if(doc_type==21 & resolution=="Mois"){
    base=read.csv("base_presse_mois_yroise_fr.csv")
  }else  if(doc_type==22 & resolution=="Année"){
    base=read.csv("base_presse_annees_pireneas_fr.csv")
  }else  if(doc_type==22 & resolution=="Mois"){
    base=read.csv("base_presse_mois_pireneas_fr.csv")
  }else  if(doc_type==23 & resolution=="Année"){
    base=read.csv("base_presse_annees_rosalis_fr.csv")
  }else  if(doc_type==23 & resolution=="Mois"){
    base=read.csv("base_presse_mois_rosalis_fr.csv")
  }else  if(doc_type==24 & resolution=="Année"){
    base=read.csv("base_presse_annees_bdn_fr.csv")
  }else  if(doc_type==24 & resolution=="Mois"){
    base=read.csv("base_presse_mois_bdn_fr.csv")
  }else  if(doc_type==25 & resolution=="Année"){
    base=read.csv("base_presse_annees_rfnum_fr.csv")
  }else  if(doc_type==25 & resolution=="Mois"){
    base=read.csv("base_presse_mois_rfnum_fr.csv")
  }else  if(doc_type==26 & resolution=="Année"){
    base=read.csv("base_presse_annees_numistral_fr.csv")
  }else  if(doc_type==26 & resolution=="Mois"){
    base=read.csv("base_presse_mois_numistral_fr.csv")
  }else  if(doc_type==27 & resolution=="Année"){
    base=read.csv("base_presse_annees_bn-r_fr.csv")
  }else  if(doc_type==27 & resolution=="Mois"){
    base=read.csv("base_presse_mois_bn-r_fr.csv")
  }else  if(doc_type==28 & resolution=="Année"){
    base=read.csv("base_presse_annees_banq_fr.csv")
  }else  if(doc_type==28 & resolution=="Mois"){
    base=read.csv("base_presse_mois_banq_fr.csv")
  }
  
  
  
  if(doc_type==13 | doc_type==14 | doc_type==19 | doc_type==28){
    if(se=="windows"){system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
      rD <- rsDriver(browser = "firefox", port = 4444L)}
    #if(se=="linux"){system("kill -9 $(lsof -t -i:4444)", intern=FALSE, ignore.stdout=FALSE)}
    if(se=="linux"){rD <- rsDriver(browser = "firefox", port = 4444L)}
    remDr <- rD[["client"]]
  }
  if(doc_type==28){
    url="https://numerique.banq.qc.ca/rechercheExterne/encoded/Sm9mZnJl/false/P/desc/W3sibm9tIjoiY29ycHVzIiwidmFsZXVyIjoiUGF0cmltb2luZSUyMHF1w6liw6ljb2lzIn0seyJub20iOiJ0eXBlX2RvY19mIiwidmFsZXVyIjoiUmV2dWVzJTIwZXQlMjBqb3VybmF1eCJ9LHsibm9tIjoibGFuZ3Vlc19jb250ZW51IiwidmFsZXVyIjoiZnJhbsOnYWlzIn0seyJub20iOiJhdmVjX3RleHRlX2ludGVncmFsIiwidmFsZXVyIjoib3VpIn0seyJub20iOiJnZW5yZV9mIiwidmFsZXVyIjoiSm91cm5hdXgifV0=/Liste%20de%20r%C3%A9sultats/true/false/eyJkZWJ1dCI6eyJhbm5lZSI6MTkxNCwibW9pcyI6MSwiam91ciI6MX0sImZpbiI6eyJhbm5lZSI6MTkxNCwibW9pcyI6MTIsImpvdXIiOjMxfX0="
    remDr$navigate(url)
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
          if(doc_type==1 | doc_type==2 | doc_type == 3 | doc_type==4 | doc_type==20 | doc_type==21 | doc_type==22 | doc_type==23 | doc_type==24 | doc_type==25 | doc_type==26)
          {or1[j]<-str_c("or%20text%20adj%20%22",mots_or[j],"%22%20")
          or1_end[j]<-str_c("%20",mots_or[j])}
          if(doc_type==6 | doc_type==7 | doc_type==8)
          {or1[j]<-str_c("OR%22",mots_or[j],"%22")
          or1_end[j]<-str_c("")}
          if(doc_type==11)
          {or1[j]<-str_c("&o=or")
          or1_end[j]<-str_c("&w=%22",mots_or[j],"%22")}
          if(doc_type==15 | doc_type==16)
          {or1[j]<-str_c("+OR+%22",mots_or[j],"%22")
          or1_end[j]<-str_c("")}
          if(doc_type==19)
          {or1[j]<-str_c(",%22",mots_or[j],"%22")
          or1_end[j]<-str_c("")}
          
          or<-str_c(or,or1[j])
          or_end<-str_c(or_end,or1_end[j])
        }
        mot1<-mots_or[1]}else{mot1=mot2}
      if(doc_type==15 | doc_type==16 | doc_type==18){
        mot1<-URLencode(mot1)
        or<-URLencode(or)
        }
      
      
      ###
      end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
      if(i%%4==0){end_of_month[2]=29}
      if(i==1700 | i==1800 | i==1900){end_of_month[2]=28}#Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
      y<-as.character(i)
      if(resolution=="Année"){beginning = str_c(y,"/01/01")
      end = str_c(y,"/12/31")}
      I = 1
      if(resolution=="Mois"){I=1:12} #Pour faire ensuite une boucle sur les mois
      
      
      if(doc_type !=2 & doc_type !=5 & doc_type !=9 & doc_type !=10 & doc_type !=12){
        for(j in I){
          if(resolution=="Mois"){
            z = as.character(j)
            if(nchar(z)<2){z<-str_c("0",z)}
            beginning = str_c(y,"/",z,"/01")
            end = str_c(y,"/",z,"/",end_of_month[j])}
          if(doc_type == 1){url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 3){
            liste_titres<-titres
            longueur_max=15
            iterations<-ceiling(length(liste_titres)/longueur_max)
            reste=iterations*longueur_max-length(liste_titres)
            a=0
            b=0
            for (k in 1:iterations) {
              if(k==iterations){titres<-liste_titres[((k-1)*longueur_max+1):(k*longueur_max-reste)]}
              else{titres<-liste_titres[((k-1)*longueur_max+1):(k*longueur_max)]}
              if(doc_type == 3 & length(titres)>1){
                ark1<-titres[1]
                ark3<-""
                for (v in 2:length(titres)) 
                {
                  ark<-titres[v]
                  ark2<-str_c("%20or%20dc.relation%20any%20%22",ark,"%22")
                  ark3<-str_c(ark3,ark2)
                }
              }else if(doc_type == 3 & length(titres)==1)
              {
                ark1<-titres
                ark3<-""
              }
              url <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)sortby%20dc.date%20")
              ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
              a<-a+as.integer(str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+"))
              url_base <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)%20sortby%20dc.date")
              ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
              b<-b+as.integer(str_extract(str_extract(ngram_base,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+"))
            }
            a<-as.character(a)
            b<-as.character(b)
            url="https://gallica.bnf.fr/"
            titres<-liste_titres
          }
          if(doc_type == 6){beginning<-str_replace_all(beginning,"/","-")
            end<-str_replace_all(end,"/","-")
            langue="de"
            url<-str_c("https://newspapers.eanadev.org/api/v2/search.json?query=%22",mot1,"%22",or,"&rows=1&profile=hits&wskey=%20athrobid&qf=proxy_dcterms_issued:%5B",beginning,"+TO+",end,"%5D&qf=LANGUAGE:de")}
          if(doc_type == 7){beginning<-str_replace_all(beginning,"/","-")
            end<-str_replace_all(end,"/","-")
            langue="nl"
            url<-str_c("https://newspapers.eanadev.org/api/v2/search.json?query=%22",mot1,"%22",or,"&rows=1&profile=hits&wskey=%20athrobid&qf=proxy_dcterms_issued:%5B",beginning,"+TO+",end,"%5D&qf=LANGUAGE:nl")}
          if(doc_type == 8){beginning<-str_replace_all(beginning,"/","-")
            end<-str_replace_all(end,"/","-")
            url<-str_c("https://www.britishnewspaperarchive.co.uk/search/results/",beginning,"/",end,"?basicsearch=%22",mot1,"%22",or,"&exactsearch=true&retrievecountrycounts=false")}
          if(doc_type == 11){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("http://hemerotecadigital.bne.es/results.vm?o=",or,"&w=%22",mot1,"%22",or_end,"&f=text&d=creation&d=",y,"&d=",z,"&d=01&d=",y,"&d=",z,"&d=",end_of_month[j],"&t=%2Bcreation&l=700&s=0&view=&lang=fr")
            }
            if(resolution=="Année"){
              url<-str_c("http://hemerotecadigital.bne.es/results.vm?o=",or,"&w=%22",mot1,"%22",or_end,"&f=text&d=creation&d=",y,"&d=01&d=01&d=",y,"&d=12&d=31&t=%2Bcreation&l=700&s=0&view=&lang=fr")
            }
          }
          if(doc_type == 13){beginning<-str_replace_all(beginning,"/","-")
            end<-str_replace_all(end,"/","-")
            url<-str_c("https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=",mot1,"&none_q=&from_d=",beginning,"&to_d=",end,"&per_lang=fr&per=&lang=FR&per_type=1")
          }
          if(doc_type == 14){beginning<-str_replace_all(beginning,"/","-")
            end<-str_replace_all(end,"/","-")
            url<-str_c("https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=",mot1,"&none_q=&from_d=",beginning,"&to_d=",end,"&per_lang=nl&per=&lang=FR&per_type=1")
          }
          if(doc_type == 15){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=",z,"&dafyq=",y,"&datdq=",end_of_month[j],"&datmq=",z,"&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
            }
            if(resolution=="Année"){
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=01&dafyq=",y,"&datdq=31&datmq=12&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
            }
          }
          if(doc_type == 16){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=",z,"&dafyq=",y,"&datdq=",end_of_month[j],"&datmq=",z,"&datyq=",y,"&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
            }
            if(resolution=="Année"){
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=01&dafyq=",y,"&datdq=31&datmq=12&datyq=",y,"&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
            }
          }
          if(doc_type == 17){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.lectura.plus/Presse/search/?query=",mot1,"&fromDate=01%2F",z,"%2F",y,"&untilDate=",end_of_month[j],"%2F",z,"%2F",y)
              }
            if(resolution=="Année"){
              url<-str_c("https://www.lectura.plus/Presse/search/?query=",mot1,"&fromDate=01%2F01%2F",y,"&untilDate=31%2F12%2F",y)
            }
          }
          if(doc_type == 18){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://kiosque.limedia.fr/recherche/?query=",mot1,"&search_type=exact&uniform_title=&date=&period_start=01/",z,"/",y,"&period_end=",end_of_month[j],"/",z,"/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")
              }
            if(resolution=="Année"){
              url<-str_c("https://kiosque.limedia.fr/recherche/?query=",mot1,"&search_type=exact&uniform_title=&date=&period_start=01/01/",y,"&period_end=31/12/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")
              }
          }
          if(doc_type == 19){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://memonum-mediatheques.montpellier3m.fr/form.aspx?SC=MEMONUM_ENCART_SEARCH#/Search/(query:(ForceSearch:!t,Grid:'%7B%22717%22:%5B%22",mot1,"%22",or,"%5D,%22719%22:%5B%22*",z,"/",y,"%22%5D%7D',Page:0,PageRange:3,QueryString:!n,ResultSize:10,ScenarioCode:MEMONUM_ENCART_SEARCH,SearchContext:1))")}
            if(resolution=="Année"){url<-str_c("https://memonum-mediatheques.montpellier3m.fr/form.aspx?SC=MEMONUM_ENCART_SEARCH#/Search/(query:(ForceSearch:!t,Grid:'%7B%22717%22:%5B%22",mot1,"%22",or,"%5D,%22719%22:%5B%22*",y,"%22%5D%7D',Page:0,PageRange:3,QueryString:!n,ResultSize:10,ScenarioCode:MEMONUM_ENCART_SEARCH,SearchContext:1))")}
          }
          if(doc_type == 20){url<-str_c("https://www.communpatrimoine.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 21){url<-str_c("https://yroise.biblio.brest.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 22){url<-str_c("https://www.pireneas.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 23){url<-str_c("https://rosalis.bibliotheque.toulouse.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 24){url<-str_c("https://bibliotheque-numerique.diplomatie.gouv.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 25){url<-str_c("http://rfnum-bibliotheque.org/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 26){url<-str_c("https://www.numistral.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)}
          if(doc_type == 27){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.bn-r.fr/presse_ancienne_resultats.php?type_rech=pr&q_fulltext=%22",mot1,"%22&pr_jour=&pr_mois=&pr_annee=&date_debut=01-",z,"-",y,"&date_fin=",end_of_month[j],"-",z,"-",y,"&sort=date_formated%20asc,tri_titre%20asc&from=presse#")
            }
            if(resolution=="Année"){
            url<-str_c("https://www.bn-r.fr/presse_ancienne_resultats.php?type_rech=pr&q_fulltext=%22",mot1,"%22&pr_jour=&pr_mois=&pr_annee=&date_debut=01-01-",y,"&date_fin=31-12-",y,"&sort=date_formated%20asc,tri_titre%20asc&from=presse#")
            }
          }
        
        
          if(doc_type == 1 | doc_type==20 | doc_type==21 | doc_type==22 | doc_type==23 | doc_type==24 | doc_type==25){
            ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
            a<-str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
          }
          if(doc_type == 6 | doc_type == 7){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_replace_all(ngram,"[:punct:]","")
            a<-str_extract(str_extract(ngram,"totalResults[:digit:]+"),"[:digit:]+")
            url<-str_c("https://classic.europeana.eu/portal/fr/collections/newspapers?q=%22",mot1,"%22",or,"&f%5BMEDIA%5D%5B%5D=true&f%5BTYPE%5D%5B%5D=TEXT&f%5BLANGUAGE%5D%5B%5D=",langue,"&f%5Bapi%5D%5B%5D=collection&range%5Bproxy_dcterms_issued%5D%5Bbegin%5D=",beginning,"&range%5Bproxy_dcterms_issued%5D%5Bend%5D=",end)}
          if(doc_type == 8){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_remove_all(ngram,"[:space:]")
            ngram<-str_extract(ngram,"Date--.+Newspapers--")
            ngram<-str_extract(ngram,'list-group-item"title.+')
            ngram<-str_remove_all(ngram,",")
            ngram<-str_c(unlist(str_extract_all(ngram,">[:digit:]+<")))
            ngram<-str_remove_all(ngram,"<")
            ngram<-str_remove_all(ngram,">")
            a<-sum(as.integer(ngram))
          }
          if(doc_type == 11){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_remove_all(ngram,"[:punct:]")
            ngram<-str_remove_all(ngram,"[:space:]")
            ngram<-str_remove_all(ngram,"<strong>")
            a<-str_extract(str_extract(ngram,"Results[:digit:]+"),"[:digit:]+")
            if (is.na(a)){a<-str_extract(str_extract(ngram,"Résultats[:digit:]+"),"[:digit:]+")}
          }
          if(doc_type == 13 | doc_type == 14){
            remDr$navigate(url)
            Sys.sleep(2) # give the page time to fully load
            ngram <- remDr$getPageSource()[[1]]
            ngram<-str_extract(ngram,"foundnumber.+")
            ngram<-str_remove_all(ngram,"[:punct:]")
            a<-as.integer(str_extract(ngram,"[:digit:]+"))
          }
          if(doc_type == 15 | doc_type == 16){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_remove_all(ngram,",")
            ngram<-str_extract(ngram,"Résultats 1 - 20 de  .+")
            ngram<-str_remove(ngram,"Résultats 1 - 20 de  ")
            a<-str_extract(ngram,"[:digit:]+")
          }
          if(doc_type == 17){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_extract(ngram,"width:100px.+")
            ngram<-str_remove(ngram,"width:100px")
            a<-str_extract(ngram,"[:digit:]+")
          }
          if(doc_type == 18){
            ngram<-read_html(RETRY("GET",url,times = 6))
            a<-str_extract(html_text(html_node(ngram,".col-milieu")),"[:digit:]+")
          }
          if(doc_type == 19){
            remDr$navigate(url)
            Sys.sleep(2) # give the page time to fully load
            ngram <- remDr$getPageSource()[[1]]
            ngram<-str_remove_all(ngram,"[:space:]")
            ngram<-str_remove_all(ngram,"<span>")
            ngram<-str_remove_all(ngram,"</span>")
            ngram<-str_extract(ngram,"Résultats1à[:digit:]+sur[:digit:]+")
            a<-str_remove(ngram,"Résultats1à[:digit:]+sur")
          }
          if(doc_type ==26){
            ngram<-read_html(url)
            ngram<-html_text(html_node(ngram,"head > title:nth-child(3)"))
            ngram<- str_remove_all(ngram,"[:space:]")
            a<-str_extract(ngram,"[:digit:]+")
          }
          if(doc_type ==27){
            ngram<-as.character(read_html(url))
            ngram<-str_extract(ngram,"Résultats de la recherche.+")
            a<-str_extract(ngram,"[:digit:]+")
          }
          if(doc_type ==28){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              
              Sys.sleep(2)
              webElem <- remDr$findElement(using = 'css selector',"#debutAnnee") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list(as.character(y)))
              webElem <- remDr$findElement(using = 'css selector',"#debutMois") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list(as.character(z)))
              webElem <- remDr$findElement(using = 'css selector',"#debutJour") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list("01"))
              webElem <- remDr$findElement(using = 'css selector',"#finAnnee") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list(as.character(y)))
              webElem <- remDr$findElement(using = 'css selector',"#finMois") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list(as.character(z)))
              webElem <- remDr$findElement(using = 'css selector',"#finJour") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list(as.character(end_of_month[j])))
              webElem <- remDr$findElement(using = 'css selector',"#submit") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem <- remDr$findElement(using = 'css selector',"#labelMotCle")
              webElem$clickElement()
              webElem <- remDr$findElement(using = 'css selector',"#recherche")
              webElem$clickElement() 
              webElem$sendKeysToElement(list(mot1))
              webElem <- remDr$findElement(using = 'css selector',"i.hidden-xs") 
              webElem$clickElement()
              webElem$clickElement()
              Sys.sleep(2)
              page <- read_html(remDr$getPageSource()[[1]])
              url<-html_attr(html_node(page,"#permalien"),"value")
              a<-html_text(html_node(page,".chapoNbResultat"))
              a<-str_extract(a,"[:digit:]+")
              webElem <- remDr$findElement(using = 'css selector',"#dateInterval")
              webElem$clickElement()
              Sys.sleep(1)
              webElem <- remDr$findElement(using = 'css selector',"facet-filter.ng-isolate-scope > div:nth-child(10) > ul:nth-child(1) > li:nth-child(1) > span:nth-child(1) > i:nth-child(1)")
              webElem$clickElement()
              
            }
            
            if(resolution=="Année"){
              Sys.sleep(2)
              webElem <- remDr$findElement(using = 'css selector',"#debutAnnee") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list(as.character(y)))
              webElem <- remDr$findElement(using = 'css selector',"#debutMois") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list("01"))
              webElem <- remDr$findElement(using = 'css selector',"#debutJour") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list("01"))
              webElem <- remDr$findElement(using = 'css selector',"#finAnnee") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list(as.character(y)))
              webElem <- remDr$findElement(using = 'css selector',"#finMois") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list("12"))
              webElem <- remDr$findElement(using = 'css selector',"#finJour") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem$sendKeysToElement(list("31"))
              webElem <- remDr$findElement(using = 'css selector',"#submit") #on accepte les cookies
              webElem$clickElement() #le clic pour accepter les cookies du coup
              webElem <- remDr$findElement(using = 'css selector',"#labelMotCle")
              webElem$clickElement()
              webElem <- remDr$findElement(using = 'css selector',"#recherche")
              webElem$clickElement()
              webElem$sendKeysToElement(list(mot1))
              webElem <- remDr$findElement(using = 'css selector',"i.hidden-xs") 
              webElem$clickElement()
              Sys.sleep(2)
              page <- read_html(remDr$getPageSource()[[1]])
              url<-html_attr(html_node(page,"#permalien"),"value")
              a<-html_text(html_node(page,".chapoNbResultat"))
              a<-str_extract(a,"[:digit:]+")
              webElem <- remDr$findElement(using = 'css selector',"#dateInterval")
              webElem$clickElement()
              Sys.sleep(1)
              webElem <- remDr$findElement(using = 'css selector',"facet-filter.ng-isolate-scope > div:nth-child(10) > ul:nth-child(1) > li:nth-child(1) > span:nth-child(1) > i:nth-child(1)")
              webElem$clickElement()
            }
          }
        
         
          if(resolution=="Mois"& (doc_type==1 | doc_type==6 | doc_type==7 | doc_type==8 | doc_type==11 | doc_type==13 | doc_type==14 | doc_type==15 | doc_type==16 | doc_type==17 | doc_type==18 | doc_type==19 | doc_type==20 | doc_type==21 | doc_type==22 | doc_type==23 | doc_type==24 | doc_type==25 | doc_type==26 | doc_type==27 | doc_type==28)){
            date=str_c(y,"/",z)
            b<-as.integer(base$base[base$date==date])}
          else if (resolution=="Année" & (doc_type==1 | doc_type==6 | doc_type==7 | doc_type==8 | doc_type==11 | doc_type==13 | doc_type==14 | doc_type==15 | doc_type==16 | doc_type==17 | doc_type==18 | doc_type==19 | doc_type==20 | doc_type==21 | doc_type==22 | doc_type==23 | doc_type==24 | doc_type==25 | doc_type==26 | doc_type==27 | doc_type==28)){b<-as.integer(base$base[base$date==y])}
          if(length(b)==0L){b=0}
          tableau[nrow(tableau)+1,] = NA
          date=y
          if(resolution=="Mois"){date = paste(y,z,sep="/")}
          tableau[nrow(tableau),]<-c(date,a,b,mot,url)
          progress$inc(1/((to-from+1)*length(I)*length(mots)), detail = paste("Gallicagram ratisse l'an", i))
        }}
      
      if(doc_type==2){
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
  tableau$count[is.na(tableau$count)]<-0
  tableau$url = str_replace(tableau$url,"SRU","services/engine/search/sru")
  tableau$url = str_replace(tableau$url,"maximumRecords=1","maximumRecords=25")
  
  if(doc_type==13 | doc_type==14 | doc_type==19 | doc_type==28){
    remDr$close()
    rD$server$stop()
    rm(rD)
    gc()
    if(se=="windows"){system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)}
    #if(se=="linux"){system("kill -9 $(lsof -t -i:4444)", intern=FALSE, ignore.stdout=FALSE)}
  }
  
  tableau$count<-as.integer(tableau$count)
  tableau$base<-as.integer(tableau$base)
  tableau$ratio<-tableau$count/tableau$base
  tableau$ratio[is.na(tableau$ratio)]<-0
  #ngram_viewer
  
  if(doc_type==5 | doc_type==9 | doc_type==10 | doc_type==12){
    if(doc_type==5){tableau=ngrami(mots,corpus = "fre_2019",year_start = from, year_end = to, smoothing = 0, aggregate = TRUE)}
    if(doc_type==9){tableau=ngrami(mots,corpus = "ger_2019",year_start = from, year_end = to, smoothing = 0, aggregate = TRUE)}
    if(doc_type==10){tableau=ngrami(mots,corpus = "eng_2019",year_start = from, year_end = to, smoothing = 0, aggregate = TRUE)}
    if(doc_type==12){tableau=ngrami(mots,corpus = "spa_2019",year_start = from, year_end = to, smoothing = 0, aggregate = TRUE)}
    tableau$search_mode<-"match"
    colnames(tableau)=c("date","mot","ratio","corpus","search_mode")
    tableau$base<-0
    tableau$date<-as.character(tableau$date)
    tableau$count<-0
    tableau$url<-""
    if(doc_type==5){
      for (i in 1:length(tableau$date)) {
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_fr") 
      }}
    if(doc_type==9){
      for (i in 1:length(tableau$date)) {
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_de") 
      }}
    if(doc_type==10){
      for (i in 1:length(tableau$date)) {
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_en") 
      }}
    if(doc_type==12){
      for (i in 1:length(tableau$date)) {
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_es") 
      }}
  }
  
  tableau$resolution<-resolution
  format = "%Y"
  if(resolution=="Mois"){format=paste(format,"%m",sep="/")}
  tableau.date = as.Date(as.character(tableau$date),format=format)
  if(doc_type==1){tableau$corpus="presse_gallica"
  tableau$search_mode<-"volume"}
  if(doc_type==2){tableau$corpus="livres_gallica"
  tableau$search_mode<-"volume"}
  if(doc_type==5){tableau$corpus="livres_fr_ngram"}
  if(doc_type == 3){tableau$corpus="titre_presse_gallica"
  tableau$search_mode<-"volume"}
  if(doc_type==4){tableau$corpus="perso_gallica"
  tableau$search_mode<-"volume"}
  if(doc_type==6){tableau$corpus="presse_de_europeana"
  tableau$search_mode<-"volume"}
  if(doc_type==7){tableau$corpus="presse_nl_europeana"
  tableau$search_mode<-"volume"}
  if(doc_type==8){tableau$corpus="presse_en_bna"
  tableau$search_mode<-"article"}
  if(doc_type==9){tableau$corpus="livres_de_ngram"}
  if(doc_type==10){tableau$corpus="livres_en_ngram"}
  if(doc_type==11){tableau$corpus="presse_es_bne"
  tableau$search_mode<-"page"}
  if(doc_type==12){tableau$corpus="livres_es_ngram"}
  if(doc_type==13){tableau$corpus="presse_fr_kbr"
  tableau$search_mode<-"page"}
  if(doc_type==14){tableau$corpus="presse_nl_kbr"
  tableau$search_mode<-"page"}
  if(doc_type==15){tableau$corpus="presse_fr_bns"
  tableau$search_mode<-"article"}
  if(doc_type==16){tableau$corpus="presse_de_bns"
  tableau$search_mode<-"article"}
  if(doc_type==17){tableau$corpus="presse_fr_lectura"
  tableau$search_mode<-"page"}
  if(doc_type==18){tableau$corpus="presse_fr_limedia"
  tableau$search_mode<-"volume"}
  if(doc_type==19){tableau$corpus="presse_fr_memonum"
  tableau$search_mode<-"volume"}
  if(doc_type==20){tableau$corpus="presse_fr_communpatrimoine"
  tableau$search_mode<-"volume"}
  if(doc_type==21){tableau$corpus="presse_fr_yroise"
  tableau$search_mode<-"volume"}
  if(doc_type==22){tableau$corpus="presse_fr_pireneas"
  tableau$search_mode<-"volume"}
  if(doc_type==23){tableau$corpus="presse_fr_rosalis"
  tableau$search_mode<-"volume"}
  if(doc_type==24){tableau$corpus="presse_fr_bdn"
  tableau$search_mode<-"volume"}
  if(doc_type==25){tableau$corpus="presse_fr_rfnum"
  tableau$search_mode<-"volume"}
  if(doc_type==26){tableau$corpus="presse_fr_numistral"
  tableau$search_mode<-"volume"}
  if(doc_type==27){tableau$corpus="presse_fr_bn-r"
  tableau$search_mode<-"volume"}
  if(doc_type==28){tableau$corpus="presse_fr_banq"
  tableau$search_mode<-"volume"}
  
  memoire<<-bind_rows(tableau,memoire)
  data = list(tableau,paste(mots,collapse="&"),resolution)
  names(data) = c("tableau","mot","resolution")
  return(data)}


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
  data=list(read.csv("exemple.csv",encoding = "UTF-8"),"Joffre&Pétain&Foch","Années")
  names(data)=c("tableau","mot","resolution")
  memoire<<-read.csv("exemple.csv",encoding="UTF-8")
  memoire$date<<-as.character(memoire$date)
  recherche_precedente<<-"Joffre&Pétain&Foch_1914_1920_Année"
  corpus_precedent<<-"1_1"
  
  observeEvent(input$doc_type,{observeEvent(input$search_mode,{
    if((input$doc_type == 1 & input$search_mode == 1)|(input$doc_type == 2 & input$search_mode == 1)|(input$doc_type == 3 & input$search_mode == 1)|input$doc_type == 5|input$doc_type == 6|input$doc_type == 7|input$doc_type == 8|input$doc_type == 9|input$doc_type == 10|input$doc_type == 11|input$doc_type == 12|input$doc_type == 15|input$doc_type == 16|input$doc_type == 19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26){
      output$instructions <- renderUI(HTML('<ul><li>Séparer les termes par un "&" pour une recherche multiple</li><li>Utiliser "a+b" pour rechercher a OU b</li><li>Cliquer sur un point du graphique pour accéder aux documents dans la bibliothèque numérique correspondante</li></ul>'))
      
    }else if(input$doc_type==13|input$doc_type==14|input$doc_type==17|input$doc_type==18 | input$doc_type == 27 | input$doc_type == 28){
      output$instructions <- renderUI(HTML('<ul><li>Séparer les termes par un "&" pour une recherche multiple</li><li>Cliquer sur un point du graphique pour accéder aux documents dans la bibliothèque numérique correspondante</li></ul>'))
    }
    else{output$instructions <- renderUI("")}
    })})
  
    
  indicator_file <- reactive({
    if(is.null(input$target_upload)) {return(0)}
    else{return(1)}
  })
  output$fileUploaded <- reactive({
    return(indicator_file())
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$legende1<-renderText(str_c("Corpus : presse\n"))
  
  observeEvent(input$doc_type,{observeEvent(input$filtre,{
    
    if(input$doc_type == 3 & input$filtre==1)
    {
      liste_themes<-read.csv("liste_themes.csv",encoding = "UTF-8")
      updateSelectizeInput(session,"theme_presse","Thématique",choices = setNames(as.character(liste_themes$num),as.character(liste_themes$titre)))
    }
    if(input$doc_type == 3 & input$filtre==2)
    {
      liste_departements<-read.csv("liste_departements.csv",encoding = "UTF-8")
      updateSelectizeInput(session,"theme_presse","Région",choices = setNames(as.character(liste_departements$num),as.character(liste_departements$titre)))
    }
  })})
  
  observeEvent(input$doc_type,{observeEvent(input$theme_presse,{
    if(input$doc_type == 3)
    {  
      if(input$theme_presse==1)
      {
        liste_journaux<-read.csv("liste_journaux.csv",encoding="UTF-8")
        output$titres<-renderUI({selectizeInput("titres","Titre des journaux",choices = setNames(liste_journaux$ark,liste_journaux$title),selected="cb39294634r",multiple=T)})
      }
      else if(as.integer(input$theme_presse) & as.integer(input$theme_presse)<=50)
      {
        themes<-read.csv("liste_themes.csv",encoding = "UTF-8")
        fichier<-as.character(themes$csv[themes$num==input$theme_presse])
        liste_journaux<-read.csv(fichier,encoding="UTF-8")
        liste_journaux$titre<-str_remove_all(liste_journaux$titre,"\n")
        output$titres<-renderUI({pickerInput("titres","Titre des journaux",choices = setNames(as.character(liste_journaux$ark),as.character(liste_journaux$titre)), options = list(`actions-box` = TRUE),multiple = T)})
      }
      else if(as.integer(input$theme_presse)>=51)
      {
        departement<-read.csv("liste_departements.csv",encoding = "UTF-8")
        fichier<-as.character(departement$csv[as.character(departement$num)==as.character(input$theme_presse)])
        liste_journaux<-read.csv(fichier,encoding="UTF-8")
        liste_journaux$titre<-str_remove_all(liste_journaux$titre,"\n")
        output$titres<-renderUI({pickerInput("titres","Titre des journaux",choices = setNames(as.character(liste_journaux$ark),as.character(liste_journaux$titre)), options = list(`actions-box` = TRUE),multiple = T)})
      }
    }
      
    })})
  
  
  output$plot <- renderPlotly({Plot(data,input)})
  output$corr<-renderTable(correlation_matrix(prepare_correlation(data),"corr1"),rownames = TRUE)
  output$pvalue=renderText("***p<.001 ; **p<.01 ; *p<.05")
  
  output$legende=renderText(HTML(paste("Source : ","<a href = 'https://gallica.bnf.fr/'> ","gallica.bnf.fr","</a>"),sep = ""))
  output$legende0=renderText("Affichage : Gallicagram par Benjamin Azoulay et Benoît de Courson")
  nb_mots<-length(unique(data[["tableau"]]$mot))
  output$legende2<-renderText(str_c("Documents épluchés : ",as.character(sum(data[["tableau"]]$base)/nb_mots)))
  output$legende3<-renderText(str_c("Résultats trouvés : ",as.character(sum(data[["tableau"]]$count))))
  
  recherche_texte<-reactive({input$mot})
  recherche_from<-reactive({input$beginning})
  recherche_to<-reactive({input$end})
  recherche_doc_type<-reactive({input$doc_type})
  recherche_titres<-reactive({input$titres})
  
  observeEvent(input$language,{
    observeEvent(input$bibli,{
    if(input$language == 1 & input$bibli==1){
      updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse française / Gallica" = 1,"Recherche par titre de presse / Gallica" = 3, "Livres / Gallica" = 2, "Corpus personnalisé / Gallica"=4),selected = 1)
    }
    else if(input$language == 1 & input$bibli==2){
      updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse wallonne / KBR"=13, "Presse québécoise / BAnQ"=28, "Livres / Ngram Viewer - Google Books" = 5),selected = 5)
    }
    else if(input$language == 1 & input$bibli==3){
      updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27),selected = 17)
    }
    else if(input$language == 2){
      updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse allemande / Europeana" = 6,"Presse suisse-allemande / Bibliothèque nationale suisse"=16 , "Livres / Ngram Viewer Allemand" = 9),selected = 6)
    }else if(input$language == 3){
      updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse flamande / KBR"=14, "Presse néerlandaise / Europeana" = 7),selected = 14)
    }else if(input$language == 4){
      updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse britannique / BNA" = 8, "Livres / Ngram Viewer Anglais" = 10),selected = 8)
    }else if(input$language == 5){
      updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12),selected = 11)
    }
  })})
  
  
  observeEvent(input$doc_type,{
    if(input$doc_type == 2){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1,"Par page" = 2),selected = 1)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if(input$doc_type == 4 | input$doc_type == 3){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1,"Par page" = 2),selected = 1)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if(input$doc_type == 1 | input$doc_type == 6 | input$doc_type == 7 | input$doc_type == 18 | input$doc_type == 19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27 | input$doc_type == 28){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1),selected = 1)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if(input$doc_type == 5 | input$doc_type == 9 | input$doc_type == 10 | input$doc_type == 12){
      updateSelectInput(session,"search_mode",choices = list("Par n-gramme" = 3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if(input$doc_type == 11){
      updateSelectInput(session,"search_mode",choices = list("Par page" = 2),selected = 2)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if(input$doc_type == 13 | input$doc_type == 14 | input$doc_type == 17){
      updateSelectInput(session,"search_mode",choices = list("Par page" = 2),selected = 2)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if(input$doc_type == 8 | input$doc_type == 15 | input$doc_type == 16){
      updateSelectInput(session,"search_mode",choices = list("Par article" = 4),selected = 4)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    })
  
  
  observeEvent(
    input$search_mode,{
      if(input$search_mode==2 & (input$doc_type ==1 |input$doc_type ==2 |input$doc_type == 3 |input$doc_type ==4)){
        output$avertissement<-renderText(message(recherche_texte(),recherche_from(),recherche_to(),recherche_doc_type(),recherche_titres()))
    }
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
    if (input$doc_type==1 |(input$doc_type == 3 & input$search_mode==1) | input$doc_type==5 | (input$doc_type==2 & input$search_mode==1) | input$doc_type==6 | input$doc_type==7 | input$doc_type==8 | input$doc_type == 9 | input$doc_type == 10 | input$doc_type == 11 | input$doc_type == 12 | input$doc_type == 13 | input$doc_type == 14 | input$doc_type == 15 | input$doc_type == 16 | input$doc_type == 17 | input$doc_type == 18 | input$doc_type == 19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27  | input$doc_type == 28){
      df = get_data(input$mot,input$beginning,input$end,input$resolution,input$doc_type,input$titres)}
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
      df=page_search(input$mot,input$beginning,input$end,input$resolution,tot_df,input$doc_type,input$search_mode,input$titres)
    }
    else if( (input$doc_type==2 | input$doc_type == 3) & input$search_mode==2){
      df=rapport(input$mot,input$beginning,input$end,input$doc_type,input$titres)
      df$identifier<-str_remove_all(df$identifier," .+")
      df=page_search(input$mot,input$beginning,input$end,input$resolution,df,input$doc_type,input$search_mode,input$titres)
    }
    
    output$plot <- renderPlotly({Plot(df,input)})
    
    if(input$doc_type==1 | (input$doc_type==2 & input$search_mode==1) | (input$doc_type == 3 & input$search_mode==1) | input$doc_type==6 | input$doc_type==7 | input$doc_type == 18 | input$doc_type == 19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27  | input$doc_type == 28){
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2<-renderText(str_c("Documents épluchés : ",as.character(sum(df[["tableau"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Résultats trouvés : ", as.character(sum(df[["tableau"]]$count))))
    }
    else if(input$doc_type==4 & input$search_mode==1){
      nb_mots<-length(unique(df[["tableau_volume"]]$mot))
      output$legende2<-renderText(str_c("Documents épluchées : ", as.character(sum(df[["tableau_volume"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Résultats trouvés : ", as.character(sum(df[["tableau_volume"]]$count))))
    }
    else if (input$doc_type==5 | input$doc_type==9 | input$doc_type==10 | input$doc_type==12){
      output$legende2<-NULL
      output$legende3<-NULL
    }
    else if (input$doc_type==11 | input$doc_type==13 | input$doc_type==14 | input$doc_type==17) {
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2<-renderText(str_c("Pages épluchées : ", as.character(sum(df[["tableau"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Pages correspondant à la recherche : ", as.character(sum(df[["tableau"]]$count))))
    }
    else if (input$doc_type==8 | input$doc_type==15 | input$doc_type==16) {
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2<-renderText(str_c("Articles épluchés : ", as.character(sum(df[["tableau"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Articles correspondant à la recherche : ", as.character(sum(df[["tableau"]]$count))))
    }
    else {
      nb_mots<-length(unique(df[["tableau_page"]]$mot))
      output$legende2<-renderText(str_c("Pages épluchées : ", as.character(sum(df[["tableau_page"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Pages correspondant à la recherche : ", as.character(sum(df[["tableau_page"]]$count))))
    }
    
    if(input$doc_type==1 | input$doc_type==2 | (input$doc_type == 3 & input$theme_presse==1) | input$doc_type==4){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://gallica.bnf.fr/'> ","gallica.bnf.fr","</a>"),sep = ""))}
    if(input$doc_type==5 | input$doc_type==9 | input$doc_type==10 | input$doc_type==12){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://books.google.com/ngrams/'> ","books.google.com/ngrams","</a>"),sep = ""))}
    if(input$doc_type==6 | input$doc_type==7){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.europeana.eu/'> ","europeana.eu","</a>"),sep = ""))}
    if(input$doc_type==8){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.britishnewspaperarchive.co.uk/'> ","britishnewspaperarchive.co.uk","</a>"),sep = ""))}
    if(input$doc_type==11){output$legende=renderText(HTML(paste("Source : ","<a href = 'http://www.hemerotecadigital.bne.es/'> ","hemerotecadigital.bne.es","</a>"),sep = ""))}
    if(input$doc_type==13 | input$doc_type==14){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.belgicapress.be/'> ","belgicapress.be","</a>"),sep = ""))}
    if(input$doc_type==15 | input$doc_type==16){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.e-newspaperarchives.ch/'> ","e-newspaperarchives.ch","</a>"),sep = ""))}
    if(input$doc_type==17){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.lectura.plus/Presse/'> ","lectura.plus/Presse","</a>"),sep = ""))}
    if(input$doc_type==18){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://kiosque.limedia.fr/'> ","kiosque.limedia.fr","</a>"),sep = ""))}
    if(input$doc_type==19){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://memonum-mediatheques.montpellier3m.fr/'> ","memonum-mediatheques.montpellier3m.fr","</a>"),sep = ""))}
    if(input$doc_type==20){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.communpatrimoine.fr/'> ","communpatrimoine.fr","</a>"),sep = ""))}
    if(input$doc_type==21){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://yroise.biblio.brest.fr/'> ","yroise.biblio.brest.fr","</a>"),sep = ""))}
    if(input$doc_type==22){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.pireneas.fr/'> ","pireneas.fr","</a>"),sep = ""))}
    if(input$doc_type==23){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://rosalis.bibliotheque.toulouse.fr/'> ","rosalis.bibliotheque.toulouse.fr","</a>"),sep = ""))}
    if(input$doc_type==24){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://bibliotheque-numerique.diplomatie.gouv.fr/'> ","bibliotheque-numerique.diplomatie.gouv.fr","</a>"),sep = ""))}
    if(input$doc_type==25){output$legende=renderText(HTML(paste("Source : ","<a href = 'http://www.rfnum-bibliotheque.org/'> ","rfnum-bibliotheque.org","</a>"),sep = ""))}
    if(input$doc_type==26){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.numistral.fr/'> ","numistral.fr","</a>"),sep = ""))}
    if(input$doc_type==27){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.bn-r.fr/'> ","bn-r.fr","</a>"),sep = ""))}
    if(input$doc_type==28){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.banq.qc.ca/'> ","banq.qc.ca","</a>"),sep = ""))}
    if(input$doc_type == 3 & input$theme_presse != 1){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://gallica.bnf.fr/html/und/presse-et-revues/presse-et-revues'> ","gallica.bnf.fr","</a>"),sep = ""))}

    if(input$doc_type==1 | input$doc_type==2 | input$doc_type == 3 | input$doc_type==4 | input$doc_type==5 | input$doc_type==13 | input$doc_type==15 | input$doc_type==17 | input$doc_type==18 | input$doc_type==19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27 | input$doc_type == 28){output$legende4=renderText("Langue : français")}
    if(input$doc_type==6 | input$doc_type==9 | input$doc_type==16){output$legende4=renderText("Langue : allemand")}
    if(input$doc_type==7 | input$doc_type==14){output$legende4=renderText("Langue : néerlandais")}
    if(input$doc_type==8 | input$doc_type==10){output$legende4=renderText("Langue : anglais")}
    if(input$doc_type==11 | input$doc_type==12){output$legende4=renderText("Langue : espagnol")}
    
    if(input$doc_type==1 | input$doc_type==6 | input$doc_type==7 | input$doc_type==8 | input$doc_type==11 | input$doc_type==13 | input$doc_type==14 | input$doc_type==15 | input$doc_type==16 | input$doc_type==17 | input$doc_type==18 | input$doc_type==19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26  | input$doc_type == 27 | input$doc_type == 28){output$legende1<-renderText("Corpus : presse")}
    if(input$doc_type==2 | input$doc_type==5 | input$doc_type==9 | input$doc_type==10 | input$doc_type==12){output$legende1<-renderText("Corpus : livres")}
    if(input$doc_type==4){output$legende1<-renderText("Corpus : personnalisé")}
    if(input$doc_type == 3 & input$theme_presse == 1){
      liste_journaux<-read.csv("liste_journaux.csv",encoding="UTF-8")
      title<-liste_journaux$title[liste_journaux$ark==input$titres[1]]
      title=str_c("Corpus : ",title)
      if(length(input$titres)>=2){
        for (i in 2:length(input$titres)) {
          title<-str_c(title," + ",liste_journaux$title[liste_journaux$ark==input$titres[i]])
        }}
      output$legende1<-renderText(paste(title))
    }
    if(input$doc_type == 3 & as.integer(input$theme_presse>=2) & as.integer(input$theme_presse<=50)){
      liste_themes<-read.csv("liste_themes.csv",encoding="UTF-8")
      title<-liste_themes$titre[as.character(liste_themes$num)==as.character(input$theme_presse)]
      output$legende1<-renderText(str_c("Corpus : ",title))
    }
    if(input$doc_type == 3 & as.integer(input$theme_presse>=51)){
      liste_departements<-read.csv("liste_departements.csv",encoding="UTF-8")
      title<-liste_departements$titre[as.character(liste_departements$num)==as.character(input$theme_presse)]
      output$legende1<-renderText(str_c("Corpus : ",title))
    }

    
    if(str_detect(input$mot,".+&.+"))
    {output$corr<-renderTable(correlation_matrix(prepare_correlation(df),"corr1"),rownames = TRUE)}
    else{output$corr<-renderTable(as.matrix(NA),colnames = FALSE)}
    
    if(recherche_precedente==str_c(input$mot,"_",input$beginning,"_",input$end,"_",input$resolution)&corpus_precedent!=str_c(input$doc_type,"_",input$search_mode)){
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
    corpus_precedent<<-str_c(input$doc_type,"_",input$search_mode)

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("gallicagram_",input$mot,"_",input$beginning,"_",input$end,'.csv', sep='')
      },
      content = function(con) {
        if((input$doc_type==2 | input$doc_type == 3 | input$doc_type==4) & input$search_mode==2){write.csv(df$tableau_page, con,row.names = F,fileEncoding = "UTF-8")}
        else if(input$search_mode==1 & input$doc_type==4){write.csv(df$tableau_volume, con,row.names = F,fileEncoding = "UTF-8")}
        else{write.csv(df$tableau, con,row.names = F,fileEncoding = "UTF-8")}
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
        x <- list(title = "Date",titlefont = 41,range=c("1500","2021"))
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
        
        for (i in 2:length(table$base)) {
          table$base[i]<-table$base[i]+table$base[i-1]
        }
        table$base=table$base/table$base[length(table$base)]
        
        plot2<-plot_ly(table, x=~date,y=~base,type='bar')
        Title = paste("<b>Distribution chronologique du corpus de presse en français\nde Gallica<b>")
        y <- list(title = "Proportion du corpus publié\navant la date indiquée en abscisse",titlefont = 41,tickformat=".1%")
        x <- list(title = "Date",titlefont = 41,range=c("1500","2021"))
        
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
        table$hovers = str_c(table$date,": N = ",table$base)
        plot2<-plot_ly(table, x=~date,y=~base,text=~hovers,type='bar',hoverinfo="text")
        Title = paste("<a href = 'https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22)%20and%20(gallicapublication_date%3E=%221380%22%20and%20gallicapublication_date%3C=%222021%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords='> <b>Répartition des ",somme," livres en français océrisés\ndans Gallica<b> </a>")
        y <- list(title = "Nombre de livres dans Gallica",titlefont = 41)
        x <- list(title = "Date",titlefont = 41,range=c("1500","2021"))
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_l==1 & input$corpus_ngram_l==TRUE){
        ngram<-read.csv("ngram_viewer_fre_20200217.csv",encoding = "UTF-8")
        total_volume_count<-sum(ngram$volume_count)
        plot2<-plot_ly(ngram, x=~year,y=~volume_count,type='bar')
        Title = paste("<b>Répartition des ",total_volume_count," livres océrisés et en français\nexploités dans"," <a href = 'https://books.google.com/ngrams/graph?content=Joffre%2CP%C3%A9tain%2CFoch&year_start=1914&year_end=1920&corpus=30&smoothing=0&direct_url=t1%3B%2CJoffre%3B%2Cc0%3B.t1%3B%2CP%C3%A9tain%3B%2Cc0%3B.t1%3B%2CFoch%3B%2Cc0'>Google Ngram Viewer</a><b>")
        y <- list(title = "Nombre de livres exploités dans Ngram Viewer",titlefont = 41)
        x <- list(title = "Date",titlefont = 41,range=c("1500","2021"))
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
        p_pages_livres<-read.csv("base_pages_livres_annees.csv",encoding = "UTF-8")
        plot7<-plot_ly(p_pages_livres,x=~date,y=~count,type='bar',colors="Dark2")
        plot7<-layout(plot7, title="Volume du corpus de livres de Gallica", xaxis=list(title="Date",tickangle="-45",range=c("1500","2021")),yaxis=list(title="Nombre de pages dans le corpus"),barmode="stack")
        return(plot7)
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
        for (i in 2:length(table$base)) {
          table$base[i]<-table$base[i]+table$base[i-1]
        }
        table$base=table$base/table$base[length(table$base)]
        
        plot2<-plot_ly(table, x=~date,y=~base,type='bar')
        Title = paste("<b>Distribution chronologique du corpus de livres en français\nde Gallica<b>")
        y <- list(title = "Proportion du corpus publié\navant la date indiquée en abscisse",titlefont = 41,tickformat=".1%")
        x <- list(title = "Date",titlefont = 41,range=c("1500","2021"))
        plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
        return(plot2)
      }
      else if(input$corpus_structure_l==1 & input$corpus_ngram_l==TRUE){
        ngram<-read.csv("ngram_viewer_fre_20200217.csv",encoding = "UTF-8")
        total_volume_count<-sum(ngram$volume_count)
        for (i in 2:length(ngram$volume_count)) {
          ngram$volume_count[i]<-ngram$volume_count[i]+ngram$volume_count[i-1]
        }
        ngram$volume_count=ngram$volume_count/ngram$volume_count[length(ngram$volume_count)]
        plot2<-plot_ly(ngram, x=~year,y=~volume_count,type='bar')
        Title = paste("<b>Distribution chronologique du corpus exploité\npar Google Ngram Viewer<b>")
        y <- list(title = "Proportion du corpus publié\navant la date indiquée en abscisse",titlefont = 41,tickformat=".1%")
        x <- list(title = "Date",titlefont = 41,range=c("1500","2021"))
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
        p_pages_livres<-read.csv("base_pages_livres_annees.csv",encoding = "UTF-8")
        plot7<-plot_ly(p_pages_livres,x=~date,y=~count,type='bar',colors="Dark2")
        plot7<-layout(plot7, title="Volume du corpus de livres de Gallica", xaxis=list(title="Date",tickangle="-45",range=c("1500","2021")),yaxis=list(title="Nombre de pages dans le corpus"),barmode="stack")
        return(plot7)
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
    if (input$corpus_structure_l==5){
      
      p_pages_livres<-read.csv("p_pages_livres.csv",encoding = "UTF-8")
      plot7<-plot_ly(p_pages_livres,x=~date,y=~Mean,type='bar',colors="Dark2")
      plot7<-layout(plot7, title="Distribution des livres en français \nselon leur volume (nombre de pages moyen)", xaxis=list(title="Date",tickangle="-45",range=c("1500","2021")),yaxis=list(title="Nombre de pages"),barmode="stack")
      output$corpus4<-renderPlotly(plot7)
      plot6<-plot_ly(p_pages_livres,x=~date,y=~Median,type='bar',colors="Dark2")
      plot6<-layout(plot6, title="Distribution des livres en français \nselon leur volume (nombre de pages médian)", xaxis=list(title="Date",tickangle="-45",range=c("1500","2021")),yaxis=list(title="Nombre de pages"),barmode="stack")
      output$corpus5<-renderPlotly(plot6)
    }
    
  })})
  
  shinyOptions(progress.style="old")
})