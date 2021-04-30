###Extraction presse/années
library(stringr)
library(xml2)
library(rvest)

mot="cochon"
from="1794"
to="2021"
resolution="Mois"
doc_type=1

mots = str_split(mot,"&")[[1]]
tableau<-as.data.frame(matrix(nrow=0,ncol=4),stringsAsFactors = FALSE)

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
      mot1<-mots_or[1]}
    else{mot1=mot2}
    ###
    end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
    if( i%%4==0){end_of_month[2]=29}
    if(i==1700 | i==1800 | i==1900){end_of_month[2]=28}#Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
    y<-as.character(i)
    if(resolution=="Année"){beginning = str_c(y,"/01/01")
    end = str_c(y,"/12/31")}
    I = 1
    if(resolution=="Mois"){I=1:12} #Pour faire ensuite une boucle sur les mois
    
    
    if(doc_type==1){
      for(j in I){
        if(resolution=="Mois"){
          z = as.character(j)
          if(nchar(z)<2){z<-str_c("0",z)}
          beginning = str_c(y,"/",z,"/01")
          end = str_c(y,"/",z,"/",end_of_month[j])}
        # url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
        # ngram<-as.character(read_xml(url))
        # a<-str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
        a=0
        url_base<-str_c("https://www.numistral.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
        ngram_base<-read_html(url_base)
        ngram_base<-html_text(html_node(ngram_base,"head > title:nth-child(3)"))
        ngram_base<- str_remove_all(ngram_base,"[:space:]")
        b<-str_extract(ngram_base,"[:digit:]+")
        tableau[nrow(tableau)+1,] = NA
        date=y
        if(resolution=="Mois"){date = paste(y,z,sep="/")}
        tableau[nrow(tableau),]<-c(date,a,b,mot)
        print(date)
      }}
    
  }
}
colnames(tableau)<-c("date","nb_temp","base","mot")
format = "%Y"
if(resolution=="Mois"){format=paste(format,"%m",sep="/")}
tableau.date = as.Date(as.character(tableau$date),format=format)
tableau$nb_temp<-as.integer(tableau$nb_temp)
tableau$base<-as.integer(tableau$base)
tableau$ratio_temp<-tableau$nb_temp/tableau$base
tableau$ratio_temp[is.na(tableau$ratio_temp)]<-0

tableau<-tableau[,-2]
tableau<-tableau[,-3]
tableau<-tableau[,-3]  

write.csv(tableau,'C:/Users/Benjamin/gallicagram_app/base_presse_mois_numistral_fr.csv',fileEncoding = "UTF-8",row.names = FALSE)  
