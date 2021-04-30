library(stringr)
library(xml2)
library(httr)
library(rvest)
from="1769"
to="1950"
resolution="Année"
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)


for (i in from:to){
  
  
  end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if( i%%4==0){end_of_month[2]=29}
  if(i==1700 | i==1800 | i==1900){end_of_month[2]=28}#Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
  y<-as.character(i)
  if(resolution=="Année"){beginning = str_c(y,"-01-01")
  end = str_c(y,"-12-31")}
  I = 1
  if(resolution=="Mois"){I=1:12} #Pour faire ensuite une boucle sur les mois
  
  for(j in I){
    if(resolution=="Mois"){
      z = as.character(j)
      if(nchar(z)<2){z<-str_c("0",z)}
      beginning = str_c(y,"-",z,"-01")
      end = str_c(y,"-",z,"-",end_of_month[j])
      url_base<-str_c("https://kiosque.limedia.fr/recherche/?query=&search_type=or&uniform_title=&date=&period_start=01/",z,"/",y,"&period_end=",end_of_month[j],"/",z,"/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")}
    if(resolution=="Année"){url_base<-str_c("https://kiosque.limedia.fr/recherche/?query=&search_type=or&uniform_title=&date=&period_start=01/01/",y,"&period_end=31/12/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")}
    ngram_base<-read_html(RETRY("GET",url_base,times = 6))
    b<-str_extract(html_text(html_node(ngram_base,".col-milieu")),"[:digit:]+")
    tableau[nrow(tableau)+1,] = NA
    date=y
    if(resolution=="Mois"){date = paste(y,z,sep="-")}
    tableau[nrow(tableau),]<-c(date,b)
    print(date)
  }
  
  
}

colnames(tableau)<-c("date","base")
tableau$date<-str_replace_all(tableau$date,"-","/")
tableau$base[is.na(tableau$base)]<-0
tableau$base<-as.integer(tableau$base)
write.csv(tableau,'C:/Users/Benjamin/gallicagram_app/base_presse_annees_limedia_fr.csv',fileEncoding = "UTF-8",row.names = FALSE)  
