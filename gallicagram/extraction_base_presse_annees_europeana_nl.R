library(stringr)
library(xml2)

from="1600"
to="2021"
resolution="Année"
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)


for (i in from:to){
  
  
  end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if( i%%4==0){end_of_month[2]=29} #Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
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
      end = str_c(y,"-",z,"-",end_of_month[j])}
    url_base<-str_c("https://www.europeana.eu/fr/search?page=1&qf=MEDIA%3Atrue&qf=TYPE%3A%22TEXT%22&qf=LANGUAGE%3A%22nl%22&qf=collection%3Anewspaper&qf=proxy_dcterms_issued%3A%5B",beginning,"%20TO%20",end,"%5D&view=grid&api=fulltext")
    ngram_base<-as.character(read_html(url_base))
    ngram_base<-str_remove_all(ngram_base,",")
    b<-str_extract(str_extract(ngram_base,"Résultats: [:digit:]+"),"[:digit:]+")
    tableau[nrow(tableau)+1,] = NA
    date=y
    if(resolution=="Mois"){date = paste(y,z,sep="-")}
    tableau[nrow(tableau),]<-c(date,b)
    print(date)
  }
  
  
}

colnames(tableau)<-c("date","base")
tableau$base[is.na(tableau$base)]<-0
tableau$base<-as.integer(tableau$base)
write.csv(tableau,'C:/Users/Benjamin/gallicagram_app/base_presse_annees_europeana_nl.csv',fileEncoding = "UTF-8",row.names = FALSE)  
