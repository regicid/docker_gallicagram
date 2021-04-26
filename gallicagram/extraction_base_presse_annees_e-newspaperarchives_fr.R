library(stringr)
library(xml2)
library(httr)

from="1692"
to="2018"
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
      url_base<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=la+OR+le+OR+un+OR+et+OR+une+OR+des+OR+les+OR+ou+OR+donc&dafdq=01&dafmq=",z,"&dafyq=",y,"&datdq=",end_of_month[j],"&datmq=",z,"&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr")
    }
    if(resolution=="Année"){
      url_base<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=la+OR+le+OR+un+OR+et+OR+une+OR+des+OR+les+OR+ou+OR+donc&dafdq=01&dafmq=01&dafyq=",y,"&datdq=31&datmq=12&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr")
      
    }
    ngram_base<-as.character(read_html(RETRY("GET",url_base,times = 3)))
    ngram_base<-str_remove_all(ngram_base,",")
    ngram_base<-str_extract(ngram_base,"ARTICLE</a><span.+")
    b<-str_extract(ngram_base,"[:digit:]+")
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
write.csv(tableau,'C:/Users/Benjamin/gallicagram_app/base_presse_annees_e-newspaperarchives_fr.csv',fileEncoding = "UTF-8",row.names = FALSE)  


