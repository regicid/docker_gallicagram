library(stringr)
library(xml2)
library(rvest)
library(RSelenium)

rD <- rsDriver(browser="firefox", port=4547L, verbose=F)
remDr <- rD[["client"]]

from="1814"
to="1970"
resolution="Mois"
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
      end = str_c(y,"-",z,"-",end_of_month[j])}
    url_base<-str_c("https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=&none_q=&from_d=",beginning,"&to_d=",end,"&per_lang=fr&per=&lang=FR&per_type=1")
    
    remDr$navigate(url_base)
    
    Sys.sleep(2) # give the page time to fully load
    ngram_base <- remDr$getPageSource()[[1]]
    ngram_base<-str_extract(ngram_base,"foundnumber.+")
    ngram_base<-str_remove_all(ngram_base,"[:punct:]")
    b<-as.integer(str_extract_all(ngram_base,"[:digit:]+"))
    tableau[nrow(tableau)+1,] = NA
    date=y
    if(resolution=="Mois"){date = paste(y,z,sep="-")}
    tableau[nrow(tableau),]<-c(date,b)
    print(date)
  }
  
  
}
remDr$close()
rD$server$stop()

colnames(tableau)<-c("date","base")
tableau$base[is.na(tableau$base)]<-0
tableau$base<-as.integer(tableau$base)
write.csv(tableau,'C:/Users/Benjamin/gallicagram_app/base_presse_mois_kbr_fr.csv',fileEncoding = "UTF-8",row.names = FALSE)  
