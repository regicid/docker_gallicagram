library(stringr)
library(xml2)
library(httr)

from="1770"
to="1970"
resolution="Année"
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)


for (i in from:to){
  
  
  end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if( i%%4==0){end_of_month[2]=29} 
  y<-as.character(i)
  if(resolution=="Année"){beginning = str_c("01/01/",y)
  end = str_c("12/31/",y)}
  I = 1
  if(resolution=="Mois"){I=1:12}
  
  for(j in I){
    if(resolution=="Mois"){
      z = as.character(j)
      if(nchar(z)<2){z<-str_c("0",z)}
      beginning = str_c(z,"/01/",y)
      end = str_c(z,"/",end_of_month[j],"/",y)}
    url_base<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?date1=",beginning,"&date2=",end,"&searchType=advanced&language=eng&state=&rows=1&ortext=&proxtext=&phrasetext=&andtext=&dateFilterType=range&page=1&sort=date&format=json")
    ngram<-as.character(read_html(RETRY("GET",url_base,times = 3)))
    ngram<-str_remove_all(ngram,"[:punct:]")
    ngram<-str_remove_all(ngram,"[:space:]")
    b<-str_extract(str_extract(ngram,"totalItems[:digit:]+"),"[:digit:]+")
    tableau[nrow(tableau)+1,] = NA
    date=y
    if(resolution=="Mois"){date = paste(y,z,sep="/")}
    tableau[nrow(tableau),]<-c(date,b)
    print(date)
  }
  
  
}

colnames(tableau)<-c("date","base")
tableau$base[is.na(tableau$base)]<-0
tableau$base<-as.integer(tableau$base)
write.csv(tableau,'C:/Users/Benjamin/gallicagram_app/base_presse_annees_loc_en.csv',fileEncoding = "UTF-8",row.names = FALSE)  
