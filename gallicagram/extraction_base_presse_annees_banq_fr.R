library(stringr)
library(xml2)
library(rvest)
library(RSelenium)

rD <- rsDriver(browser="firefox", port=4546L, verbose=F)
remDr <- rD[["client"]]

from="1800"
to="2021"
resolution="Année"
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)

url_base="https://numerique.banq.qc.ca/rechercheExterne/encoded/Kg==/false/T/asc/W3sibm9tIjoiY29ycHVzIiwidmFsZXVyIjoiUGF0cmltb2luZSUyMHF1w6liw6ljb2lzIn0seyJub20iOiJ0eXBlX2RvY19mIiwidmFsZXVyIjoiUmV2dWVzJTIwZXQlMjBqb3VybmF1eCJ9LHsibm9tIjoibGFuZ3Vlc19jb250ZW51IiwidmFsZXVyIjoiZnJhbsOnYWlzIn0seyJub20iOiJhdmVjX3RleHRlX2ludGVncmFsIiwidmFsZXVyIjoib3VpIn0seyJub20iOiJnZW5yZV9mIiwidmFsZXVyIjoiSm91cm5hdXgifV0=/Liste%20de%20r%C3%A9sultats/true/false/eyJkZWJ1dCI6eyJhbm5lZSI6MTkzMywibW9pcyI6MSwiam91ciI6MX0sImZpbiI6eyJhbm5lZSI6MTkzMywibW9pcyI6MTIsImpvdXIiOjMxfX0="
remDr$navigate(url_base) #on navigue vers la premiere url pour initialiser notre merdier


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
      Sys.sleep(2)
      page <- read_html(remDr$getPageSource()[[1]])
      b<-html_text(html_node(page,".chapoNbResultat"))
      b<-str_extract(b,"[:digit:]+")
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
      Sys.sleep(2)
      page <- read_html(remDr$getPageSource()[[1]])
      b<-html_text(html_node(page,".chapoNbResultat"))
      b<-str_extract(b,"[:digit:]+")
      webElem <- remDr$findElement(using = 'css selector',"#dateInterval")
      webElem$clickElement()
      Sys.sleep(1)
      webElem <- remDr$findElement(using = 'css selector',"facet-filter.ng-isolate-scope > div:nth-child(10) > ul:nth-child(1) > li:nth-child(1) > span:nth-child(1) > i:nth-child(1)")
      webElem$clickElement()
    }
    
    
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
write.csv(tableau,'C:/Users/Benjamin/gallicagram_app/base_presse_annees_banq_fr.csv',fileEncoding = "UTF-8",row.names = FALSE)  
