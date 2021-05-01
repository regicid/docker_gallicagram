library(stringr)
library(rvest)
library(xml2)
library(httr)
library(dplyr)

#Principaux quotidiens
url="https://gallica.bnf.fr/html/presse-et-revues/les-principaux-quotidiens?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_principaux-quotidiens.csv",fileEncoding = "UTF-8",row.names = F)

#Hebdomadaires
url="https://gallica.bnf.fr/html/und/presse-et-revues/les-hebdomadaires?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_hebdomadaires.csv",fileEncoding = "UTF-8",row.names = F)

#Journaux de tranchées
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-b?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-c?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-d-e?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-f-g?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-h-o?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-p?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-q-s?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-t-z?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_feuilles-de-tranchee.csv",fileEncoding = "UTF-8",row.names = F)


#principaux titres résistance
url="https://gallica.bnf.fr/html/und/presse-et-revues/principaux-titres-de-presse-clandestine-de-la-resistance?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-principaux-titres.csv",fileEncoding = "UTF-8",row.names = F)

#Resistance zone sud
url="https://gallica.bnf.fr/html/und/presse-et-revues/journaux-clandestins-de-la-zone-sud?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-zone-sud.csv",fileEncoding = "UTF-8",row.names = F)

#Resistance zone interdite et réservée
url="https://gallica.bnf.fr/html/und/presse-et-revues/journaux-clandestins-de-la-zone-reservee?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-zone-interdite-et-reservee.csv",fileEncoding = "UTF-8",row.names = F)

#Resistance alsace moselle
url="https://gallica.bnf.fr/html/und/presse-et-revues/journaux-clandestins-en-alsace-moselle?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-zone-alsace-moselle.csv",fileEncoding = "UTF-8",row.names = F)

#Resistance empire français
url="https://gallica.bnf.fr/html/und/presse-et-revues/journaux-clandestins-de-lempire-francais?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-empire-francais.csv",fileEncoding = "UTF-8",row.names = F)

#Resistance region parisienne
url="https://gallica.bnf.fr/html/und/presse-et-revues/journaux-clandestins-de-la-region-parisienne?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-region-parisienne.csv",fileEncoding = "UTF-8",row.names = F)

#Resistance zone nord
url="https://gallica.bnf.fr/html/und/presse-et-revues/journaux-clandestins-de-la-zone-nord?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
tableau$ark<-str_remove(tableau$ark,'[:punct:].+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-zone-nord.csv",fileEncoding = "UTF-8",row.names = F)


#Anciens combattants
url="https://gallica.bnf.fr/html/presse-et-revues/anciens-combattants?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_anciens-combattants.csv",fileEncoding = "UTF-8",row.names = F)

#Faits divers
url="https://gallica.bnf.fr/html/und/presse-et-revues/faits-divers?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_faits-divers.csv",fileEncoding = "UTF-8",row.names = F)

#Presse artistique
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-artistique?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-artistique.csv",fileEncoding = "UTF-8",row.names = F)

#Presse coloniale
url="https://gallica.bnf.fr/html/presse-et-revues/presse-coloniale?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-coloniale.csv",fileEncoding = "UTF-8",row.names = F)

#Presse culinaire
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-culinaire?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-culinaire.csv",fileEncoding = "UTF-8",row.names = F)


#Presse d'annonce
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-dannonces?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-d'annonces.csv",fileEncoding = "UTF-8",row.names = F)

#Presse de cinéma
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-de-cinema?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-de-cinema.csv",fileEncoding = "UTF-8",row.names = F)

#Presse de loisir
url="https://gallica.bnf.fr/html/presse-et-revues/presse-de-loisirs?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-de-loisirs.csv",fileEncoding = "UTF-8",row.names = F)

#Presse de mode
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-de-mode?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-de-mode.csv",fileEncoding = "UTF-8",row.names = F)

#Presse de musique
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-de-musique?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-de-musique.csv",fileEncoding = "UTF-8",row.names = F)


#Presse de spectacles
url="https://gallica.bnf.fr/html/presse-et-revues/presse-de-spectacles?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-de-spectacles.csv",fileEncoding = "UTF-8",row.names = F)

#Presse des immigrations
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-des-immigrations?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-des-immigrations.csv",fileEncoding = "UTF-8",row.names = F)

#Presse économique
url="https://gallica.bnf.fr/html/presse-et-revues/presse-economique"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-economique.csv",fileEncoding = "UTF-8",row.names = F)

#Presse enfantine
url="https://gallica.bnf.fr/html/presse-et-revues/presse-enfantine?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-enfantine.csv",fileEncoding = "UTF-8",row.names = F)

#Journaux d'entreprise
url="https://gallica.bnf.fr/html/und/presse-et-revues/journaux-dentreprises?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_journaux-d'entreprise.csv",fileEncoding = "UTF-8",row.names = F)

#Presse financière
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-financiere?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-financiere.csv",fileEncoding = "UTF-8",row.names = F)

#Presse par secteurs d'activité
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-par-metiers-et-secteurs?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-par-secteur-d'activites.csv",fileEncoding = "UTF-8",row.names = F)

#Annuaires professionnels
url="https://gallica.bnf.fr/html/und/presse-et-revues/annuaires-professionnels?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_annuaires-professionnels.csv",fileEncoding = "UTF-8",row.names = F)

#Presse féminine
url="https://gallica.bnf.fr/html/presse-et-revues/presse-feminine?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-feminine.csv",fileEncoding = "UTF-8",row.names = F)

#Presse féministe
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-feministe?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-feministe.csv",fileEncoding = "UTF-8",row.names = F)

#Presse littéraire
url="https://gallica.bnf.fr/html/presse-et-revues/presse-litteraire?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-litteraire.csv",fileEncoding = "UTF-8",row.names = F)

#Presse médicale
url="https://gallica.bnf.fr/html/und/sciences/presse-medicale?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-medicale.csv",fileEncoding = "UTF-8",row.names = F)

#Presse ouvrière
url="https://gallica.bnf.fr/html/presse-et-revues/presse-ouvriere?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-ouvriere.csv",fileEncoding = "UTF-8",row.names = F)

#Presse politique-revolution
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-durant-la-revolution-et-lempire?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-politique-revolution-empire.csv",fileEncoding = "UTF-8",row.names = F)

#Presse restauration-second empire
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-de-la-restauration-au-second-empire?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-politique-restauration-second-empire.csv",fileEncoding = "UTF-8",row.names = F)

#Presse 3e rép
url="https://gallica.bnf.fr/html/und/presse-et-revues/presse-de-la-iiie-republique-1870-1940?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-3e-republique.csv",fileEncoding = "UTF-8",row.names = F)

#Presse professionnelle
url="https://gallica.bnf.fr/html/presse-et-revues/presse-professionnelle?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-professionnelle.csv",fileEncoding = "UTF-8",row.names = F)

#Presse religieuse
url="https://gallica.bnf.fr/html/presse-et-revues/presse-religieuse?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-religieuse.csv",fileEncoding = "UTF-8",row.names = F)

#Presse satirique
url="https://gallica.bnf.fr/html/presse-et-revues/presse-satirique?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-satirique.csv",fileEncoding = "UTF-8",row.names = F)

#Presse scientifique
url="https://gallica.bnf.fr/html/sciences/presse-scientifique?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-scientifique.csv",fileEncoding = "UTF-8",row.names = F)

#Presse sportive
url="https://gallica.bnf.fr/html/presse-et-revues/presse-sportive?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-sportive.csv",fileEncoding = "UTF-8",row.names = F)

#Presse syndicale
url="https://gallica.bnf.fr/html/presse-et-revues/presse-syndicale?mode=desktop"
page<-read_html(url)
page<-html_node(page,".mosaic")
nb<-str_count(page,"mosaic-item")
tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
  titre<-str_remove_all(titre,"\t")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
tableau$ark<-str_remove(tableau$ark,'%20')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-syndicale.csv",fileEncoding = "UTF-8",row.names = F)

######################
liste_departements<-read.csv("C:/Users/Benjamin/gallicagram_app/liste_departements.csv",encoding = "UTF-8")
liste_departements<-liste_departements[order(liste_departements$titre),]
rownames(liste_departements)=NULL
for (j in 1:length(liste_departements$titre)) {
  url=liste_departements$url[j]
  page<-read_html(url)
  page<-html_node(page,".mosaic")
  nb<-str_count(page,"mosaic-item")
  tableau<-as.data.frame(matrix(nrow=0,ncol=2),stringsAsFactors = FALSE)
  for (i in 1:nb) {
    a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
    ark<-str_extract(a,"ark:/12148/.+")
    ark<-str_remove(ark,"ark:/12148/")
    ark<-str_remove(ark,"/.+")
    titre<-html_text(html_node(a,"div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"))
    titre<-str_remove_all(titre,"\t")
    tableau<-rbind(tableau,cbind(ark,titre))
  }
  tableau<-tableau[is.na(tableau$ark)==FALSE,]
  tableau$ark<-str_remove(tableau$ark,'".+')
  tableau$ark<-str_remove(tableau$ark,'%20')
  fichier<-liste_departements$titre[j]
  fichier<-iconv(fichier,from="UTF-8",to="ASCII//TRANSLIT")
  fichier<-str_to_lower(fichier)
  fichier<-str_replace_all(fichier," $","")
  fichier<-str_replace_all(fichier," ","-")
  fichier<-str_c("C:/Users/Benjamin/gallicagram_app/liste_departement_",fichier,".csv")
  write.csv(tableau,fichier,fileEncoding = "UTF-8",row.names = F)
  print(fichier)
}

liste_departements<-read.csv("C:/Users/Benjamin/gallicagram_app/liste_departements.csv",encoding = "UTF-8")
liste_departements<-liste_departements[order(liste_departements$titre),]
rownames(liste_departements)=NULL
liste_departements$csv=NA
liste_departements$num=NA
for (j in 1:length(liste_departements$titre)) {
  fichier<-liste_departements$titre[j]
  fichier<-iconv(fichier,from="UTF-8",to="ASCII//TRANSLIT")
  fichier<-str_to_lower(fichier)
  fichier<-str_replace_all(fichier," $","")
  fichier<-str_replace_all(fichier," ","-")
  fichier<-str_c("liste_departement_",fichier,".csv")
  liste_departements$csv[j]<-fichier
  liste_departements$num[j]<-j+50
}
liste_departements<-liste_departements[liste_departements$titre!="Essonne",]
write.csv(liste_departements,"C:/Users/Benjamin/gallicagram_app/liste_departements.csv",fileEncoding = "UTF-8",row.names = F)
