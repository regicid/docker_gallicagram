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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-c?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-d-e?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-f-g?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-h-o?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-p?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-q-s?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
url="https://gallica.bnf.fr/html/presse-et-revues/feuilles-de-tranchee-t-z?mode=desktop"
page<-read_html(url)
page<-html_node(page,".liste")
nb<-str_count(page,"mosaic-item")
for (i in 1:nb) {
  a<-html_node(page,str_c("div.mosaic-item:nth-child(",i,")"))
  ark<-str_extract(a,"ark:/12148/.+")
  ark<-str_remove(ark,"ark:/12148/")
  ark<-str_remove(ark,"/.+")
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_feuilles-de-tranchée.csv",fileEncoding = "UTF-8",row.names = F)


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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_resistance-zone-nord.csv",fileEncoding = "UTF-8",row.names = F)

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
  titre<-str_extract(a,"Accéder à.+")
  titre<-str_remove(titre,"Accéder à la page ")
  titre<-str_remove(titre,"&quot;")
  titre<-str_remove(titre," \\(.+")
  titre<-str_remove_all(titre,"[:punct:]")
  tableau<-rbind(tableau,cbind(ark,titre))
}
tableau<-tableau[is.na(tableau$ark)==FALSE,]
tableau$ark<-str_remove(tableau$ark,'".+')
write.csv(tableau,"C:/Users/Benjamin/gallicagram_app/liste_themes_presse-litteraire.csv",fileEncoding = "UTF-8",row.names = F)
