library(shiny)
library(plotly)
library(cartogram)
library(sf)
library(stringr)
library(xml2)
library(markdown)
library(shinythemes)
library(htmlwidgets)
library(httr)
library(dplyr)
library(htmltools)
library(purrr)
library(rvest)
library(tidytext)
library(shinybusy)
library(ggthemes)
library(RColorBrewer)
library(cowplot)
library(leaflet)
library(scales)
library(gtrendsR)
library(timetk)
library(jsonlite)
library(ggwordcloud)
library(shinyalert)
library(bezier)

httr::set_config(config(ssl_verifypeer = 0L))


js <- "
function(el, x) {
el.on('plotly_click', function(d) {
var point = d.points[0];
var url = point.data.customdata[point.pointIndex];
window.open(url);
});
}"

jsg <- "
function(el, x) {
el.on('plotly_click', function(d) {
var point = d.points[0];
var url = point.data.customdata[point.pointIndex];
});
}"


se="linux"

Plot <- function(data,input){
  tableau = data[["tableau"]]
  if(is.null(data[["tableau_page"]])==FALSE){
    if(isolate(input$search_mode)==2){
      tableau = data[["tableau_page"]]
    }
    if(isolate(input$doc_type)==4 & isolate(input$search_mode)==1){
      tableau = data[["tableau_volume"]]
    }
  }
  if(input$multicourbes==TRUE | isolate(input$doc_type)==0){
    if(input$multicourbes==TRUE){tableau = memoire}
    tableau$mot[str_length(tableau$mot)>=30]<-str_c(str_trunc(tableau$mot[str_length(tableau$mot)>=30],30,"right"),"..")
    if(input$multicourbes==TRUE){
      tableau$mot<-str_c(tableau$mot,"<br>",tableau$bibli,"/",tableau$corpus,"/",tableau$langue,"/",tableau$search_mode)
    }
    if(isolate(input$doc_type)==0){
      tableau$mot<-str_c(tableau$mot,"<br>",tableau$bibli,"/",tableau$corpus)
      tableau$mot<-str_replace_all(tableau$mot,"Le Monde/Presse","Le Monde")
    }
    if(isolate(input$resolution=="Mois")){
      tableau<-tableau[tableau$resolution=="Mois",]
    }
    if(isolate(input$resolution=="Année")){
      tableau<-tableau[tableau$resolution=="Année",]
    }
    if(isolate(input$resolution=="Semaine")){
      tableau<-tableau[tableau$resolution=="Semaine",]
    }
  }else{tableau$mot[str_length(tableau$mot)>=30]<-str_c(str_trunc(tableau$mot[str_length(tableau$mot)>=30],30,"right"),"..")}
  
  tableau<-distinct(tableau)
  
  if(data[["resolution"]]=="Semaine"){tableau$date=ymd(tableau$date)}
  if(data[["resolution"]]=="Mois"){
    tableau$date<-str_c(tableau$date,"/01")
    tableau$date<-as.Date.character(tableau$date,format = c("%Y/%m/%d"))
  }
  if(data[["resolution"]]=="Année"){
    tableau$date<-str_c(tableau$date,"/01/01")
    tableau$date<-as.Date.character(tableau$date,format = c("%Y/%m/%d"))
  }
  
  
  if(input$visualiseur==6 | input$visualiseur==9){
    library(FactoMineR)
    library(tidyr)
    if(data[["resolution"]]=="Année"){
      tableau$date<-str_extract(tableau$date,"....")}
    if(data[["resolution"]]=="Mois"){
      tableau$date<-str_extract(tableau$date,".......")}
    total<-select(tableau,mot,date,ratio)
    a=spread(total, mot,ratio)
    rownames(a)=as.character(a$date)
    b=as.character(a$date)
    a<-a[,-1]
    rownames(a)=b
    #a = t(a)
    if(input$visualiseur==6){
      res.pca=PCA(a,scale.unit = TRUE)
      rownames(res.pca$ind$coord)=rownames(a)
    }
    if(input$visualiseur==9){
      res.pca=CA(a)
      rownames(res.pca$row$coord)=rownames(a)
    }
    
    library(factoextra)
    if( isolate(input$resolution=="Mois")){b = str_extract(b,"....")
    }
    if(input$afcline==T){
      if(input$visualiseur==6){
        bb<-fviz_pca_biplot(res.pca,geom.var = c("text"),geom.ind = c("point"), label="all",labelsize=3,col.ind=as.integer(b),col.var="black")+labs(title="") + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
        gg=as.data.frame(cbind(res.pca$ind$coord[,1],res.pca$ind$coord[,2]))
      }
      if(input$visualiseur==9){
        bb<-fviz_ca_biplot(res.pca,geom.col = c("text"),geom.row = c("point"), label="all",labelsize=3,col.row=as.integer(b),col.col="black")+labs(title="") + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
        gg=as.data.frame(cbind(res.pca$row$coord[,1],res.pca$row$coord[,2]))
      }
      
      colnames(gg)=c("x","y")
      gg=as.data.frame(bezier::bezier(seq(0, 1, len=100), gg, deg=nrow(gg)-1))
      colnames(gg)=c("x","y")
      bb=bb+geom_path(data=gg,aes(x,y),col=1)
      plot=ggplotly(bb)
      plot$x$data[[1]]$text <- bb$data$name
      plot$x$data[[4]]$hovertext<-plot$x$data[[4]]$text
      
      plot$x$data[[5]]$text=NA
      
    }else{
      if(input$visualiseur==6){
        bb<-fviz_pca_biplot(res.pca,geom.var = c("text"),geom.ind = c("text"), label="all",labelsize=3,col.ind=as.integer(b),col.var="black")+labs(title="") + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
      }
      if(input$visualiseur==9){
        bb<-fviz_ca_biplot(res.pca,geom.col = c("text"),geom.row = c("text"), label="all",labelsize=3,col.row=as.integer(b),col.col="black")+labs(title="") + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
      }
      plot=ggplotly(bb)
    }
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  if(input$visualiseur==7){
    total<-select(tableau,mot,count)
    total=total%>%group_by(mot)%>%summarise_all(sum)
    total$x=0
    total$y=0
    for (i in 1:length(total$count)) {
      total$x[i]=runif(1)*100
      total$y[i]=runif(1)*100
    }
    total$hovers=str_c(total$mot," : ",total$count)
    total$url="www.google.com"
    if(isolate(input$doc_type)==1){total$url=str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",total$mot,"%22%20)%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",input$beginning,"%22%20and%20gallicapublication_date%3C=%22",input$end,"%22)&suggest=10&keywords=",total$mot)}
    if(isolate(input$doc_type)==2){total$url=str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",total$mot,"%22%20)%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",input$beginning,"%22%20and%20gallicapublication_date%3C=%22",input$end,"%22)&suggest=10&keywords=",total$mot)}
    if(isolate(input$doc_type)==30){total$url=str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",total$mot,"%22&start_at=01%2F01%2F",input$beginning,"&end_at=31%2F12%2F",input$end,"&search_sort=relevance_desc")}
    # plot=plot_ly(total,x=~x,y=~y,size = ~count,color=~count,type = 'scatter', mode = 'markers',colors="Reds",
    #            marker = list(sizemode = "diameter", opacity = 0.5),text=~total$hovers,hoverinfo="text")%>%
    #   add_text(text=~mot,size=10, textposition="center",textfont = list(color = '#000000'))
    # plot=layout(plot,xaxis = list(title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
    #           yaxis = list(title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
    #           showlegend=F)
    plot=ggplot(total, aes(x=x, y=y, size = count,color=count,text=hovers)) +
      geom_point(alpha=0.7)+
      scale_size(range = c(1, 24))+
      scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
      geom_text(aes(x=x,y=y,label=mot),size=3,color="black")+
      labs(x = "", y = "") + theme_void()+
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        legend.position="none"
      )
    plot=ggplotly(plot,tooltip = "text")%>%
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  tableau$scale<-tableau$ratio
  
  for(mot in unique(tableau$mot)){
    z = which(tableau$mot==mot)
    tableau$scale[z]=scale(tableau$scale[z],center = F,scale = T)
  }
  if(input$scale==TRUE |input$multicourbes==TRUE){tableau$ratio = tableau$scale}
  
  if(input$delta==T | input$fraction==T){
    mots<-str_split(input$mot,"&")
    x = 1:sum(tableau$mot==unlist(mots)[1])
    if(input$delta==T){
      tableau$ratio[tableau$mot==unlist(mots)[1]]<-tableau$ratio[tableau$mot==unlist(mots)[1]]-tableau$ratio[tableau$mot==unlist(mots)[2]]
    }
    if(input$fraction==T){
      tableau$ratio[tableau$mot==unlist(mots)[1]]<-tableau$ratio[tableau$mot==unlist(mots)[1]]/tableau$ratio[tableau$mot==unlist(mots)[2]]
    }
    tableau<-tableau[tableau$mot==unlist(mots)[1],]
  }
  
  Title = paste("")
  tableau$loess=tableau$ratio
  width = length(unique(tableau$date))
  span = 2/width + input$span*(width-2)/(10*width)
  
  
  if(input$span >0){
    if(input$loess==F){
      for(mot in unique(tableau$mot)){
        z = which(tableau$mot==mot)
        for(i in 1:length(z)){
          j = max(i-floor(input$span/2),0)
          k = i+ceiling(input$span/2)
          if(is.na(tableau$ratio[i])){
            next
          }
          pond = tableau$base[z][j:k]
          tableau$loess[z][i] = sum(tableau$ratio[z][j:k]*pond/sum(pond,na.rm = T),na.rm=T)
            #sum(tableau$ratio[z][j:k]*pond/sum(pond[!is.na(tableau$ratio[z][j:k])],na.rm = T))
          ##Si ça bug, remettre le na.rm=T
        }}
    }
    if(input$loess==T){
      for(mot in unique(tableau$mot)){
        z = which(tableau$mot==mot)
        x = 1:length(z)
        tableau$loess[z] = loess(tableau$loess[z]~x,span=span)$fitted
      }
    }
    
  }
  
  
  
  if(input$delta==F){
    tableau$loess[tableau$loess<0]<-0
  }
  dn<-as.character(max(format(tableau$ratio,scientific=FALSE)))
  if(max(tableau$ratio, na.rm = TRUE)>=0.1){digit_number=".1%"}
  else{
    digit_number=str_extract(dn,"\\..+")
    digit_number=str_replace(digit_number,"\\.","")
    digit_number=str_extract(digit_number,"0+")
    digit_number<-str_length(digit_number)
    digit_number<-str_c(".",digit_number,"%")
  }
  numGroups=3
  if(length(unique(tableau$mot))>=3){numGroups <- length(unique(tableau$mot))}
  customPalette <- brewer.pal(numGroups, "Set1")
  customPalette = customPalette[c(2,1,3:numGroups)]
  if(length(unique(tableau$mot))==1){
    customPalette <- brewer.pal(numGroups, "Set1")
    customPalette = customPalette[c(2)]
  }
  if(length(unique(tableau$mot))==2){
    customPalette <- brewer.pal(numGroups, "Set1")
    customPalette = customPalette[c(2,1)]
  }
  if(length(unique(tableau$mot))>9){customPalette=NULL}
  
  if(input$visualiseur==10){
    total<-select(tableau,mot,ratio)
    total=total%>%group_by(mot)%>%summarise_all(sum)
    total$center=as.Date(NA)
    for (mot in total$mot) {
      zaz=tableau[tableau$mot==mot,]
      zoo=0
      j=NULL
      for (i in 1:length(zaz$ratio)) {
        if(zoo<total$ratio[total$mot==mot]/2) {
          zoo=zoo+zaz$ratio[i]
          j=zaz$date[i]
        }
        
      }
      total$center[total$mot==mot]<-as.Date(j)
    }
    
    tableau$ratio=NA
    for (mot in total$mot) {
      tableau$ratio[tableau$mot==mot & tableau$date==total$center[total$mot==mot]]=total$ratio[total$mot==mot]
    }
    plot=plot_ly(tableau, x = ~date, y = ~ratio, text = ~mot,color=~mot, type = 'scatter', mode = 'markers',size = ~ratio,sizes = c(10, 25),
                 colors=customPalette,marker = list(sizemode="diameter", opacity = 0))
    plot=plot%>%add_text()
    maximum=tableau$ratio[!is.na(tableau$ratio)]
    maximum=max(maximum)
    maximum=maximum+maximum/10
    plot=layout(plot,showlegend = FALSE,
                xaxis=list(range=c(tableau$date[1],tableau$date[length(tableau$date)]),title=""),
                yaxis=list(type = "log",showticklabels = FALSE,showgrid = FALSE,title=""))
    
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  if(input$visualiseur==2){
    
    total<-select(tableau,count,mot)
    total<-total%>%group_by(mot)%>%summarise_all(sum)
    total$url="www.google.com"
    if(isolate(input$doc_type)==1){total$url=str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",total$mot,"%22%20)%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",input$beginning,"%22%20and%20gallicapublication_date%3C=%22",input$end,"%22)&suggest=10&keywords=",total$mot)}
    if(isolate(input$doc_type)==2){total$url=str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",total$mot,"%22%20)%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",input$beginning,"%22%20and%20gallicapublication_date%3C=%22",input$end,"%22)&suggest=10&keywords=",total$mot)}
    if(isolate(input$doc_type)==30){total$url=str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",total$mot,"%22&start_at=01%2F01%2F",input$beginning,"&end_at=31%2F12%2F",input$end,"&search_sort=relevance_desc")}
    plot<-plot_ly(x=~total$count,y=reorder(total$mot,total$count),type="bar",customdata=total$url,colors=customPalette)
    if(length(unique(tableau$mot))>9){plot<-plot_ly(x=~total$count,y=reorder(total$mot,total$count),type="bar",customdata=total$url)}
    plot<-layout(plot,xaxis=list(title="Nombre d'occurrences dans le corpus"))
    plot = layout(plot,showlegend=F)
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  if(data[["resolution"]]=="Mois"){tableau$hovers = str_c(str_extract(tableau$date,"......."),": x/N = ",tableau$count,"/",tableau$base,"\n                 = ",round(tableau$ratio*100,digits = 1),"%")}
  else if(data[["resolution"]]=="Semaine"){tableau$hovers = str_c(tableau$date,": x/N = ",tableau$count,"/",tableau$base,"\n                 = ",round(tableau$ratio*100,digits = 1),"%")}
  else{tableau$hovers = str_c(str_extract(tableau$date,"...."),": x/N = ",tableau$count,"/",tableau$base,"\n                 = ",round(tableau$ratio*100,digits = 1),"%")}
  y <- list(title = "Fréquence dans le corpus",titlefont = 41,tickformat = digit_number,spikecolor="grey")
  if(input$scale==TRUE | input$multicourbes==TRUE){y <- list(title = "Fréquence dans le corpus",titlefont = 41,spikecolor="grey")}
  x <- list(title = "",titlefont = 41,spikecolor="grey")
  if(isolate(input$search_mode)==3){
    if(data[["resolution"]]=="Mois"){tableau$hovers = str_c(str_extract(tableau$date,".......")," : ",round(tableau$ratio*100,digits = 6),"%")}
    else if(data[["resolution"]]=="Semaine"){tableau$hovers = str_c(tableau$date," : ",round(tableau$ratio*100,digits = 6),"%")}
    else{tableau$hovers = str_c(str_extract(tableau$date,"....")," : ",round(tableau$ratio*100,digits = 6),"%")}
    y <- list(title = "Fréquence dans le corpus",titlefont = 41,tickformat = digit_number,spikecolor="grey")
    if(input$scale==TRUE | input$multicourbes==TRUE){y <- list(title = "Fréquence dans le corpus",titlefont = 41,spikecolor="grey")}
  }
  
  if(input$visualiseur==11){
    total<-tableau
    total$hovers<-str_c(total$mot," : ",total$hovers)
    total<-total%>%group_by(mot)
    total$date=as.Date.character(total$date)
    total$mot=as.factor(total$mot)
    total$date=as.factor(total$date)
    total=total[order(total$mot,total$date),]
    plot=ggplot(total,aes(x=date,fill=ratio,y=mot))+
      geom_tile()+
      scale_fill_fermenter(palette="RdBu")+
      # scale_x_date(breaks = date_breaks("2 year"),
      #              labels = date_format("%Y"))+
      labs(x = "", y = "")+theme_tufte() +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x= element_blank(),
        axis.line.y= element_blank(),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position="none"
      )
    plot=ggplotly(plot)
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  if(input$visualiseur==4 & (isolate(input$resolution!="Mois") | input$saisons==F)){
    total<-tableau
    total$hovers<-str_c(total$mot," : ",total$hovers)
    total<-total%>%group_by(mot)
    plot<-plot_ly(x=~total$date,y=reorder(total$mot, total$count, sum),type = 'scatter', mode = 'markers',customdata=total$url, color=~total$mot,colors=customPalette,size = ~total$ratio,sizes = c(0, 50),marker = list( sizemode = "diameter", opacity = 0.3),text=~total$hovers,hoverinfo="text")
    if(length(unique(tableau$mot))>9){plot<-plot_ly(x=~total$date,y=reorder(total$mot, total$count, sum),type = 'scatter', mode = 'markers',customdata=total$url, color=~total$mot,size = ~total$ratio,sizes = c(0, 50),marker = list( sizemode = "diameter", opacity = 0.3),text=~total$hovers,hoverinfo="text")}
    plot<-layout(plot,xaxis=list(title=""))
    plot = layout(plot,showlegend=F)
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  if(input$visualiseur==4 & isolate(input$resolution=="Mois") & input$saisons==T){
    aaa=str_extract(tableau$date,"....")
    bbb=str_extract(tableau$date,".......")
    bbb=str_remove(bbb,".....")
    tableau$annee=aaa
    tableau$mois=bbb
    tableau<-tableau%>%group_by(mois,mot)%>%summarise(loess=mean(loess))
    tableau$date=as.numeric(tableau$mois)
    tableau$date=as.Date.character(str_c("2000-",tableau$date,"-01"))
    plot<-plot_ly(x=~tableau$date,y=reorder(tableau$mot, tableau$loess, sum),type = 'scatter', mode = 'markers', color=~tableau$mot,colors=customPalette,size = ~tableau$loess,sizes = c(0, 50),marker = list( sizemode = "diameter", opacity = 0.6))
    plot<-layout(plot,xaxis=list(title="",tickformat="%b"))
    plot = layout(plot,showlegend=F)
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  if(input$visualiseur==5){
    plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+ geom_line(aes(color=mot))+geom_area(aes(fill=mot,text=map(paste('<b>',mot,':</b>', hovers, ''), HTML)),alpha=0.3)+facet_wrap(~mot,ncol = 1)+xlab("")+ylab("")+
      theme_tufte()+ scale_color_manual(values=customPalette,name=NULL)+ scale_fill_manual(values=customPalette,name=NULL)+
      theme(plot.background = element_rect(fill = 'white', colour = 'white'),panel.margin.y = unit(0, "lines"),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),strip.background = element_blank(), strip.text.x = element_blank(),axis.line.x = element_line(colour = "black"),legend.title= element_blank(), legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
    if(length(unique(tableau$mot))>9){
      plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+ geom_line(aes(color=mot))+geom_area(aes(fill=mot,text=map(paste('<b>',mot,':</b>', hovers, '<br>'), HTML)),alpha=0.3)+facet_wrap(~mot,ncol = 1)+xlab("")+ylab("")+
        theme_tufte()+scale_color_discrete(name=NULL)+
        theme(plot.background = element_rect(fill = 'white', colour = 'white'),panel.margin.y = unit(0, "lines"),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),strip.background = element_blank(), strip.text.x = element_blank(),axis.line.x = element_line(colour = "black"),legend.title= element_blank(), legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
    }
    plot=ggplotly(plot,tooltip = c("text"))
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  loess = tableau$loess
  base = tableau$base
  tableau$ribbon_down = loess-1.96*sqrt(loess*(1-loess)/base)
  z = which(tableau$ribbon_down<0)
  tableau$ribbon_down[z] = 0
  tableau$ribbon_up = loess+1.96*sqrt(loess*(1-loess)/base)
  z = which(loess==0)
  tableau$ribbon_up[z] = 3/base[z]
  tableau$ribbon_up[is.na(tableau$ribbon_up)] <- max(tableau$ribbon_up,na.rm=T)
  tableau$ribbon_down[is.na(tableau$ribbon_down)] <- 0
  tableau$ribbon_up[tableau$ribbon_up>1.1*max(loess,na.rm=T)] = max(loess,na.rm=T)*1.1
  if(length(unique(tableau$date))<=20){
    plot = plot_ly(tableau, x=~date,y=~loess,color =~mot,type='scatter',mode='lines+markers',line = list(shape = "spline"),customdata=tableau$url,colors=customPalette,legendgroup=~mot,text=~hovers,hoverinfo="text",connectgaps = FALSE)
    if(isolate(input$doc_type)!=5 & isolate(input$doc_type)!=9 & isolate(input$doc_type)!=10 & isolate(input$doc_type)!=12 & isolate(input$doc_type)!=44 & isolate(input$doc_type)!=58 & isolate(input$doc_type)!=59 & isolate(input$doc_type)!=60 & isolate(input$doc_type)!=61 & isolate(input$doc_type)!=62 & isolate(input$doc_type)!=63 & isolate(input$doc_type)!=64 & input$scale==F & input$multicourbes==F){
      plot=plot%>%add_ribbons(data=tableau,x=~date,ymin=~ribbon_down,ymax=~ribbon_up,legendgroup=~mot,fillcolor=~mot,showlegend=F,opacity=.2)
    }
    plot = plot %>% add_trace(x=~date,y=~loess,color=~mot,legendgroup=~mot,showlegend=F,connectgaps = FALSE)
  }  else{
    plot = plot_ly(data=tableau, x=~date,y=~loess,color =~mot,type='scatter',mode='lines',line = list(shape = "spline"),customdata=tableau$url,colors=customPalette,legendgroup=~mot,text=~hovers,hoverinfo="text",connectgaps = FALSE)
    if(isolate(input$doc_type)!=5 & isolate(input$doc_type)!=9 & isolate(input$doc_type)!=10 & isolate(input$doc_type)!=12 & isolate(input$doc_type)!=44 & isolate(input$doc_type)!=58 & isolate(input$doc_type)!=59 & isolate(input$doc_type)!=60 & isolate(input$doc_type)!=61 & isolate(input$doc_type)!=62 & isolate(input$doc_type)!=63 & isolate(input$doc_type)!=64 & input$scale==F & input$multicourbes==F){
      plot=plot%>%add_ribbons(data=tableau,x=~date,ymin=~ribbon_down,ymax=~ribbon_up,legendgroup=~mot,showlegend=F,fillcolor=~mot,opacity=.2)
    }
    plot = plot %>% add_trace(x=~date,y=~loess,color=~mot,legendgroup=~mot,showlegend=F,customdata=tableau$url,connectgaps = FALSE)
    y_max = tableau$ribbon_up[which.max(tableau$loess)]
    y_min = tableau$ribbon_down[which.min(tableau$loess)]
    plot = plot %>% layout(yaxis=list(range=c(y_min,y_max)))}
  
  if(input$visualiseur==1 & isolate(input$resolution=="Mois") & input$saisons==T){
    aaa=str_extract(tableau$date,"....")
    bbb=str_extract(tableau$date,".......")
    bbb=str_remove(bbb,".....")
    tableau$annee=aaa
    tableau$mois=bbb
    tableau<-tableau%>%group_by(mois,mot)%>%summarise(loess=mean(loess))
    tableau$date=as.numeric(tableau$mois)
    tableau$date=as.Date.character(str_c("2000-",tableau$date,"-01"))
    plot = plot_ly(data=tableau,x=~date,y=~loess,color=~mot,type='scatter',mode='spline',line = list(shape = "spline"),colors=customPalette,legendgroup=~mot)
    plot=layout(plot,xaxis = list(tickformat="%b"))
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  if(input$visualiseur==8 & isolate(input$resolution=="Mois")){
    if(input$loess==T){
      for (mot in unique(tableau$mot)) {
        z = which(tableau$mot==mot)
        for(i in 1:length(z)){
          j = max(i-floor(12/2),0)
          k = i+ceiling(12/2)
          if(is.na(tableau$ratio[i])){
            next
          }
          pond = tableau$base[z][j:k]
          tableau$trend[z][i] = sum(tableau$ratio[z][j:k]*pond/sum(pond[!is.na(tableau$ratio[z][j:k])],na.rm = T),na.rm = T)
        }
      }
      tableau$loess=tableau$ratio-tableau$trend
    }
    
    aaa=str_extract(tableau$date,"....")
    bbb=str_extract(tableau$date,".......")
    bbb=str_remove(bbb,".....")
    if(input$saisons==T){
      tableau$annee=aaa
      tableau$mois=bbb
      tableau<-tableau%>%group_by(mois,mot)%>%summarise(loess=mean(loess))
      tableau$date=as.numeric(tableau$mois)*30
      for (mot in unique(tableau$mot)) {
        ccc=tableau[tableau$mot==mot,]
        ccc=ccc[1,]
        tableau=rbind(tableau,ccc)
      }
      plot = plot_ly(data=tableau,r=~loess, theta=~date,color =~mot,type='scatterpolar',mode='spline',line = list(shape = "spline"),colors=customPalette,legendgroup=~mot)
      
    }
    else{
      tableau$date=360*as.numeric(aaa)+30*as.numeric(bbb)
      plot = plot_ly(data=tableau,r=~loess, theta=~date,color =~mot,type='scatterpolar',mode='spline',line = list(shape = "spline"),customdata=tableau$url,colors=customPalette,legendgroup=~mot,text=~hovers,hoverinfo="text")
    }
    plot=layout(plot,
                showlegend = T,
                polar = list(
                  angularaxis = list(
                    showticklabels = TRUE,
                    showgrid = FALSE,
                    ticks = '',
                    direction = 'clockwise',
                    tickmode="array",
                    tickvals = seq(0, 360, 30),
                    ticktext = as.array(c("décembre","janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"))
                  ),
                  radialaxis = list(
                    showgrid = T,
                    showticklabels = F,
                    ticks = ''
                  )
                ))
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
  if(input$visualiseur==3){
    if(data[["resolution"]]=="Mois"){tableau$hovers = str_c(str_extract(tableau$date,".......")," : ", tableau$count)}
    else if(data[["resolution"]]=="Semaine"){tableau$hovers = str_c(tableau$date," : ", tableau$count)}
    else{tableau$hovers = str_c(str_extract(tableau$date,"....")," : ", tableau$count)}
    y <- list(title = "Nombre d'occurrences dans\nle corpus",titlefont = 41,spikecolor="grey")
    plot = plot_ly(tableau, x=~date,y=~count,text=~hovers,color =~mot,type='bar', hoverinfo="text",customdata=tableau$url,colors=customPalette)}
  plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  if(length(grep(",",data$mot))==0 & input$isMobile==F){plot = layout(plot,showlegend=TRUE,legend = list(x = 100, y = -0.1))}
  if(length(grep(",",data$mot))==0 & input$isMobile==T){plot = layout(plot,showlegend=TRUE,legend = list(orientation = 'h',y=-0.1))}
  
  if(input$delta==TRUE){
    if(data[["resolution"]]=="Mois"){tableau$hovers2 = str_c(str_extract(tableau$date,".......")," : delta = ",round(tableau$loess*100,digits=2),"%")}
    else{tableau$hovers2 = str_c(str_extract(tableau$date,"....")," : delta = ",round(tableau$loess*100,digits=2),"%")}
    if(length(unique(tableau$date))<=20){
      plot = plot_ly(filter(tableau,mot==unlist(mots)[[1]]), x=~date,y=~loess,text=~hovers2,type='scatter',mode='spline+markers',line = list(shape = "spline"),hoverinfo="text",colors=customPalette)
    }
    else{
      plot = plot_ly(filter(tableau,mot==unlist(mots)[[1]]), x=~date,y=~loess,text=~hovers2,type='scatter',mode='spline',line = list(shape = "spline"),hoverinfo="text",colors=customPalette)
    }
    y <- list(title = "",titlefont = 41,tickformat = digit_number,spikecolor="grey")
    x <- list(title = "",titlefont = 41,spikecolor="grey")
    Title = paste("Freq(",unlist(mots)[1],") – Freq(",unlist(mots)[2],")")
    Title=str_remove_all(Title," ")
    plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  }
  if(input$fraction==TRUE){
    if(data[["resolution"]]=="Mois"){tableau$hovers2 = str_c(str_extract(tableau$date,".......")," : delta = ",round(tableau$loess*100,digits=2),"%")}
    else{tableau$hovers2 = str_c(str_extract(tableau$date,"....")," : delta = ",round(tableau$loess*100,digits=2),"%")}
    if(length(unique(tableau$date))<=20){
      plot = plot_ly(filter(tableau,mot==unlist(mots)[[1]]), x=~date,y=~loess,text=~hovers2,type='scatter',mode='spline+markers',line = list(shape = "spline"),hoverinfo="text",colors=customPalette)
    }
    else{
      plot = plot_ly(filter(tableau,mot==unlist(mots)[[1]]), x=~date,y=~loess,text=~hovers2,type='scatter',mode='spline',line = list(shape = "spline"),hoverinfo="text",colors=customPalette)
    }
    y <- list(title = "",titlefont = 41,spikecolor="grey")
    x <- list(title = "",titlefont = 41,spikecolor="grey")
    Title = paste("Freq(",unlist(mots)[1],") / Freq(",unlist(mots)[2],")")
    Title=str_remove_all(Title," ")
    plot = layout(plot, yaxis = y, xaxis = x,title = Title)
  }
  if(input$barplot){
    width = nrow(tableau)
    span = 2/width + input$span*(width-2)/(10*width)
    if(data[["resolution"]]=="Mois"){tableau$hovers = str_c(str_extract(tableau$date,"......."),": N = ",tableau$base)}
    if(data[["resolution"]]=="Semaine"){tableau$hovers = str_c(tableau$date,": N = ",tableau$base)}
    else{tableau$hovers = str_c(str_extract(tableau$date,"...."),": N = ",tableau$base)}
    plot1 = plot_ly(tableau, x=~date[tableau$mot==mot[1]],y=~base[tableau$mot==mot[1]],text=~hovers[tableau$mot==mot[1]],type='bar',hoverinfo="text",marker = list(color='rgba(31, 119, 180,1)'),colors=customPalette)
    y <- list(title = "",titlefont = 41,spikecolor="grey")
    x <- list(title = "",titlefont = 41)
    plot1 = layout(plot1, yaxis = y, xaxis = x,title = Title,showlegend = FALSE)
    plot= plot%>%add_lines()
    plot = plotly::subplot(plot,plot1,nrows = 2,legend=NULL,shareX = T)
    plot=plot %>%  layout(xaxis = list(autorange = TRUE),  yaxis = list(autorange = TRUE))
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  } else{
    plot=layout(plot)
    plot=plot %>%  layout(xaxis = list(autorange = TRUE),  yaxis = list(autorange = TRUE))
    if(isolate(input$doc_type)==1 | isolate(input$doc_type)==2 | isolate(input$doc_type)==56 | isolate(input$doc_type) %in% 66:76){return(onRender(plot,jsg))}
    else{return(onRender(plot,js))}
  }
  
}

SPlot <- function(data,input){
  
  tableau = data[["tableau"]]
  if(is.null(data[["tableau_page"]])==FALSE){
    if(input$search_mode==2){
      tableau = data[["tableau_page"]]
    }
    if(input$doc_type==4 & input$search_mode==1){
      tableau = data[["tableau_volume"]]
    }
  }
  if(input$multicourbes==TRUE | input$doc_type==0){
    if(input$multicourbes==TRUE){tableau = memoire}
    if(input$multicourbes==TRUE){
      tableau$mot<-str_c(tableau$mot,"\n",tableau$bibli,"/",tableau$corpus,"/",tableau$langue,"/",tableau$search_mode)
    }
    if(input$doc_type==0){
      tableau$mot<-str_c(tableau$mot,"\n",tableau$bibli,"/",tableau$corpus)
      tableau$mot<-str_replace_all(tableau$mot,"Le Monde/Presse","Le Monde")
    }
    if(isolate(input$resolution=="Mois")){
      tableau<-tableau[tableau$resolution=="Mois",]
    }
    if(isolate(input$resolution=="Année")){
      tableau<-tableau[tableau$resolution=="Année",]
    }
    if(isolate(input$resolution)=="Semaine"){
      tableau<-tableau[tableau$resolution=="Semaine",]
    }
  }
  tableau<-distinct(tableau)
  
  if(data[["resolution"]]=="Semaine"){tableau$date=ymd(tableau$date)}
  if(data[["resolution"]]=="Mois"){
    tableau$date<-str_c(tableau$date,"/01")
    tableau$date<-as.Date.character(tableau$date,format = c("%Y/%m/%d"))
  }
  if(data[["resolution"]]=="Année"){
    tableau$date<-str_c(tableau$date,"/01/01")
    tableau$date<-as.Date.character(tableau$date,format = c("%Y/%m/%d"))
  }
  Title = paste("")
  
  if(input$visualiseur==6 | input$visualiseur==9){
    library(FactoMineR)
    library(tidyr)
    if(data[["resolution"]]=="Année"){
      tableau$date<-str_extract(tableau$date,"....")}
    if(data[["resolution"]]=="Mois"){
      tableau$date<-str_extract(tableau$date,".......")}
    total<-select(tableau,mot,date,ratio)
    a=spread(total, mot,ratio)
    rownames(a)=as.character(a$date)
    b=as.character(a$date)
    a<-a[,-1]
    rownames(a)=b
    if(input$visualiseur==6){
      res.pca=PCA(a,scale.unit = TRUE)
      rownames(res.pca$ind$coord)=rownames(a)
    }
    if(input$visualiseur==9){
      res.pca=CA(a)
      rownames(res.pca$row$coord)=rownames(a)
    }
    library(factoextra)
    if( isolate(input$resolution=="Mois")){
  b = str_extract(b,"....")}
    repel = (length(unique(tableau$date))<30)
    if(input$visualiseur==6){
      plot<-fviz_pca_biplot(res.pca,geom.var = c("text"),geom.ind = c("text"), label="all",labelsize=3,col.ind=as.integer(b),col.var="black",repel=repel)+labs(title="")+theme(plot.background = element_rect(fill = 'white', colour = 'white')) + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
    }
    if(input$visualiseur==9){
      plot<-fviz_ca_biplot(res.pca,geom.col = c("text"),geom.row = c("text"), label="all",labelsize=3,col.row=as.integer(b),col.col="black",repel=repel)+labs(title="")+theme(plot.background = element_rect(fill = 'white', colour = 'white')) + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
    }
    if(input$afcline==T){
      if(input$visualiseur==6){
        plot<-fviz_pca_biplot(res.pca,geom.var = c("text"),geom.ind = c("point"), label="all",labelsize=3,col.ind=as.integer(b),col.var="black",repel=repel)+labs(title="")+theme(plot.background = element_rect(fill = 'white', colour = 'white')) + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
        gg=as.data.frame(cbind(res.pca$ind$coord[,1],res.pca$ind$coord[,2]))
      }
      if(input$visualiseur==9){
        plot<-fviz_ca_biplot(res.pca,geom.col = c("text"),geom.row = c("point"), label="all",labelsize=3,col.row=as.integer(b),col.col="black",repel=repel)+labs(title="")+theme(plot.background = element_rect(fill = 'white', colour = 'white')) + scale_color_gradientn(colors=rainbow(10,start=.65),guide="none")
        gg=as.data.frame(cbind(res.pca$row$coord[,1],res.pca$row$coord[,2]))
      }
    colnames(gg)=c("x","y")
    gg=as.data.frame(bezier::bezier(seq(0, 1, len=100), gg, deg=nrow(gg)-1))
    colnames(gg)=c("x","y")
    plot=plot+geom_path(data=gg,aes(x,y),col=1)
    }
    return(plot)
  }
  
  
  if(input$visualiseur==7){
    total<-select(tableau,mot,count)
    total=total%>%group_by(mot)%>%summarise_all(sum)
    total$x=0
    total$y=0
    for (i in 1:length(total$count)) {
      total$x[i]=runif(1)*100
      total$y[i]=runif(1)*100
    }
    total$hovers=str_c(total$mot," : ",total$count)
    total$url="www.google.com"
    if(input$doc_type==1){total$url=str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",total$mot,"%22%20)%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",input$beginning,"%22%20and%20gallicapublication_date%3C=%22",input$end,"%22)&suggest=10&keywords=",total$mot)}
    if(input$doc_type==2){total$url=str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",total$mot,"%22%20)%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",input$beginning,"%22%20and%20gallicapublication_date%3C=%22",input$end,"%22)&suggest=10&keywords=",total$mot)}
    if(input$doc_type==30){total$url=str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",total$mot,"%22&start_at=01%2F01%2F",input$beginning,"&end_at=31%2F12%2F",input$end,"&search_sort=relevance_desc")}
    plot=ggplot(total, aes(x=x, y=y, size = count,color=count,text=hovers)) +
      geom_point(alpha=0.7)+
      scale_size(range = c(1, 24))+
      scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
      geom_text(aes(x=x,y=y,label=mot),size=3,color="black")+
      coord_fixed(xlim=c(min(total$x)-2,max(total$x)+2),ylim=c(min(total$y)-2,max(total$y)+2))+
      labs(x = "", y = "") + theme_void()+
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position="none"
      )
    return(plot)
  }
  
  tableau$scale<-tableau$ratio
  
  for(mot in unique(tableau$mot)){
    z = which(tableau$mot==mot)
    tableau$scale[z]=scale(tableau$scale[z],center = F,scale = T)
  }
  if(input$scale==TRUE |input$multicourbes==TRUE){tableau$ratio = tableau$scale}
  
  if(input$delta==T | input$fraction==T){
    mots<-str_split(input$mot,"&")
    x = 1:sum(tableau$mot==unlist(mots)[1])
    if(input$delta==T){
      tableau$ratio[tableau$mot==unlist(mots)[1]]<-tableau$ratio[tableau$mot==unlist(mots)[1]]-tableau$ratio[tableau$mot==unlist(mots)[2]]
      Title = paste("Freq(",unlist(mots)[1],") - Freq(",unlist(mots)[2],")")
    }
    if(input$fraction==T){
      tableau$ratio[tableau$mot==unlist(mots)[1]]<-tableau$ratio[tableau$mot==unlist(mots)[1]]/tableau$ratio[tableau$mot==unlist(mots)[2]]
      Title = paste("Freq(",unlist(mots)[1],") / Freq(",unlist(mots)[2],")")
    }
    tableau<-tableau[tableau$mot==unlist(mots)[1],]
  }
  
  
  tableau$loess=tableau$ratio
  width = length(unique(tableau$date))
  span = 2/width + input$span*(width-2)/(10*width)
  
  if(input$span >0){
    if(input$loess==F){
      for(mot in unique(tableau$mot)){
        z = which(tableau$mot==mot)
        for(i in 1:length(z)){
          j = max(i-floor(input$span/2),0)
          k = i+ceiling(input$span/2)
          if(is.na(tableau$ratio[i])){
            next
          }
          pond = tableau$base[z][j:k]
          tableau$loess[z][i] = sum(tableau$ratio[z][j:k]*pond/sum(pond,na.rm = T),na.rm=T)
          ##Si ça bug, remettre le na.rm=T
        }}
    }
    if(input$loess==T){
      for(mot in unique(tableau$mot)){
        z = which(tableau$mot==mot)
        x = 1:length(z)
        tableau$loess[z] = loess(tableau$loess[z]~x,span=span)$fitted
      }
    }
    
  }
  
  
  
  spline.d=z=data.frame(x=0, y=0, mot="")
  spline.d<-spline.d[-1,]
  
  for(mot in unique(tableau$mot)){
    z = which(tableau$mot==mot)
    d = seq.Date(ymd(tableau$date[1]), ymd(tableau$date[length(tableau$date)]), by = 1)
    SF<-splinefun(tableau$date[z], tableau$loess[z])
    ba<-cbind(as.data.frame(d),SF(d))
    colnames(ba)<-c("x","y")
    ma<- bind_cols(ba,mot)
    spline.d<-rbind(spline.d,ma)
  }
  colnames(spline.d)<-c("x","y","mot")
  
  numGroups=3
  if(length(unique(tableau$mot))>=3){numGroups <- length(unique(tableau$mot))}
  customPalette <- brewer.pal(numGroups, "Set1")
  customPalette = customPalette[c(2,1,3:numGroups)]
  if(length(unique(tableau$mot))==1){
    customPalette <- brewer.pal(numGroups, "Set1")
    customPalette = customPalette[c(2)]
  }
  if(length(unique(tableau$mot))==2){
    customPalette <- brewer.pal(numGroups, "Set1")
    customPalette = customPalette[c(2,1)]
  }
  
  if(input$visualiseur==11){
    total<-tableau
    total$hovers<-str_c(total$mot," : ",total$hovers)
    total<-total%>%group_by(mot)
    total$date=as.Date.character(total$date)
    total$mot=as.factor(total$mot)
    total$date=as.factor(total$date)
    total=total[order(total$mot,total$date),]
    plot=ggplot(total,aes(x=date,fill=ratio,y=mot))+
      geom_tile()+
      scale_fill_fermenter(palette="RdBu")+
      # scale_x_date(breaks = date_breaks("2 year"),
      #              labels = date_format("%Y"))+
      labs(x = "", y = "")+theme_tufte() +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x= element_blank(),
        axis.line.y= element_blank(),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position="none"
      )
    return(plot)
  }
  
  
  if(input$visualiseur==3){
    plot=ggplot(data=tableau, aes(x = date, y = count, group=mot,fill=mot))+geom_bar(stat="identity",position=position_dodge())+xlab("")+ylab("")+
      theme_tufte()+ scale_fill_manual(values=customPalette)+
      theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.size = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
  }
  else{
    if(input$visualiseur==5){
      plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+geom_area(aes(fill=mot),alpha=0.3)+facet_wrap(~mot,ncol = 1)+xlab("")+ylab("")+
        theme_tufte()+ scale_color_manual(values=customPalette,name=NULL)+ scale_fill_manual(values=customPalette)+
        theme(plot.background = element_rect(fill = 'white', colour = 'white'),panel.margin.y = unit(0, "lines"),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),strip.background = element_blank(), strip.text.x = element_blank(),axis.line.x = element_line(colour = "black"),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
      if(length(unique(tableau$mot))>9){
        plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+geom_area(aes(fill=mot),alpha=0.3)+facet_wrap(~mot,ncol = 1)+xlab("")+ylab("")+
          theme_tufte()+scale_color_discrete(name=NULL)+
          theme(plot.background = element_rect(fill = 'white', colour = 'white'),panel.margin.y = unit(0, "lines"),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),strip.background = element_blank(), strip.text.x = element_blank(),axis.line.x = element_line(colour = "black"),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
      }
    }else if(input$scale==TRUE |input$multicourbes==TRUE){
      plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+ geom_line(data = spline.d,aes(x=x,y=y,group=mot,color=mot),size=.7)+xlab("")+ylab("")+
        theme_tufte()+ scale_color_manual(values=customPalette)+
        theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.line.x = element_line(colour = "black"),axis.text.y = element_blank(),axis.ticks.y = element_blank(),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=6),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
    }
    else if (input$span==0 | input$points==F | input$doc_type==0 | input$multicourbes==T){
      if (input$spline==T & input$doc_type!=0 & input$multicourbes==F){plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot)) + geom_line(data = spline.d,aes(x=x,y=y,group=mot,color=mot),size=.7)+xlab("")+ylab("")+
        theme_tufte()+ scale_color_manual(values=customPalette)+
        theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))}
      else{plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot)) + geom_line(aes(color=mot),size=.7)+xlab("")+ylab("")+
        theme_tufte()+ scale_color_manual(values=customPalette)+
        theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))}
    }
    else{
      plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+geom_point(data=tableau, aes(x = date, y = ratio, group=mot,color=mot),size=.5, alpha=.5) + geom_line(data = spline.d,aes(x=x,y=y,group=mot,color=mot),size=.7)+
      xlab("Date")+ylab("Fréquence dans le corpus")+
      theme_tufte()+ scale_color_manual(values=customPalette)+
      theme(plot.background = element_rect(fill = 'white', colour = 'white'),
            axis.line.x = element_line(colour = "black"),
            axis.line.y = element_line(colour = "black"),
            legend.title= element_blank(),legend.position="bottom", 
            legend.box = "horizontal",legend.text = element_text(size=8),
            legend.justification="left", legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines')) +
    guides(color=guide_legend(nrow=2, byrow=TRUE))
      }
  }
  
  if(input$fraction==TRUE | input$delta==TRUE){
    plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+ geom_line(data = spline.d,aes(x=x,y=y,group=mot,color=mot),size=.7)+xlab("")+ylab("")+
      theme_tufte()+ scale_color_manual(labels=Title,values = customPalette)+
      theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
  }
  if((input$fraction==TRUE | input$delta==TRUE) & input$scale==T){
    plot=ggplot(data=tableau, aes(x = date, y = loess, group=mot))+ geom_line(data = spline.d,aes(x=x,y=y,group=mot,color=mot),size=.7)+xlab("")+ylab("")+
      theme_tufte()+ scale_color_manual(labels=Title,values = customPalette)+
      theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.line.x = element_line(colour = "black"),axis.line.y = element_blank(),legend.title= element_blank(),legend.position="bottom", legend.box = "horizontal",legend.text = element_text(size=8),legend.justification="left", legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10),legend.key.height = unit(.5, 'lines'))+guides(color=guide_legend(nrow=2, byrow=TRUE))
  }
  return(plot)
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
    url<-str_c("https://gallica.bnf.fr/services/ContentSearch?ark=",tot_df$ark[i],"&query=%22",mot,"%22")
    resultat<-as.character(read_html(RETRY("GET",url,times = 3)))
    resultat=str_remove_all(resultat,"[:space:]")
    resultat=str_remove_all(resultat,".+countresults")
    resultat=str_remove_all(resultat,"searchtime.+")
    resultat=str_extract(resultat,"[:digit:]+")
    tot_df$resultats[i]<-as.integer(resultat)
    
    if(doc_type==4 | doc_type == 3){
      url_base<-str_c("https://gallica.bnf.fr/services/ContentSearch?ark=",tot_df$ark[i],"&query=%22%20%22")
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
  
  if (doc_type==2){
    a<-data.frame(date=min(tableau$date):max(tableau$date))
    a$date<-as.character(a$date)
    tableau<-left_join(a,tableau,by="date")
  }
  tableau$count<-0
  tableau$detect<-0
  tableau$count_base<-0
  tableau$mot<-mot_init
  for (j in 1:length(tableau$date)) {
    tableau$count[j]<-sum(tot_df$resultats[tot_df$date==tableau$date[j]])
    tableau$count_base[j]<-sum(tot_df$resultats_base[tot_df$date==tableau$date[j]])
    tableau$detect[j]<-sum(tot_df$detect[tot_df$date==tableau$date[j]])
    
    if(doc_type==2){tableau$url[j]<-str_c(url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",tableau$date[j],"%22%20and%20gallicapublication_date%3C=%22",tableau$date[j],"%22)&suggest=10&keywords=",mot1,or_end))}
    if(doc_type == 3){
      beginning<-str_replace_all(tableau$date[j],"-","/")
      end<-str_replace_all(tableau$date[j],"-","/")
      if(resolution=="Années"){
        beginning=str_c(beginning,"/01/01")
        end=str_c(end,"/12/31")}
      tableau$url[j]<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=20&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(text%20adj%20%22",mot,"%22%20)%20and%20(gallicapublication_date%3E=%22",beginning,"/01%22%20and%20gallicapublication_date%3C=%22",end,"/31%22)sortby%20dc.date%20")}
    if(doc_type==4){tableau$url[j]<-"https://gallica.bnf.fr/"}
  }
  
  
  colnames(tableau)<-c("date","base","page_count","count","page_base","mot","url")
  
  tableau$ratio<-tableau$count/tableau$base
  tableau$ratio_page<-tableau$page_count/tableau$page_base
  ## On laisse les NA quand il n'y a pas d'archives
  #tableau$ratio[is.na(tableau$ratio)]<-0
  tableau$ratio_page[is.na(tableau$ratio_page)]<-0
  tableau$date<-str_replace_all(tableau$date,"-","/")
  
  tableau_page<-select(tableau,date,page_count,page_base,mot,url,ratio_page)
  tableau_page$resolution<-resolution
  if(doc_type==2){tableau_page$corpus="Livres"
  tableau_page$langue="Français"
  tableau_page$bibli="Gallica"}
  if(doc_type == 3){tableau_page$corpus="Titres de presse"
  tableau_page$langue="Français"
  tableau_page$bibli="Gallica"}
  if(doc_type==4){tableau_page$corpus="Corpus personnalisé"
  tableau_page$langue="Français"
  tableau_page$bibli="Gallica"}
  tableau_page$search_mode<-"Page"
  colnames(tableau_page)<-c("date",	"count",	"base",	"mot",	"url",	"ratio",	"resolution",	"corpus","langue","bibli",	"search_mode")
  
  tableau_volume<-select(tableau,date,count,base,mot,url,ratio)
  tableau_volume$resolution<-resolution
  if(doc_type==2){tableau_volume$corpus="Livres"
  tableau_volume$langue="Français"
  tableau_volume$bibli="Gallica"}
  if(doc_type == 3){tableau_volume$corpus="Titres de presse"
  tableau_volume$langue="Français"
  tableau_volume$bibli="Gallica"}
  if(doc_type==4){tableau_volume$corpus="Corpus personnalisé"
  tableau_volume$langue="Français"
  tableau_volume$bibli="Gallica"}
  tableau_volume$search_mode<-"Document"
  colnames(tableau_volume)<-c("date",	"count",	"base",	"mot",	"url",	"ratio",	"resolution",	"corpus","langue","bibli",	"search_mode")
  
  if(doc_type==4){memoire<<-bind_rows(tableau_volume,memoire)}
  memoire<<-bind_rows(tableau_page,memoire)
  
  data = list(tableau_volume,tableau_page,paste(mot),resolution)
  names(data) = c("tableau_volume","tableau_page","mot","resolution")
  return(data)
}


cloudify<-function(input){
  show_spinner(spin_id = "ngram")
  # 
  # if(input$doc_type==2){ngram_file<-str_c("/mnt/persistent/1gram.db")}
  # if(input$doc_type==1){ngram_file<-str_c("/mnt/persistent/1gram_presse.db")}
  # if(input$doc_type==30){ngram_file<-str_c("/mnt/persistent/1gram_lemonde.db")}
  # con=dbConnect(RSQLite::SQLite(),dbname = ngram_file)
  # 
  # if(input$doc_type==1 | input$doc_type==2){query = dbSendQuery(con,str_c("select sum(n) as tot, gram from gram where annee between ",input$beginning," and ",input$end," group by gram order by tot desc limit 1200;"))}
  # if(input$doc_type==30){query = dbSendQuery(con,str_c("select sum(n) as tot, gram from gram_mois where annee between ",input$beginning," and ",input$end," group by gram_mois order by tot desc limit 1200;"))}
  # 
  # w = dbFetch(query)
  # dbDisconnect(con)
  # 
  # # stpw = read.csv("stopwords.csv",row.names=1,stringsAsFactors=F)
  # # stpw<-as.data.frame(stpw$monogram)
  # # colnames(stpw)<-c("mot")

  w=bind_cols(c(1000000,100000),c("de","la"))
  colnames(w)<-c("tot","gram")
  
  w<-bind_cols(w$gram,w$tot)
  colnames(w)<-c("mot","count")
  
  set.seed(42)
  cl<-ggplot(w, aes(label = mot, size = count)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 24) +
    theme_minimal()

  # # data = list(w,"Année")
  # # names(data) = c("tableau","resolution")

    hide_spinner(spin_id="ngram")
    return(cl)
  
}



jokerize<-function(input){
  
  show_spinner(spin_id="ngram")
  
  mot<-str_remove_all(input$mot,"&.+")
  mot<-str_remove_all(mot,"[+].+")
  mot<-str_remove_all(mot,"[+]")
  mot<-str_remove_all(mot,"&")
  
  if(str_detect(mot,"_.+")){pos<-"avant"}
  else{pos<-"apres"}
  
  mot<-str_remove_all(mot,"_")
  
  table<-unnest_tokens(as.data.frame(mot),ngram,mot, token = "ngrams", n = 1)
  nb<-length(table$ngram)
  mot<-table$ngram[1]
  if(nb>1){for(x in 2:nb){mot<-str_c(mot," ",table$ngram[x])}}
  
  if(input$doc_type==30){
    if(nb>3){next}
    if(nb<=3){
      ngram_file<-str_c("/mnt/persistent/",nb+1,"gram_lemonde.db")
      gram<-"gram"
      if(nb==1){base<-read.csv("lemonde2.csv")}
      if(nb==2){base<-read.csv("lemonde3.csv")}
      if(nb==3){base<-read.csv("lemonde4.csv")}
    }
  }
  
  if(input$doc_type==2){
    if(nb>4){next}
    if(nb<=4){
      ngram_file<-str_c("/mnt/persistent/",nb+1,"gram.db")
      if(nb==1){gram<-"bigram"
      base<-read.csv("base_livres_gallica_bigrammes.csv")}
      if(nb==2){gram<-"trigram"
      base<-read.csv("base_livres_gallica_trigrammes.csv")}
      if(nb==3){gram<-"tetragram"
      base<-read.csv("base_livres_gallica_tetragrammes.csv")}
      if(nb==4){gram<-"pentagram"
      base<-read.csv("base_livres_gallica_pentagrammes.csv")}
    }
  }
  if(input$doc_type==1){
    if(nb<=2){
      ngram_file<-str_c("/mnt/persistent/",nb+1,"gram_presse.db")
      gram<-"gram"
      if(input$resolution=="Année"){
        base<-read.csv("base_presse_annees_gallica_monogrammes.csv")}
      if(input$resolution=="Mois"){
        base<-read.csv("base_presse_mois_gallica_monogrammes.csv")}
    }
    if(nb>2){next}
  }
  
  
  con=dbConnect(RSQLite::SQLite(),dbname = ngram_file)
  if(!dbExistsTable(con,"gram")){
    next
  }
  
  if(pos=="apres"){
    query = dbSendQuery(con,str_c('select sum(n) as tot, ',gram,' from ',gram,' where annee between ',input$beginning,' and ',input$end,' and rowid in (select rowid from full_text where gram'," match '^",'"',mot,'"',"') group by ",gram,' order by tot desc limit ',20+input$nbJoker+input$stpw))
  }
  if(pos=="avant"){
    query = dbSendQuery(con,str_c('select sum(n) as tot, ',gram,' from ',gram,' where annee between ',input$beginning,' and ',input$end,' and rowid in (select rowid from full_text where gram'," match '",'"',mot,'"', "') group by ",gram,' order by tot desc limit ',3000+input$nbJoker+input$stpw))
  }
  if(input$doc_type==30)
  {
    if(pos=="apres"){
      query = dbSendQuery(con,str_c('select sum(n) as tot, gram from gram_mois where annee between ',input$beginning,' and ',input$end,' and rowid in (select rowid from full_text where gram'," match '^",'"',mot,'"',"') group by ",gram,' order by tot desc limit ',20+input$nbJoker+input$stpw))
    }
    if(pos=="avant"){
      query = dbSendQuery(con,str_c('select sum(n) as tot, gram from gram_mois where annee between ',input$beginning,' and ',input$end,' and rowid in (select rowid from full_text where gram'," match '",'"',mot,'"', "') group by ",gram,' order by tot desc limit ',3000+input$nbJoker+input$stpw))
    }
  }
  w = dbFetch(query)
  dbDisconnect(con)
  
  stpw = read.csv("stopwords.csv",stringsAsFactors=F)[0:input$stpw,]
  
  colnames(w)<-c("tot","gram")
  gram<-"gram"
  
  jokertable<-w
  jokertable<-jokertable[str_detect(jokertable$gram,"&")==F,]
  
  if(pos=="apres"){
    jokertable<-jokertable[str_detect(jokertable$gram,str_c("^",mot)),]
    z = unlist(jokertable[gram]) %in% paste(mot,stpw$monogram)
    jokertable<-jokertable[!z,]
    jokertable<-jokertable[1:input$nbJoker,]
  }
  if(pos=="avant"){
    jokertable<-jokertable[str_detect(jokertable$gram,str_c("^",mot))==F,]
    paste(stpw$monogram,mot)
    z = unlist(jokertable[gram]) %in% paste(stpw$monogram,mot)
    jokertable<-jokertable[!z,]
    jokertable<-jokertable[1:input$nbJoker,]
  }
  if(is.na(jokertable$tot[1])){
    jokertable<-data.frame(tot=0,gram=mot)
  }
  jokertable<-jokertable[is.na(jokertable$tot)==F,]
  
  
  hide_spinner(spin_id = "ngram")
  
  return(jokertable)
  
}

ngramize<-function(input,nouvrequette,gallicagram,agregator){
  print("blaaa")
  show_spinner(spin_id="ngram")
  require("RSQLite")
  require("DBI")
  from<-input$beginning
  to<-input$end
  
  if(gallicagram==1 & to>1950){to=1950}
  if(gallicagram==1 & from<1789){from=1789}
  if(gallicagram==2 & to>2022){to=2022}
  if(gallicagram==2 & from<1945){from=1945}
  
  if(input$resolution=="Semaine"){
    from=min(input$dateRange)
    to<-max(input$dateRange)
    to<-str_replace_all(to,"-","/")
    from<-str_replace_all(from,"-","/")
  }
  
  
  if(input$joker==F){mots = str_split(input$mot,"&")[[1]]}
  if(input$joker==T){mots = str_split(nouvrequette,"&")[[1]]}
  
  increment<-1
  
  for(mot1 in mots){
    
        mots2 = str_split(mot1,"[+]")[[1]]
    z = grep("^[aeiouéh]",mots2)
    xxxx=str_c("l'",mots2[z])
    if(length(z)>0){mots2=append(mots2,xxxx)}
    mots2=mots2[!duplicated(mots2)]
    print(mots2)
    increment2<-1
    for(mot in mots2){
      
      
      table<-unnest_tokens(as.data.frame(mot),ngram,mot, token = "ngrams", n = 1)
      nb<-length(table$ngram)
      mot<-table$ngram[1]
      if(nb>1){for(x in 2:nb){mot<-str_c(mot," ",table$ngram[x])}}
      if(input$doc_type==2 | (input$doc_type==56 & agregator==2)){
        if(nb>5){z=data.frame(date=from:to, count=0, base=0,ratio=NA)
        next}
        if(nb<=5){
          ngram_file<-str_c("/mnt/persistent/",nb,"gram.db")
          if(nb==1){gram<-"gram"
          base<-read.csv("base_livres_gallica_monogrammes.csv")}
          if(nb==2){gram<-"gram"
          base<-read.csv("base_livres_gallica_bigrammes.csv")}
          if(nb==3){gram<-"gram"
          base<-read.csv("base_livres_gallica_trigrammes.csv")}
          if(nb==4){gram<-"gram"
          base<-read.csv("base_livres_gallica_tetragrammes.csv")}
          if(nb==5){gram<-"gram"
          base<-read.csv("base_livres_gallica_pentagrammes.csv")}
        }
      }
      if(input$doc_type==1 | gallicagram==1 | (input$doc_type==56 & agregator==1)){
        if(nb<=3){
          ngram_file<-str_c("/mnt/persistent/",nb,"gram_presse.db")
          gram<-"gram"
          if(input$resolution=="Année"){
            base<-read.csv("base_presse_annees_gallica_monogrammes.csv")}
          if(input$resolution=="Mois"){
            base<-read.csv("base_presse_mois_gallica_monogrammes.csv")}
        }
        if(nb>3){z=data.frame(date=from:to, count=0, base=0,ratio=0)
        next}
      }
      if(input$doc_type %in% 77:78){
        if(nb<3){
        gram<-"gram"
        if(input$doc_type==77){ngram_file=str_c("/mnt/persistent/",nb,"gram_subtitles.db")
        base=read.csv(str_c("subtitles",nb,".csv"))}
        if(input$doc_type==78){ngram_file=str_c("/mnt/persistent/",nb,"gram_subtitles_en.db")
        base=read.csv(str_c("subtitles_en",nb,".csv"))}
        colnames(base)<-c("base","date")
        }
        else{z=data.frame(date=from:to, count=0, base=0,ratio=NA)
          next}
      }
      if(input$doc_type==30 | gallicagram==2){
        if(nb<=4){
        gram<-"gram"
        ngram_file<-str_c("/mnt/persistent/",nb,"gram_lemonde.db")
        base<-read.csv(str_c("lemonde",nb,".csv"))
        }
        base$mois[str_length(base$mois)==1]<-str_c("0",base$mois[str_length(base$mois)==1])
        base$jour[str_length(base$jour)==1]<-str_c("0",base$jour[str_length(base$jour)==1])
        if(input$resolution=="Année"){
          base<-base%>%group_by(annee)%>%summarise(n = sum(n))
          colnames(base)<-c("date","base")}
        if(input$resolution=="Mois"){
          base<-base%>%group_by(annee,mois)%>%summarise(n = sum(n))
          base<-cbind(str_c(base$annee,"/",base$mois),base$n)
          colnames(base)<-c("date","base")}
        if(input$resolution=="Semaine"){
          base<-cbind(str_c(base$annee,"/",base$mois,"/",base$jour),base$n)
          colnames(base)<-c("date","base")}
      if(nb>4){z=data.frame(date=from:to, count=0, base=0,ratio=NA)
      next}
      }
      if( input$doc_type %in% 66:76){
        if(nb<=2){
          gram<-"gram"
          if(input$doc_type==66){ngram_file=str_c("/mnt/persistent/",nb,"gram_figaro.db")
          base=read.csv(str_c("figaro",nb,".csv"))}
          if(input$doc_type==67){ngram_file<-str_c("/mnt/persistent/",nb,"gram_huma.db")
            base <- read.csv(str_c("humanite",nb,".csv"))}
          if(input$doc_type==68){ngram_file<-str_c("/mnt/persistent/",nb,"gram_constitutionnel.db")
          base <- read.csv(str_c("constitutionnel",nb,".csv"))}
          if(input$doc_type==69){ngram_file <-str_c ("/mnt/persistent/",nb,"gram_paris.db")
          base <- read.csv(str_c("paris",nb,".csv"))}
          if(input$doc_type==70){ngram_file=str_c("/mnt/persistent/",nb,"gram_moniteur.db")
          base=read.csv(str_c("moniteur",nb,".csv"))}
          if(input$doc_type==71){ngram_file=str_c("/mnt/persistent/",nb,"gram_temps.db")
          base=read.csv(str_c("temps",nb,".csv"))}
          if(input$doc_type==72){ngram_file=str_c("/mnt/persistent/",nb,"gram_petit_journal.db")
          base=read.csv(str_c("petit_journal",nb,".csv"))}
          if(input$doc_type==73){ngram_file=str_c("/mnt/persistent/",nb,"gram_petit_parisien.db")
          base=read.csv(str_c("petit_parisien",nb,".csv"))}
          if(input$doc_type==74){ngram_file=str_c("/mnt/persistent/",nb,"gram_journal_officiel.db")
          base=read.csv(str_c("journal_officiel",nb,".csv"))}
          if(input$doc_type==75){ngram_file=str_c("/mnt/persistent/",nb,"gram_journal_des_debats.db")
          base=read.csv(str_c("journal_des_debats",nb,".csv"))}
          if(input$doc_type==76){ngram_file=str_c("/mnt/persistent/",nb,"gram_la_presse.db")
          base=read.csv(str_c("la_presse",nb,".csv"))}
          
          base$mois[str_length(base$mois)==1]<-str_c("0",base$mois[str_length(base$mois)==1])
          base$jour[str_length(base$jour)==1]<-str_c("0",base$jour[str_length(base$jour)==1])
          if(input$resolution=="Année"){
            base<-base%>%group_by(annee)%>%summarise(n = sum(n))
            colnames(base)<-c("date","base")}
          if(input$resolution=="Mois"){
            base<-base%>%group_by(annee,mois)%>%summarise(n = sum(n))
            base<-cbind(str_c(base$annee,"/",base$mois),base$n)
            colnames(base)<-c("date","base")}
          if(input$resolution=="Semaine"){
            base<-cbind(str_c(base$annee,"/",base$mois,"/",base$jour),base$n)
            colnames(base)<-c("date","base")}
        }
        if(nb>2){z=data.frame(date=from:to, count=0, base=0,ratio=NA)
        next}
      }
      if(input$doc_type == 43){
        if(nb<=2){
          ngram_file<-str_c("/mnt/persistent/",nb,"gram_ddb.db")
          gram<-"gram"
          base<-read.csv(str_c("ddb",nb,".csv"))
          if(input$resolution == "Année"){
            base = base %>% group_by(annee) %>% summarise(base=sum(n))
            colnames(base)[1] = "date"
            #base$date = as.character(base$date)
          } else{
          zz = base$mois < 10
          base$mois[zz] = paste("0",base$mois[zz],sep="")
          base$date = paste(base$annee,base$mois,sep="/")
          base = base[c("n","date")]
          colnames(base)[1] = "base"}
        }
      }
      if(input$doc_type == 99){
        if(nb<4){
        ngram_file<-str_c("/mnt/persistent/",nb,"gram_american_stories.db")
        gram<-"gram"
        base<-read.csv(str_c("american_stories",nb,".csv"))
        colnames(base) = c("base","date")}
        else{z=data.frame(date=from:to, count=0, base=0,ratio=NA)
        next}
      }
      
      base<-as.data.frame(base)

      if(input$resolution=="Année"){
        base<-base[base$date<=to,]
        base<-base[base$date>=from,]
      }
      if(input$resolution=="Mois"){
        base<-base[base$date<=str_c(to,"/12"),]
        base<-base[base$date>=str_c(from,"/01"),]
      }
      
      if(input$resolution=="Semaine"){
        base<-base[base[,"date"]<=to,]
        base<-base[base[,"date"]>=from,]
      }

      con=dbConnect(RSQLite::SQLite(),dbname = ngram_file)
      if(!dbExistsTable(con,"gram")){
        z=data.frame(date=from:to, count=0, base=0,ratio=NA)
        next
      }
      
      if(input$doc_type==2 | (input$doc_type==56 & agregator==2)){
        query = dbSendQuery(con,str_c('SELECT n,annee FROM ',gram,' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'"'))
        w = dbFetch(query)
        print(w)
      }
      if((input$doc_type==1 | input$doc_type==30 | input$doc_type==0 |
          input$doc_type %in% 66:76 | input$doc_type %in% 77:78 | input$doc_type==43 | (input$doc_type==99&nb<4)|
          (input$doc_type==56 & agregator==1)) & input$resolution=="Année"){
        #q=str_c('SELECT n,annee FROM gram',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'"')
        q=str_c('SELECT sum(n),annee FROM gram',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'" group by annee')
        if(input$doc_type==30 | input$doc_type %in% 66:76){
          q=str_c('SELECT sum(n),gram,annee,mois FROM gram_mois',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'" group by annee')
        }
        query = dbSendQuery(con,q)
        w = dbFetch(query)
        if(input$doc_type==30 | input$doc_type %in% 66:76){
          w<-w[,-2]
          w<-w[,-3]
          
        }
        colnames(w)<-c("n","annee")
        w = group_by(w,annee) %>% summarise(n = sum(as.integer(n)))
        w$annee = as.integer(w$annee)
      }
      if((input$doc_type==1 | input$doc_type==30 | input$doc_type==0 | input$doc_type==43| input$doc_type %in% 66:76)  & input$resolution=="Mois"){
        # q=str_c('SELECT * FROM gram',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'"')
        q=str_c('SELECT sum(n),annee,mois FROM gram',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'" group by annee,mois')
        if(input$doc_type==30 | input$doc_type %in% 66:76){
          q=str_c('SELECT * FROM gram_mois',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'"')
          
        }
        query = dbSendQuery(con,q)
        w = dbFetch(query)
        if(input$doc_type==30 | input$doc_type %in% 66:76){
          w<-w[,-2]
        }
        colnames(w)<-c("n","annee","mois")
        w$n = as.integer(w$n)
        if(length(w$mois)>0){
          for (i in 1:length(w$mois)) {if(str_length(w$mois[i])==1){w$mois[i]<-str_c("0",w$mois[i])}}
        }
        w$annee<-str_c(w$annee,"/",w$mois)
        w<-w[,-3]
      }
      
      if((input$doc_type==30 | input$doc_type %in% 66:76 ) &
          input$resolution=="Semaine"){
        q=str_c('SELECT * FROM gram',' WHERE annee BETWEEN ',str_split(from,"/")[[1]][1]," AND ",str_split(to,"/")[[1]][1] ,' AND ',gram,'="',mot,'"')
        query = dbSendQuery(con,q)
        w = dbFetch(query)
        w<-w[,-2]
        w$n = as.integer(w$n)
        if(length(w$mois)>0){
        for (i in 1:length(w$mois)) {if(str_length(w$mois[i])==1){w$mois[i]<-str_c("0",w$mois[i])}
          if(str_length(w$jour[i])==1){w$jour[i]<-str_c("0",w$jour[i])}}
        }
        w$annee<-str_c(w$annee,"/",w$mois,"/",w$jour)
        w<-w[,-3]
        w<-w[,-3]
      }
      
      dbDisconnect(con)
      

      if(input$resolution=="Année"){
        y=data.frame(annee=from:to, n=0)
      }
      if(input$resolution=="Mois"){
        y=data.frame(annee="AAAA/MM", n=0)
        for (i in str_split(from,"/")[[1]][1]:str_split(to,"/")[[1]][1]) {
          for (j in 1:12) {
            if(j<=9){k=str_c(0,j)}
            else{k=j}
            zz=as.data.frame(cbind(str_c(i,"/",k),0))
            colnames(zz)=c("annee","n")
            zz$n<-as.integer(zz$n)
            y=bind_rows(y,zz)
          }
          
        }
        y<-y[-1,]
      }
      if(input$resolution=="Semaine"){
        y=data.frame(annee=seq(as.Date(from),as.Date(to),by="day"), n=0)
        y$annee<-str_replace_all(y$annee,"-","/")
      }

      w=left_join(y,w,by="annee")
      
      w<-w[,-2]
      w<-w[,-3]
      colnames(w)=c("date","count")
      w$count[is.na(w$count)]<-0
      w<-w%>%group_by(date)%>%summarise(count = sum(count))

      
      w = left_join(w,as.data.frame(base),by="date")
      
      w$base<-as.numeric(w$base)
      if(input$resolution=="Semaine"){
        w$date<-as.Date(w$date)
        w<-w%>%
          summarise_by_time(
            .date_var = date,
            .by       = "week",
            count  = sum(count),
            base=sum(base)
          )
        w$date<-str_replace_all(w$date,"-","/")
      }
      w$ratio=w$count/w$base
      #On laisse les NA
      #w$ratio[is.na(w$ratio)]<-0
      #w$ratio[is.infinite(w$ratio)]<-0
      if(increment2==1){z=w}
      else
      {
        z$count=z$count+w$count
        z$base[is.na(z$base)]<-0
        if(sum(z$base)==0){z$base=w$base}
        z$ratio=z$ratio+w$ratio
      }
      increment2=increment2+1
    }
    #z$ratio[is.na(z$ratio)]<-0
    #z$ratio[is.infinite(z$ratio)]<-0
    z$mot<-mot1
    mot2<-mot1
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
    if(input$doc_type==2){
      z$url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",z$date,"%22%20and%20gallicapublication_date%3C=%22",z$date,"%22)&suggest=10&keywords=",mot1,or_end)
    }
    if(input$doc_type==56){
      z$url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22monographie%22%20or%20dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",z$date,"%22%20and%20gallicapublication_date%3C=%22",z$date,"%22)&suggest=10&keywords=",mot1,or_end)
    }
    if( (input$doc_type==1 | gallicagram==1) & input$resolution=="Année"){
      z$url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",z$date,"%22%20and%20gallicapublication_date%3C=%22",z$date,"%22)&suggest=10&keywords=",mot1,or_end)
    }
    if((input$doc_type==1 | gallicagram==1) & input$resolution=="Mois"){
      z$url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",z$date,"/01%22%20and%20gallicapublication_date%3C=%22",z$date,"/31%22)&suggest=10&keywords=",mot1,or_end)
    }
    if((input$doc_type==30 | gallicagram==2) & input$resolution=="Année"){
      z$url<-str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",mot1,"%22&start_at=01%2F01%2F",z$date,"&end_at=31%2F12%2F",z$date,"&search_sort=relevance_desc")
    }
    if((input$doc_type==30 | gallicagram==2) & input$resolution=="Mois"){
      z$url<-str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",mot1,"%22&start_at=01%2F",str_extract(z$date,"..$"),"%2F",str_extract(z$date,"...."),"&end_at=31%2F",str_extract(z$date,"..$"),"%2F",str_extract(z$date,"...."),"&search_sort=relevance_desc")
      #z<-z[z$date<="2022/08",]
    }
    if(input$doc_type==30 & input$resolution=="Semaine"){
      z$url<-str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",mot1,"%22&start_at=",substr(z$date,9,10),"%2F",substr(z$date,6,7),"%2F",str_extract(z$date,"...."),"&end_at=",substr(z$date,9,10),"%2F",substr(z$date,6,7),"%2F",str_extract(z$date,"...."),"&search_sort=relevance_desc")
      #z<-z[z$date<="2022/08/31",]
    }
    if(input$doc_type==43 & input$resolution=="Année"){
      z$url = str_c("https://www.deutsche-digitale-bibliothek.de/search/newspaper?query=%22",mot1,"%22&language=ger&fromDay=1&fromMonth=1","&fromYear=",z$date,"&toDay=31&toMonth=12&toYear=",z$date)
    }
    if(input$doc_type==43 & input$resolution=="Mois"){
      end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
      mois = as.integer(str_extract(z$date,"..$"))
      z$url = str_c("https://www.deutsche-digitale-bibliothek.de/search/newspaper?query=%22",mot1,"%22&language=ger&fromDay=1&fromMonth=",mois,"&fromYear=",str_extract(z$date,"...."),"&toDay=",end_of_month[mois],"&toMonth=",mois,"&toYear=",str_extract(z$date,"...."))
    }
    if(input$doc_type %in% 66:76){
      codes = c("cb34355551z","cb327877302","cb32747578p","cb327986698","cb34452336z","cb34431794k","cb32895690j","cb34419111x","cb34378481r","cb39294634r","cb34448033b")
      names(codes) = as.character(66:76)
      if(input$resolution=="Année"){z$url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",z$date,"%22%20and%20gallicapublication_date%3C=%22",z$date,"%22%20and%20arkPress%20adj",codes[as.character(input$doc_type)],"_date)&suggest=10&keywords=",mot1,or_end)}
      if(input$resolution=="Mois"){z$url<-str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",z$date,"/01%22%20and%20gallicapublication_date%3C=%22",z$date,"/31%22%20and%20arkPress%20adj",codes[as.character(input$doc_type)],"_date)&suggest=10&keywords=",mot1,or_end)}
    }
    if(input$doc_type %in% 77:78){
      z$url<-str_c("https://www.opensubtitles.org/")
    }
    if(input$doc_type == 99){
      z$url = "https://huggingface.co/datasets/dell-research-harvard/AmericanStories"
    }

    if(input$resolution=="Année"){z$resolution<-"Année"}
    if(input$resolution=="Mois"){z$resolution<-"Mois"}
    if(input$resolution=="Semaine"){z$resolution<-"Semaine"}
    
    if(input$doc_type==2){z$corpus="Livres"
    z$langue="Français"
    z$bibli="Gallica"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==56){z$corpus="Presse et livres"
    z$langue="Français"
    z$bibli="Gallica"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==1){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Gallica"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==30){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Le Monde"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==43){z$corpus="Presse"
    z$langue="Allemand"
    z$bibli="Deutsche Digitale Bibliothek"
    z$search_mode<-"N-gramme"}
    if((input$doc_type==0 & gallicagram==2)){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Le Monde"
    z$search_mode<-"N-gramme"}
    if((input$doc_type==0 & gallicagram==1)){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Gallica"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==66){z$corpus="Le Figaro"
    z$langue="Français"
    z$bibli="Le Figaro"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==67){z$corpus="L'Humanité"
    z$langue="Français"
    z$bibli="L'Humanité"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==68){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Le Constitutionnel"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==69){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Le Journal de Paris"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==70){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Le Moniteur Universel"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==71){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Le Temps"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==72){z$corpus="Presse"
    z$langue="Français"
    z$bibli="Le Petit Journal"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==77){z$corpus="Films"
    z$langue="Français"
    z$bibli="Opensubtitles / Films en français"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==78){z$corpus="Films"
    z$langue="Français"
    z$bibli="Opensubtitles / Films en anglais"
    z$search_mode<-"N-gramme"}
    if(input$doc_type==99){z$corpus="American Stories"
    z$langue="Anglais"
    z$bibli="American Stories"
    z$search_mode<-"N-gramme"}
    
    if(increment==1){tableau=z}
    else{tableau=bind_rows(tableau,z)}
    increment=increment+1
  }
  tableau$date<-as.character(tableau$date)
  if(input$doc_type!=0){memoire<<-bind_rows(tableau,memoire)}
  
  if(input$joker==F){data = list(tableau,paste(input$mot,collapse="&"),input$resolution)}
  if(input$joker==T){data = list(tableau,paste(nouvrequette,collapse="&"),input$resolution)}
  names(data) = c("tableau","mot","resolution")
  
  hide_spinner(spin_id="ngram")
  
  
  return(data)
  
}

get_data <- function(mot,from,to,resolution,doc_type,titres,input,cooccurrences,prox){
  if(doc_type==6 | doc_type==7){mot <- iconv(mot, from="UTF-8",to="ASCII//TRANSLIT//IGNORE")}
  if(input$resolution=="Semaine" & input$doc_type==31){
    from=min(input$dateRange)
    to=max(input$dateRange)
    from=as.integer(str_extract(from,"...."))
    to=as.integer(str_extract(to,"...."))
  }
  mots = str_split(mot,"&")[[1]]
  tableau<-as.data.frame(matrix(nrow=0,ncol=5),stringsAsFactors = FALSE)
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  
  if(doc_type==13 | doc_type==14 | doc_type==28 | doc_type==29 | doc_type==37 | doc_type==38 | doc_type==39 | doc_type==40| doc_type==55 | doc_type==57){
    if(se=="windows"){system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
      require("RSelenium")
      rD <- rsDriver(browser = "firefox", port = 4444L)
      remDr <- rD[["client"]]}
    if(se=="linux"){remDr<-remoteDriver$new(remoteServerAddr = "172.18.0.1", port = 4444L, browserName = "firefox")
    remDr$open()}
  }
  
  if(doc_type==44 | doc_type ==58| doc_type ==59 | doc_type ==60 | doc_type ==61 | doc_type ==62 | doc_type ==63 | doc_type ==64){
    if(from<2004){from=2004}
    if(from>2022){from=2004}
    if(to>2022){to=2022}
    if(to<2004){to=2022}
  }
  
  
  for (i in from:to){
    ###
    end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
    if(i%%4==0){end_of_month[2]=29}
    if(i==1700 | i==1800 | i==1900){end_of_month[2]=28}#Ne pas oublier les années bisextiles (merci Maxendre de m'y avoir fait penser)
    y<-as.character(i)
    if(resolution=="Année"){beginning = str_c(y,"/01/01")
    end = str_c(y,"/12/31")}
    I = 1
    if(resolution=="Mois"){I=1:12} #Pour faire ensuite une boucle sur les mois
    
    
    if(doc_type !=5 & doc_type !=9 & doc_type !=10 & doc_type !=12 & doc_type !=44 & doc_type !=50 & doc_type !=51 & doc_type !=52 & doc_type !=53 & doc_type !=54 & doc_type !=58 & doc_type !=59 & doc_type !=60 & doc_type !=61 & doc_type !=62 & doc_type !=63 & doc_type !=64 & doc_type !=65 & doc_type !=79){
      for(j in I){
        if(resolution=="Mois"){
          z = as.character(j)
          if(nchar(z)<2){z<-str_c("0",z)}
          beginning = str_c(y,"/",z,"/01")
          end = str_c(y,"/",z,"/",end_of_month[j])}
        
        incr_mot=0
        for(mot in mots){
          incr_mot=incr_mot+1
          mot2 = str_replace_all(mot," ","%20")
          if(doc_type!=28){mot2=URLencode(mot2)}
          mot1=""
          
          
          ###
          or<-""
          or_end<-""
          if(str_detect(mot2,"[+]")){
            mots_or = str_split(mot2,"[+]")[[1]]
            or1<-NA
            or1_end<-NA
            
            if(str_detect(mots_or[1],"[*]") & (doc_type==1 | doc_type==2 | doc_type == 3 | doc_type == 56)){
              mots_co = str_split(mots_or[1],"[*]")[[1]]
              mot1<-str_c("(%20text%20adj%20%22",mots_co[1],"%22%20%20prox/unit=word/distance=",prox,"%20%22",mots_co[2],"%22)")
            }
            if(str_detect(mots_or[1],"[*]")==F & (doc_type==1 | doc_type==2 | doc_type == 3 | doc_type == 56)){
              mot1<-str_c("(%20text%20adj%20%22",mots_or[1],"%22%20)")
            }
            
            for (j in 2:length(mots_or)) {
              if(doc_type==1 | doc_type==2 | doc_type == 3 | doc_type == 56)
              {
                if(str_detect(mots_or[j],"[*]")==F){
                  or1[j]<-str_c("or%20(%20text%20adj%20%22",mots_or[j],"%22%20)")
                  if(mot1==""){mot1<-str_c("(%20text%20adj%20%22",mots_or[1],"%22%20)")}
                }
                else{
                  mots_co = str_split(mots_or[j],"[*]")[[1]]
                  or1[j]<-str_c("%20or%20(%20text%20adj%20%22",mots_co[1],"%22%20%20prox/unit=word/distance=",prox,"%20%22",mots_co[2],"%22)")
                }
              }
              if(doc_type==4 | doc_type==19| doc_type==20 | doc_type==21 | doc_type==22 | doc_type==23 | doc_type==24 | doc_type==25 | doc_type==26)
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
              if(doc_type==29)
              {or1[j]<-str_c("+OR+%22",mots_or[j],"%22")
              or1_end[j]<-str_c("")}
              
              or<-str_c(or,or1[j])
              or_end<-str_c(or_end,or1_end[j])
              
            }
            if(doc_type!=1 & doc_type!=2 & doc_type != 3  & doc_type != 56){mot1<-mots_or[1]}
          }
          else{
            if(doc_type==1 | doc_type==2 | doc_type == 3 | doc_type == 56){
              if(str_detect(mot2,"[*]")){
                mots_co = str_split(mot2,"[*]")[[1]]
                mot1<-str_c("(%20text%20adj%20%22",mots_co[1],"%22%20%20prox/unit=word/distance=",prox,"%20%22",mots_co[2],"%22)")
              }
              else{mot1=str_c("(%20text%20adj%20%22",mot2,"%22%20)")}}
            else{mot1=mot2}
            
          }
          if(doc_type==15 | doc_type==16 | doc_type==18){
            mot1<-URLencode(mot1)
            or<-URLencode(or)
          }
          
          
          
          if(doc_type == 1){
            url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(",mot1,or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)")
            url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 56){
            url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(",mot1,or,")%20%20and%20(dc.type%20all%20%22monographie%22%20or%20dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)")
            url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22%20or%20dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 3){
            liste_titres<-titres
            longueur_max=15
            iterations<-ceiling(length(liste_titres)/longueur_max)
            reste=iterations*longueur_max-length(liste_titres)
            a=0
            if(incr_mot==1){b=0}
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
              url <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(",mot1,or,")%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)sortby%20dc.date%20")
              ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
              a<-a+as.integer(str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+"))
              url_base <- str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.relation%20any%20%22",ark1,"%22",ark3,")%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)%20sortby%20dc.date")
              
              if(incr_mot==1){
                ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
                b<-b+as.integer(str_extract(str_extract(ngram_base,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+"))
              }
              if(k==1){url1<-url}
            }
            a<-as.character(a)
            b<-as.character(b)
            url=url1
            titres<-liste_titres
          }
          if(doc_type == 6){beginning<-str_replace_all(beginning,"/","-")
          end<-str_replace_all(end,"/","-")
          langue="de"
          url<-str_c("https://www.europeana.eu/fr/search?page=1&qf=collection%3Anewspaper&qf=proxy_dcterms_issued%3A%5B",beginning,"%20TO%20",end,"%5D&qf=TYPE%3A%22TEXT%22&qf=LANGUAGE%3A%22",langue,"%22&query=%22",mot1,"%22",or,"&view=grid&api=fulltext")
          url_base<-str_c("https://www.europeana.eu/fr/search?page=1&qf=collection%3Anewspaper&qf=proxy_dcterms_issued%3A%5B",beginning,"%20TO%20",end,"%5D&qf=TYPE%3A%22TEXT%22&qf=LANGUAGE%3A%22",langue,"%22&query=&view=grid&api=fulltext")
          }
          if(doc_type == 7){beginning<-str_replace_all(beginning,"/","-")
          end<-str_replace_all(end,"/","-")
          langue="nl"
          url<-str_c("https://www.europeana.eu/fr/search?page=1&qf=collection%3Anewspaper&qf=proxy_dcterms_issued%3A%5B",beginning,"%20TO%20",end,"%5D&qf=TYPE%3A%22TEXT%22&qf=LANGUAGE%3A%22",langue,"%22&query=%22",mot1,"%22",or,"&view=grid&api=fulltext")
          url_base<-str_c("https://www.europeana.eu/fr/search?page=1&qf=collection%3Anewspaper&qf=proxy_dcterms_issued%3A%5B",beginning,"%20TO%20",end,"%5D&qf=TYPE%3A%22TEXT%22&qf=LANGUAGE%3A%22",langue,"%22&query=&view=grid&api=fulltext")
          }
          if(doc_type == 8){beginning<-str_replace_all(beginning,"/","-")
          end<-str_replace_all(end,"/","-")
          url<-str_c("https://www.britishnewspaperarchive.co.uk/search/results/",beginning,"/",end,"?basicsearch=%22",mot1,"%22",or,"&exactsearch=true&contenttype=article")
          url_base<-str_c("https://www.britishnewspaperarchive.co.uk/search/results/",beginning,"/",end,"?basicsearch=a&contenttype=article")
          }
          if(doc_type == 11){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("http://hemerotecadigital.bne.es/results.vm?o=",or,"&w=%22",mot1,"%22",or_end,"&f=text&d=creation&d=",y,"&d=",z,"&d=01&d=",y,"&d=",z,"&d=",end_of_month[j],"&t=%2Bcreation&l=700&s=0&view=&lang=fr")
              url_base<-str_c("http://hemerotecadigital.bne.es/results.vm?d=creation&d=",y,"&d=",z,"&d=01&d=",y,"&d=",z,"&d=",end_of_month[j],"&t=%2Bcreation&l=700&s=0&view=&lang=fr")
              url=str_c("https://hemerotecadigital.bne.es/hd/es/results?o=&w=%22",mot1,"%22&f=text&o=o&w=&f=text&o=n&w=&f=text&o=&w=&f=text&o=o&w=&f=text&o=n&w=&f=text&p=0%7E0&g=p&d=date&d=",y,"-",z,"-01&d=",y,"-",z,"-",end_of_month[j],"&l=10&t=date-asc&g=e&upload=")
              url_base=str_c("https://hemerotecadigital.bne.es/hd/es/results?o=&w=&f=text&o=o&w=&f=text&o=n&w=&f=text&o=&w=&f=text&o=o&w=&f=text&o=n&w=&f=text&p=0~0&g=p&d=date&d=",y,"-",z,"-01&d=",y,"-",z,"-",end_of_month[j],"&l=10&t=date-asc&g=e&upload=")
            }
            if(resolution=="Année"){
              url=str_c("https://hemerotecadigital.bne.es/hd/es/results?o=&w=%22",mot1,"%22&f=text&o=o&w=&f=text&o=n&w=&f=text&o=&w=&f=text&o=o&w=&f=text&o=n&w=&f=text&p=0%7E0&g=p&d=date&d=",y,"-01-01&d=",y,"-12-31&l=10&t=date-asc&g=e&upload=")
              url_base=str_c("https://hemerotecadigital.bne.es/hd/es/results?o=&w=&f=text&o=o&w=&f=text&o=n&w=&f=text&o=&w=&f=text&o=o&w=&f=text&o=n&w=&f=text&p=0~0&g=p&d=date&d=",y,"-01-01&d=",y,"-12-31&l=10&t=date-asc&g=e&upload=")
            }
          }
          if(doc_type == 13){beginning<-str_replace_all(beginning,"/","-")
          end<-str_replace_all(end,"/","-")
          url<-str_c("https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=",mot1,"&none_q=&from_d=",beginning,"&to_d=",end,"&per_lang=fr&per=&lang=FR&per_type=1")
          url_base<-str_c("https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=&none_q=&from_d=",beginning,"&to_d=",end,"&per_lang=fr&per=&lang=FR&per_type=1")
          }
          if(doc_type == 14){beginning<-str_replace_all(beginning,"/","-")
          end<-str_replace_all(end,"/","-")
          url<-str_c("https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=",mot1,"&none_q=&from_d=",beginning,"&to_d=",end,"&per_lang=nl&per=&lang=FR&per_type=1")
          url_base<-str_c("https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=&none_q=&from_d=",beginning,"&to_d=",end,"&per_lang=nl&per=&lang=FR&per_type=1")
          }
          if(doc_type == 15){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=",z,"&dafyq=",y,"&datdq=",end_of_month[j],"&datmq=",z,"&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
              url_base<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=la+OR+le+OR+un+OR+et+OR+une+OR+des+OR+les+OR+ou+OR+donc&dafdq=01&dafmq=",z,"&dafyq=",y,"&datdq=",end_of_month[j],"&datmq=",z,"&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr")
            }
            if(resolution=="Année"){
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=01&dafyq=",y,"&datdq=31&datmq=12&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
              url_base<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=la+OR+le+OR+un+OR+et+OR+une+OR+des+OR+les+OR+ou+OR+donc&dafdq=01&dafmq=01&dafyq=",y,"&datdq=31&datmq=12&datyq=",y,"&laq=fr&puq=&txf=txIN&ssnip=&ccq=&l=fr")
            }
          }
          if(doc_type == 16){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=",z,"&dafyq=",y,"&datdq=",end_of_month[j],"&datmq=",z,"&datyq=",y,"&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
              url_base<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=der+OR+die+OR+das+OR+ein+OR+ich+OR+du+OR+er+OR+sie+OR+es&dafdq=01&dafmq=",z,"&dafyq=",y,"&datdq=",end_of_month[j],"&datmq=",z,"&datyq=",y,"&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr")
            }
            if(resolution=="Année"){
              url<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=%22",mot1,"%22",or,"&dafdq=01&dafmq=01&dafyq=",y,"&datdq=31&datmq=12&datyq=",y,"&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr&tyq=ARTICLE")
              url_base<-str_c("https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=der+OR+die+OR+das+OR+ein+OR+ich+OR+du+OR+er+OR+sie+OR+es&dafdq=01&dafmq=01&dafyq=",y,"&datdq=31&datmq=12&datyq=",y,"&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr")
            }
          }
          if(doc_type == 17){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.lectura.plus/Presse/search/?query=",mot1,"&fromDate=01%2F",z,"%2F",y,"&untilDate=",end_of_month[j],"%2F",z,"%2F",y)
              url_base<-str_c("https://www.lectura.plus/Presse/search/?query=&fromDate=01%2F",z,"%2F",y,"&untilDate=",end_of_month[j],"%2F",z,"%2F",y)
            }
            if(resolution=="Année"){
              url<-str_c("https://www.lectura.plus/Presse/search/?query=",mot1,"&fromDate=01%2F01%2F",y,"&untilDate=31%2F12%2F",y)
              url_base<-str_c("https://www.lectura.plus/Presse/search/?query=&fromDate=01%2F01%2F",y,"&untilDate=31%2F12%2F",y)
            }
          }
          if(doc_type == 18){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://kiosque.limedia.fr/recherche/?query=",mot1,"&search_type=exact&uniform_title=&date=&period_start=01/",z,"/",y,"&period_end=",end_of_month[j],"/",z,"/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")
              url_base<-str_c("https://kiosque.limedia.fr/recherche/?query=&search_type=or&uniform_title=&date=&period_start=01/",z,"/",y,"&period_end=",end_of_month[j],"/",z,"/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")
            }
            if(resolution=="Année"){
              url<-str_c("https://kiosque.limedia.fr/recherche/?query=",mot1,"&search_type=exact&uniform_title=&date=&period_start=01/01/",y,"&period_end=31/12/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")
              url_base<-str_c("https://kiosque.limedia.fr/recherche/?query=&search_type=or&uniform_title=&date=&period_start=01/01/",y,"&period_end=31/12/",y,"&filter_language=fre&sort_patrimonial=item_created_start_asc")
            }
          }
          if(doc_type == 19){
            url<-str_c("https://memonum-mediatheques.montpellier3m.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("https://memonum-mediatheques.montpellier3m.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 20){
            url<-str_c("https://www.communpatrimoine.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("https://www.communpatrimoine.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 21){
            url<-str_c("https://yroise.biblio.brest.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("https://yroise.biblio.brest.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 22){
            url<-str_c("https://www.pireneas.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("https://www.pireneas.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 23){
            url<-str_c("https://rosalis.bibliotheque.toulouse.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("https://rosalis.bibliotheque.toulouse.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 24){
            url<-str_c("https://bibliotheque-numerique.diplomatie.gouv.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("https://bibliotheque-numerique.diplomatie.gouv.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 25){
            url<-str_c("http://rfnum-bibliotheque.org/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("http://rfnum-bibliotheque.org/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 26){
            url<-str_c("https://www.numistral.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22",mot1,"%22%20",or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=",mot1,or_end)
            url_base<-str_c("https://www.numistral.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",beginning,"%22%20and%20gallicapublication_date%3C=%22",end,"%22)&suggest=10&keywords=")
          }
          if(doc_type == 27){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.bn-r.fr/presse_ancienne_resultats.php?type_rech=pr&q_fulltext=%22",mot1,"%22&pr_jour=&pr_mois=&pr_annee=&date_debut=01-",z,"-",y,"&date_fin=",end_of_month[j],"-",z,"-",y,"&sort=date_formated%20asc,tri_titre%20asc&from=presse#")
              url_base<-str_c("https://www.bn-r.fr/presse_ancienne_resultats.php?type_rech=pr&q_fulltext=&pr_jour=&pr_mois=&pr_annee=&date_debut=01-",z,"-",y,"&date_fin=",end_of_month[j],"-",z,"-",y,"&sort=date_formated%20asc,tri_titre%20asc&from=presse#")
            }
            if(resolution=="Année"){
              url<-str_c("https://www.bn-r.fr/presse_ancienne_resultats.php?type_rech=pr&q_fulltext=%22",mot1,"%22&pr_jour=&pr_mois=&pr_annee=&date_debut=01-01-",y,"&date_fin=31-12-",y,"&sort=date_formated%20asc,tri_titre%20asc&from=presse#")
              url_base<-str_c("https://www.bn-r.fr/presse_ancienne_resultats.php?type_rech=pr&q_fulltext=&pr_jour=&pr_mois=&pr_annee=&date_debut=01-01-",y,"&date_fin=31-12-",y,"&sort=date_formated%20asc,tri_titre%20asc&from=presse#")
            }
          }
          if(doc_type==28){
            url="https://numerique.banq.qc.ca/rechercheExterne/encoded/Sm9mZnJl/false/P/desc/W3sibm9tIjoiY29ycHVzIiwidmFsZXVyIjoiUGF0cmltb2luZSUyMHF1w6liw6ljb2lzIn0seyJub20iOiJ0eXBlX2RvY19mIiwidmFsZXVyIjoiUmV2dWVzJTIwZXQlMjBqb3VybmF1eCJ9LHsibm9tIjoibGFuZ3Vlc19jb250ZW51IiwidmFsZXVyIjoiZnJhbsOnYWlzIn0seyJub20iOiJhdmVjX3RleHRlX2ludGVncmFsIiwidmFsZXVyIjoib3VpIn0seyJub20iOiJnZW5yZV9mIiwidmFsZXVyIjoiSm91cm5hdXgifV0=/Liste%20de%20r%C3%A9sultats/true/false/eyJkZWJ1dCI6eyJhbm5lZSI6MTkxNCwibW9pcyI6MSwiam91ciI6MX0sImZpbiI6eyJhbm5lZSI6MTkxNCwibW9pcyI6MTIsImpvdXIiOjMxfX0="
            url_base="https://numerique.banq.qc.ca/rechercheExterne/encoded/Kg==/false/T/asc/W3sibm9tIjoiY29ycHVzIiwidmFsZXVyIjoiUGF0cmltb2luZSUyMHF1w6liw6ljb2lzIn0seyJub20iOiJ0eXBlX2RvY19mIiwidmFsZXVyIjoiUmV2dWVzJTIwZXQlMjBqb3VybmF1eCJ9LHsibm9tIjoibGFuZ3Vlc19jb250ZW51IiwidmFsZXVyIjoiZnJhbsOnYWlzIn0seyJub20iOiJhdmVjX3RleHRlX2ludGVncmFsIiwidmFsZXVyIjoib3VpIn0seyJub20iOiJnZW5yZV9mIiwidmFsZXVyIjoiSm91cm5hdXgifV0=/Liste%20de%20r%C3%A9sultats/true/false/eyJkZWJ1dCI6eyJhbm5lZSI6MTkzMywibW9pcyI6MSwiam91ciI6MX0sImZpbiI6eyJhbm5lZSI6MTkzMywibW9pcyI6MTIsImpvdXIiOjMxfX0="
          }
          if(doc_type == 29){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://anno.onb.ac.at/anno-suche#searchMode=complex&text=%22",mot1,"%22",or,"&language=ger&dateMode=date&dateFrom=01.",z,".",y,"&dateTo=",end_of_month[j],".",z,".",y,"&from=1")
              url_base<-str_c("https://anno.onb.ac.at/anno-suche#searchMode=complex&language=ger&dateMode=date&dateFrom=01.",z,".",y,"&dateTo=",end_of_month[j],".",z,".",y,"&from=1")
            }
            if (resolution=="Année"){
              url<-str_c("https://anno.onb.ac.at/anno-suche#searchMode=complex&text=%22",mot1,"%22",or,"&language=ger&dateMode=date&dateFrom=01.01.",y,"&dateTo=31.12.",y,"&from=1")
              url_base<-str_c("https://anno.onb.ac.at/anno-suche#searchMode=complex&language=ger&dateMode=date&dateFrom=01.01.",y,"&dateTo=31.12.",y,"&from=1")
            }
          }
          if(doc_type == 32){
            if (input$cairn==0){
              url<-str_c("https://www.cairn.info/resultats_recherche.php?src1=Tx&word1=",mot1,"&exact1=1&operator1=AND&src2=Year&from2=",y,"&to2=",y,"&operator2=AND&src3=TypePub&word3=1&searchTermAccess=all&nparams=3&submitAdvForm=Chercher")
              url_base<-str_c("https://www.cairn.info/resultats_recherche.php?src1=Tx&word1=le&exact1=1&operator1=AND&src2=Year&from2=",y,"&to2=",y,"&operator2=AND&src3=TypePub&word3=1&searchTermAccess=all&nparams=3&submitAdvForm=Chercher")
            }
            else{
              url<-str_c("https://www.cairn.info/resultats_recherche.php?src1=Tx&word1=",mot1,"&exact1=1&operator1=AND&src2=Year&from2=",y,"&to2=",y,"&operator2=AND&src3=TypePub&word3=1&operator3=AND&src4=Disc&word4=",input$cairn,"&operator4=&nparams=4&submitAdvForm=Chercher")
              url_base<-str_c("https://www.cairn.info/resultats_recherche.php?src1=Tx&word1=le&exact1=1&operator1=AND&src2=Year&from2=",y,"&to2=",y,"&operator2=AND&src3=TypePub&word3=1&operator3=AND&src4=Disc&word4=",input$cairn,"&operator4=&nparams=4&submitAdvForm=Chercher")
            }
          }
          if(doc_type == 33){
            th<-URLencode(input$theses)
            if (input$theses=="_"){
              url<-str_c("https://www.theses.fr/fr/?q=&zone1=textes&val1=",mot1,"&op1=AND&zone2=auteurs&val2=&op2=AND&zone3=etabSoutenances&val3=&op3=AND&zone4=dateSoutenance&val4a=01%2F01%2F",y,"&val4b=31%2F12%2F",y,"&access=accessible:oui")
              url_base<-str_c("https://www.theses.fr/fr/?q=&zone1=textes&val1=le&op1=AND&zone2=auteurs&val2=&op2=AND&zone3=etabSoutenances&val3=&op3=AND&zone4=dateSoutenance&val4a=01%2F01%2F",y,"&val4b=31%2F12%2F",y,"&access=accessible:oui")
            }
            else{
              url<-str_c("https://www.theses.fr/fr/?q=&checkedfacets=discipline=",th,";&zone1=textes&val1=",mot1,"&op1=AND&zone2=auteurs&val2=&op2=AND&zone3=etabSoutenances&val3=&op3=AND&zone4=dateSoutenance&val4a=01%2F01%2F",y,"&val4b=31%2F12%2F",y,"&access=accessible:oui")
              url_base<-str_c("https://www.theses.fr/fr/?q=&checkedfacets=discipline=",th,";&zone1=textes&val1=le&op1=AND&zone2=auteurs&val2=&op2=AND&zone3=etabSoutenances&val3=&op3=AND&zone4=dateSoutenance&val4a=01%2F01%2F",y,"&val4b=31%2F12%2F",y,"&access=accessible:oui")
            }
          }
          if(doc_type == 34){
            url<-str_c("https://www.persee.fr/search?l=fre&da=",y,"&q=%22",mot1,"%22")
            url_base<-str_c("https://www.persee.fr/search?l=fre&da=",y,"&q=%22les%22")
          }
          if(doc_type == 35){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://api.trove.nla.gov.au/v2/result?key=p1746d1fbge6qp59&zone=newspaper&q=%22",mot1,"%22%20date:[",y,"-",z,"-01T00:00:00Z%20TO%20",y,"-",z,"-",end_of_month[j],"T23:59:00Z]&n=1")
              url_base<-str_c("https://api.trove.nla.gov.au/v2/result?key=p1746d1fbge6qp59&zone=newspaper&q=%20date:[",y,"-",z,"-01T00:00:00Z%20TO%20",y,"-",z,"-",end_of_month[j],"T23:59:00Z]&n=1")
            }
            if (resolution=="Année"){
              url<-str_c("https://api.trove.nla.gov.au/v2/result?key=aiffua8e6gnjlpoi&zone=newspaper&q=%22",mot1,"%22%20date:[",y,"%20TO%20",y,"]&n=1")
              url_base<-str_c("https://api.trove.nla.gov.au/v2/result?key=aiffua8e6gnjlpoi&zone=newspaper&q=date:[",y,"%20TO%20",y,"]&n=1")
            }
          }
          if(doc_type == 36){
            if (input$isidore=="_"){
              url<-str_c("https://api.isidore.science/resource/search?q=%22",mot1,"%22&date=",y,"&replies=0&type=http%3A%2F%2Fisidore.science%2Fontology%23thesis&type=http%3A%2F%2Fisidore.science%2Fontology%23seminar&type=http%3A%2F%2Fisidore.science%2Fontology%23recension&type=http%3A%2F%2Fisidore.science%2Fontology%23memorandum&type=http%3A%2F%2Fisidore.science%2Fontology%23preprint&type=http%3A%2F%2Fisidore.science%2Fontology%23periodical&type=http%3A%2F%2Fisidore.science%2Fontology%23course&type=http%3A%2F%2Fisidore.science%2Fontology%23conference&type=http%3A%2F%2Fisidore.science%2Fontology%23book&type=http%3A%2F%2Fisidore.science%2Fontology%23blogPost&type=http%3A%2F%2Fisidore.science%2Fontology%23article")
              url_base<-str_c("https://api.isidore.science/resource/search?date=",y,"&replies=0&type=http%3A%2F%2Fisidore.science%2Fontology%23thesis&type=http%3A%2F%2Fisidore.science%2Fontology%23seminar&type=http%3A%2F%2Fisidore.science%2Fontology%23recension&type=http%3A%2F%2Fisidore.science%2Fontology%23memorandum&type=http%3A%2F%2Fisidore.science%2Fontology%23preprint&type=http%3A%2F%2Fisidore.science%2Fontology%23periodical&type=http%3A%2F%2Fisidore.science%2Fontology%23course&type=http%3A%2F%2Fisidore.science%2Fontology%23conference&type=http%3A%2F%2Fisidore.science%2Fontology%23book&type=http%3A%2F%2Fisidore.science%2Fontology%23blogPost&type=http%3A%2F%2Fisidore.science%2Fontology%23article")
            }
            else{
              url<-str_c("https://api.isidore.science/resource/search?q=%22",mot1,"%22&date=",y,"&replies=0&type=http%3A%2F%2Fisidore.science%2Fontology%23thesis&type=http%3A%2F%2Fisidore.science%2Fontology%23seminar&type=http%3A%2F%2Fisidore.science%2Fontology%23recension&type=http%3A%2F%2Fisidore.science%2Fontology%23memorandum&type=http%3A%2F%2Fisidore.science%2Fontology%23preprint&type=http%3A%2F%2Fisidore.science%2Fontology%23periodical&type=http%3A%2F%2Fisidore.science%2Fontology%23course&type=http%3A%2F%2Fisidore.science%2Fontology%23conference&type=http%3A%2F%2Fisidore.science%2Fontology%23book&type=http%3A%2F%2Fisidore.science%2Fontology%23blogPost&type=http%3A%2F%2Fisidore.science%2Fontology%23article&discipline=http%3A%2F%2Faurehal.archives-ouvertes.fr%2Fsubject%2Fshs.",input$isidore)
              url_base<-str_c("https://api.isidore.science/resource/search?date=",y,"&replies=0&type=http%3A%2F%2Fisidore.science%2Fontology%23thesis&type=http%3A%2F%2Fisidore.science%2Fontology%23seminar&type=http%3A%2F%2Fisidore.science%2Fontology%23recension&type=http%3A%2F%2Fisidore.science%2Fontology%23memorandum&type=http%3A%2F%2Fisidore.science%2Fontology%23preprint&type=http%3A%2F%2Fisidore.science%2Fontology%23periodical&type=http%3A%2F%2Fisidore.science%2Fontology%23course&type=http%3A%2F%2Fisidore.science%2Fontology%23conference&type=http%3A%2F%2Fisidore.science%2Fontology%23book&type=http%3A%2F%2Fisidore.science%2Fontology%23blogPost&type=http%3A%2F%2Fisidore.science%2Fontology%23article&discipline=http%3A%2F%2Faurehal.archives-ouvertes.fr%2Fsubject%2Fshs.",input$isidore)
            }
          }
          if(doc_type == 30){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",mot1,"%22&start_at=01%2F",z,"%2F",y,"&end_at=",end_of_month[j],"%2F",z,"%2F",y,"&search_sort=relevance_desc")
              url_base<-str_c("https://www.lemonde.fr/recherche/?search_keywords=le&start_at=01%2F",z,"%2F",y,"&end_at=",end_of_month[j],"%2F",z,"%2F",y,"&search_sort=relevance_desc")
            }
            if (resolution=="Année"){
              url<-str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",mot1,"%22&start_at=01%2F01%2F",y,"&end_at=31%2F12%2F",y,"&search_sort=relevance_desc")
              url_base<-str_c("https://www.lemonde.fr/recherche/?search_keywords=le&start_at=01%2F01%2F",y,"&end_at=31%2F12%2F",y,"&search_sort=relevance_desc")
            }
          }
          if(doc_type == 31){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://recherche.lefigaro.fr/recherche/",mot1,"/?publication=lefigaro.fr&datemin=01-",z,"-",y,"&datemax=",end_of_month[j],"-",z,"-",y)
              url_base<-str_c("https://recherche.lefigaro.fr/recherche/_/?publication=lefigaro.fr&datemin=01-",z,"-",y,"&datemax=",end_of_month[j],"-",z,"-",y)
            }
            if (resolution=="Année"){
              url<-str_c("https://recherche.lefigaro.fr/recherche/",mot1,"/?publication=lefigaro.fr&datemin=01-01-",y,"&datemax=31-12-",y)
              url_base<-str_c("https://recherche.lefigaro.fr/recherche/_/?publication=lefigaro.fr&datemin=01-01-",y,"&datemax=31-12-",y)
            }
          }
          if(doc_type==37|doc_type==38|doc_type==39|doc_type==40){
            if(doc_type==37){country="us"}
            if(doc_type==38){country="ca"}
            if(doc_type==39){country="gb"}
            if(doc_type==40){country="au"}
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url=str_c("https://www.newspapers.com/search/#query=%22",mot1,"%22&p_country=",country,"&ymd-start=",beginning,"&ymd-end=",end)
              url_base=str_c("https://www.newspapers.com/search/#p_country=",country,"&dr_year=",beginning,"&ymd-end=",end)
            }
            if (resolution=="Année"){
              url=str_c("https://www.newspapers.com/search/#query=%22",mot1,"%22&p_country=",country,"&dr_year=",y,"-",y)
              url_base=str_c("https://www.newspapers.com/search/#p_country=",country,"&dr_year=",y,"-",y)
            }
          }
          if(doc_type == 42){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=",z,"%2F01%2F",y,"&date2=",z,"%2F",end_of_month[j],"%2F",y,"&language=eng&ortext=&andtext=&phrasetext=",mot1,"&proxtext=&proxdistance=5&rows=1&searchType=advanced&sort=date&format=json")
              url_base<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=",z,"%2F01%2F",y,"&date2=",z,"%2F",end_of_month[j],"%2F",y,"&language=eng&ortext=&andtext=&proxtext=&proxdistance=5&rows=1&searchType=advanced&sort=date&format=json")
            }
            if (resolution=="Année"){
              url<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=01%2F01%2F",y,"&date2=12%2F31%2F",y,"&language=eng&ortext=&andtext=&phrasetext=",mot1,"&proxtext=&proxdistance=5&rows=1&searchType=advanced&sort=date&format=json")
              url_base<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=01%2F01%2F",y,"&date2=12%2F31%2F",y,"&language=eng&ortext=&andtext=&proxtext=&proxdistance=5&rows=1&searchType=advanced&sort=date&format=json")
            }
          }
          if(doc_type == 43 & input$search_mode == 1){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://www.deutsche-digitale-bibliothek.de/search/newspaper?query=%22",mot1,"%22&language=ger&fromDay=1&fromMonth=",z,"&fromYear=",y,"&toDay=",end_of_month[j],"&toMonth=",z,"&toYear=",y)
              url_base<-str_c("https://www.deutsche-digitale-bibliothek.de/search/newspaper?query=&language=ger&fromDay=1&fromMonth=",z,"&fromYear=",y,"&toDay=",end_of_month[j],"&toMonth=",z,"&toYear=",y)
            }
            if (resolution=="Année"){
              url<-str_c("https://www.deutsche-digitale-bibliothek.de/search/newspaper?query=%22",mot1,"%22&language=ger&fromDay=1&fromMonth=1&fromYear=",y,"&toDay=31&toMonth=12&toYear=",y)
              url_base<-str_c("https://www.deutsche-digitale-bibliothek.de/search/newspaper?query=&language=ger&fromDay=1&fromMonth=1&fromYear=",y,"&toDay=31&toMonth=12&toYear=",y)
            }
          }
          if(doc_type == 45 | doc_type == 46 | doc_type == 47 | doc_type == 48 | doc_type == 49){
            if(doc_type==45){lang="fr"}
            if(doc_type==46){lang="en"}
            if(doc_type==47){lang="de"}
            if(doc_type==48){lang="nl"}
            if(doc_type==49){lang="es"}
            if(input$musixmatch_genre==0){genre=""}
            else{genre=as.character(input$musixmatch_genre)}
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,z,"01")
              end = str_c(y,z,end_of_month[j])
              url<-str_c("https://api.musixmatch.com/ws/1.1/track.search?apikey=f126b582c17d10329daf15284bc0543f&format=json&callback=jsonp&q_lyrics=",mot1,"&s_track_release_date=asc&f_lyrics_language=",lang,"&page=1&page_size=1&f_track_release_group_first_release_date_min=",beginning,"&f_track_release_group_first_release_date_max=",end,"&f_music_genre_id=",genre)
              url_base<-str_c("https://api.musixmatch.com/ws/1.1/track.search?apikey=f126b582c17d10329daf15284bc0543f&format=json&callback=jsonp&q_lyrics=&s_track_release_date=asc&f_lyrics_language=",lang,"&page=1&page_size=1&f_track_release_group_first_release_date_min=",beginning,"&f_track_release_group_first_release_date_max=",end,"&f_music_genre_id=",genre)
            }
            if(resolution=="Année"){url<-str_c("https://api.musixmatch.com/ws/1.1/track.search?apikey=f126b582c17d10329daf15284bc0543f&format=json&callback=jsonp&q_lyrics=",mot1,"&s_track_release_date=asc&f_lyrics_language=",lang,"&page=1&page_size=1&f_track_release_group_first_release_date_min=",y,"0101&f_track_release_group_first_release_date_max=",y,"1231","&f_music_genre_id=",genre)
            url_base<-str_c("https://api.musixmatch.com/ws/1.1/track.search?apikey=f126b582c17d10329daf15284bc0543f&format=json&callback=jsonp&q_lyrics=&s_track_release_date=asc&f_lyrics_language=",lang,"&page=1&page_size=1&f_track_release_group_first_release_date_min=",y,"0101&f_track_release_group_first_release_date_max=",y,"1231","&f_music_genre_id=",genre)
            }
          }
          if(doc_type == 55){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://lemarin.ouest-france.fr/archives/search/%22",mot1,"%22/92c8b815583ded5ee58155129533fdfe/",beginning,"/",end,"/page")
              url_base<-str_c("https://lemarin.ouest-france.fr/archives/search/%20/92c8b815583ded5ee58155129533fdfe/",beginning,"/",end,"/page")
            }
            if (resolution=="Année"){
              url<-str_c("https://lemarin.ouest-france.fr/archives/search/%22",mot1,"%22/92c8b815583ded5ee58155129533fdfe/",y,"-01-01/",y,"-12-31/page")
              url_base<-str_c("https://lemarin.ouest-france.fr/archives/search/%20/92c8b815583ded5ee58155129533fdfe/",y,"-01-01/",y,"-12-31/page")
            }
          }
          if(doc_type == 57){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://eluxemburgensia.lu/search?query=",mot1,"&documentType=newspaper,serial&type=CORRECTIONS,SUPPLEMENT,MAP_GROUP,Monograph,TITLE_SECTION,SHIPPING,ADVERTISEMENT_SECTION,ISSUE,APPENDIX,INDEX,Manuscript,WEATHER,VOLUME,PREFACE,COVER_SECTION,ARTICLE,BIBLIOGRAPHY,CONTRIBUTION,CHAPTER,Poster,REVIEW,SECTION,TABLE_OF_CONTENTS&startDate=",beginning,"&endDate=",end,"&rows=20&start=0&contains=true&exact=true")
              url_base<-str_c("https://eluxemburgensia.lu/search?query=la&documentType=newspaper,serial&type=CORRECTIONS,SUPPLEMENT,MAP_GROUP,Monograph,TITLE_SECTION,SHIPPING,ADVERTISEMENT_SECTION,ISSUE,APPENDIX,INDEX,Manuscript,WEATHER,VOLUME,PREFACE,COVER_SECTION,ARTICLE,BIBLIOGRAPHY,CONTRIBUTION,CHAPTER,Poster,REVIEW,SECTION,TABLE_OF_CONTENTS&startDate=",beginning,"&endDate=",end,"&rows=20&start=0&contains=true&exact=true")
            }
            if (resolution=="Année"){
              url<-str_c("https://eluxemburgensia.lu/search?query=",mot1,"&documentType=newspaper,serial&type=CORRECTIONS,SUPPLEMENT,MAP_GROUP,Monograph,TITLE_SECTION,SHIPPING,ADVERTISEMENT_SECTION,ISSUE,APPENDIX,INDEX,Manuscript,WEATHER,VOLUME,PREFACE,COVER_SECTION,ARTICLE,BIBLIOGRAPHY,CONTRIBUTION,CHAPTER,Poster,REVIEW,SECTION,TABLE_OF_CONTENTS&startDate=",y,"-01-01&endDate=",y,"-12-31&rows=20&start=0&contains=true&exact=true")
              url_base<-str_c("https://eluxemburgensia.lu/search?query=la&documentType=newspaper,serial&type=CORRECTIONS,SUPPLEMENT,MAP_GROUP,Monograph,TITLE_SECTION,SHIPPING,ADVERTISEMENT_SECTION,ISSUE,APPENDIX,INDEX,Manuscript,WEATHER,VOLUME,PREFACE,COVER_SECTION,ARTICLE,BIBLIOGRAPHY,CONTRIBUTION,CHAPTER,Poster,REVIEW,SECTION,TABLE_OF_CONTENTS&startDate=",y,"-01-01&endDate=",y,"-12-31&rows=20&start=0&contains=true&exact=true")
            }
          }
          
          # if(doc_type == 65){
          #   if(resolution=="Mois"){
          #     z = as.character(j)
          #     if(nchar(z)<2){z<-str_c("0",z)}
          #     beginning = str_c(y,z,"01")
          #     end = str_c(y,z,end_of_month[j])
          #     url<-str_c("https://www.nytimes.com/search?dropmab=false&endDate=",end,"&query=",mot1,"&sort=best&startDate=",beginning,"&types=article")
          #     url_base<-str_c("https://www.nytimes.com/search?dropmab=false&endDate=",end,"&query=have&sort=best&startDate=",beginning,"&types=article")
          #     }
          #   if (resolution=="Année"){
          #     url<-str_c("https://www.nytimes.com/search?dropmab=false&endDate=",y,"1231&query=",mot1,"&sort=best&startDate=",y,"0101&types=article")
          #     url_base<-str_c("https://www.nytimes.com/search?dropmab=false&endDate=",y,"1231&query=have&sort=best&startDate=",y,"0101&types=article")
          #   }
          # }
          
          
          if(doc_type == 1 | doc_type==56 | doc_type==19 | doc_type==20 | doc_type==21 | doc_type==22 | doc_type==23 | doc_type==24 | doc_type==25){
            ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
            a<-str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
              b<-str_extract(str_extract(ngram_base,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
            }
          }
          if(doc_type == 6 | doc_type == 7){
            ngram<-read_html(RETRY("GET",url,times = 6))
            ngram<-html_text(html_node(ngram,"#search-interface > div > div.col-results.col > div > section > div.mb-3.d-flex.align-items-start.justify-content-between > div.overflow-hidden > h1"))
            ngram<-str_remove_all(ngram,"[:punct:]")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram<-read_html(RETRY("GET",url_base,times = 6))
              ngram<-html_text(html_node(ngram,"#search-interface > div > div.col-results.col > div > section > div.mb-3.d-flex.align-items-start.justify-content-between > div.overflow-hidden > h1"))
              ngram<-str_remove_all(ngram,"[:punct:]")
              b<-str_extract(ngram,"[:digit:]+")
            }
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
            if(incr_mot==1){
              ngram_base<-as.character(read_html(RETRY("GET",url_base,times = 6)))
              ngram_base<-str_remove_all(ngram_base,"[:space:]")
              ngram_base<-str_extract(ngram_base,"Date--.+Newspapers--")
              ngram_base<-str_extract(ngram_base,'list-group-item"title.+')
              ngram_base<-str_remove_all(ngram_base,",")
              ngram_base<-str_c(unlist(str_extract_all(ngram_base,">[:digit:]+<")))
              ngram_base<-str_remove_all(ngram_base,"<")
              ngram_base<-str_remove_all(ngram_base,">")
              b<-sum(as.integer(ngram_base))
            }
          }
          if(doc_type == 11){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_remove_all(ngram,"[:punct:]")
            ngram<-str_remove_all(ngram,"[:space:]")
            ngram<-str_remove_all(ngram,"<strong>")
            a<-str_extract(str_extract(ngram,"Resultados[:digit:]+"),"[:digit:]+")
            if(incr_mot==1){
              ngram<-as.character(read_html(RETRY("GET",url_base,times = 6)))
              ngram<-str_remove_all(ngram,"[:punct:]")
              ngram<-str_remove_all(ngram,"[:space:]")
              ngram<-str_remove_all(ngram,"<strong>")
              b<-str_extract(str_extract(ngram,"Resultados[:digit:]+"),"[:digit:]+")
            }
          }
          if(doc_type == 13 | doc_type == 14){
            remDr$navigate(url)
            Sys.sleep(2) # give the page time to fully load
            ngram <- remDr$getPageSource()[[1]]
            ngram<-str_extract(ngram,"foundnumber.+")
            ngram<-str_remove_all(ngram,"[:punct:]")
            a<-as.integer(str_extract(ngram,"[:digit:]+"))
            if(incr_mot==1){
              remDr$navigate(url_base)
              Sys.sleep(2) # give the page time to fully load
              ngram_base <- remDr$getPageSource()[[1]]
              ngram_base<-str_extract(ngram_base,"foundnumber.+")
              ngram_base<-str_remove_all(ngram_base,"[:punct:]")
              b<-as.integer(str_extract(ngram_base,"[:digit:]+"))
            }
          }
          if(doc_type == 15 | doc_type == 16){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_remove_all(ngram,",")
            ngram<-str_extract(ngram,"Résultats 1 - 20 de  .+")
            ngram<-str_remove(ngram,"Résultats 1 - 20 de  ")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-as.character(read_html(RETRY("GET",url_base,times = 6)))
              ngram_base<-str_remove_all(ngram_base,",")
              ngram_base<-str_extract(ngram_base,"Résultats 1 - 20 de  .+")
              ngram_base<-str_remove(ngram_base,"Résultats 1 - 20 de  ")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
          }
          if(doc_type == 17){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            ngram<-str_extract(ngram,"width:100px.+")
            ngram<-str_remove(ngram,"width:100px")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-as.character(read_html(RETRY("GET",url_base,times = 6)))
              ngram_base<-str_extract(ngram_base,"width:100px.+")
              ngram_base<-str_remove(ngram_base,"width:100px")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
          }
          if(doc_type == 18){
            ngram<-read_html(RETRY("GET",url,times = 6))
            a<-str_extract(html_text(html_node(ngram,".col-milieu")),"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 6))
              b<-str_extract(html_text(html_node(ngram_base,".col-milieu")),"[:digit:]+")
            }
          }
          if(doc_type ==26){
            ngram<-read_html(url)
            ngram<-html_text(html_node(ngram,"head > title:nth-child(3)"))
            ngram<- str_remove_all(ngram,"[:space:]")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(url_base)
              ngram_base<-html_text(html_node(ngram_base,"head > title:nth-child(3)"))
              ngram_base<- str_remove_all(ngram_base,"[:space:]")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
          }
          if(doc_type ==27){
            ngram<-as.character(read_html(url))
            ngram<-str_extract(ngram,"Résultats de la recherche.+")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-as.character(read_html(url_base))
              ngram_base<-str_extract(ngram_base,"Résultats de la recherche.+")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
          }
          if(doc_type ==28){
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              
              remDr$navigate(url)
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
              webElem$clickElement()
              Sys.sleep(1)
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
              if(str_detect(html_text(html_node(page,"h1.ng-binding")),"Aucun résultat")){a=0}
              else{
              url<-html_attr(html_node(page,"#permalien"),"value")
              a<-html_text(html_node(page,".chapoNbResultat"))
              a<-str_extract(a,"[:digit:]+")
              webElem <- remDr$findElement(using = 'css selector',"#dateInterval")
              webElem$clickElement()
              Sys.sleep(1)
              webElem <- remDr$findElement(using = 'css selector',"facet-filter.ng-isolate-scope > div:nth-child(10) > ul:nth-child(1) > li:nth-child(1) > span:nth-child(1) > i:nth-child(1)")
              webElem$clickElement()
              }
              
              if(incr_mot==1){
                remDr$navigate(url_base)
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
                if(str_detect(html_text(html_node(page,"h1.ng-binding")),"Aucun résultat")){b=0}
                else{
                b<-html_text(html_node(page,".chapoNbResultat"))
                b<-str_extract(b,"[:digit:]+")
                webElem <- remDr$findElement(using = 'css selector',"#dateInterval")
                webElem$clickElement()
                Sys.sleep(1)
                webElem <- remDr$findElement(using = 'css selector',"facet-filter.ng-isolate-scope > div:nth-child(10) > ul:nth-child(1) > li:nth-child(1) > span:nth-child(1) > i:nth-child(1)")
                webElem$clickElement()
                }
              }
            }
            
            
            if(resolution=="Année"){
              remDr$navigate(url)
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
              webElem$clickElement()
              Sys.sleep(1)
              webElem <- remDr$findElement(using = 'css selector',"#labelMotCle")
              webElem$clickElement()
              webElem <- remDr$findElement(using = 'css selector',"#recherche")
              webElem$clickElement()
              webElem$sendKeysToElement(list(mot1))
              webElem <- remDr$findElement(using = 'css selector',"i.hidden-xs") 
              webElem$clickElement()
              Sys.sleep(2)
              page <- read_html(remDr$getPageSource()[[1]])
              if(str_detect(html_text(html_node(page,"h1.ng-binding")),"Aucun résultat")){a=0}
              else{
              url<-html_attr(html_node(page,"#permalien"),"value")
              a<-html_text(html_node(page,".chapoNbResultat"))
              a<-str_extract(a,"[:digit:]+")
              webElem <- remDr$findElement(using = 'css selector',"#dateInterval")
              webElem$clickElement()
              Sys.sleep(1)
              webElem <- remDr$findElement(using = 'css selector',"facet-filter.ng-isolate-scope > div:nth-child(10) > ul:nth-child(1) > li:nth-child(1) > span:nth-child(1) > i:nth-child(1)")
              webElem$clickElement()}
              
              if(incr_mot==1){
                remDr$navigate(url_base)
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
                if(str_detect(html_text(html_node(page,"h1.ng-binding")),"Aucun résultat")){b=0}
                else{
                b<-html_text(html_node(page,".chapoNbResultat"))
                b<-str_extract(b,"[:digit:]+")
                webElem <- remDr$findElement(using = 'css selector',"#dateInterval")
                webElem$clickElement()
                Sys.sleep(1)
                webElem <- remDr$findElement(using = 'css selector',"facet-filter.ng-isolate-scope > div:nth-child(10) > ul:nth-child(1) > li:nth-child(1) > span:nth-child(1) > i:nth-child(1)")
                webElem$clickElement()
                }
              }
            }
          }
          if(doc_type==29){
            remDr$navigate(url)
            Sys.sleep(2) # give the page time to fully load
            ngram <- remDr$getPageSource()[[1]]
            ngram <- str_extract(ngram,".+Ergebnisse")
            ngram<-str_remove_all(ngram,"[:punct:]")
            ngram<-str_extract(ngram,"[:digit:]+ Ergebnisse")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              remDr$navigate(url_base)
              Sys.sleep(2) # give the page time to fully load
              ngram_base <- remDr$getPageSource()[[1]]
              ngram_base <- str_extract(ngram_base,".+Ergebnisse")
              ngram_base<-str_remove_all(ngram_base,"[:punct:]")
              ngram_base<-str_extract(ngram_base,"[:digit:]+ Ergebnisse")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
          }
          if(doc_type ==32){
            ngram<-read_html(RETRY("GET",url,times = 3, add_headers(.headers = c("User-Agent"="PARIS-SACLAY-Benjamin-Gallicanet"))))
            a<-html_text(html_node(ngram,".filter-result-list > li:nth-child(1) > b:nth-child(1)"))
            a<-str_remove_all(a,"[:space:]")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 3, add_headers(.headers = c("User-Agent"="PARIS-SACLAY-Benjamin-Gallicanet"))))
              b<-html_text(html_node(ngram_base,".filter-result-list > li:nth-child(1) > b:nth-child(1)"))
              b<-str_remove_all(b,"[:space:]")
            }
          }
          if(doc_type ==33){
            ngram<-read_html(RETRY("GET",url,times = 3))
            ngram<-str_remove_all(as.character(ngram),"[:space:]")
            ngram<-str_remove(ngram,".+numFound")
            ngram<-str_remove(ngram,",.+")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 3))
              ngram_base<-str_remove_all(as.character(ngram_base),"[:space:]")
              ngram_base<-str_remove(ngram_base,".+numFound")
              ngram_base<-str_remove(ngram_base,",.+")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
          }
          if(doc_type ==35){
            ngram<-read_html(RETRY("GET",url,times = 3))
            ngram<-as.character(ngram)
            ngram<-str_extract(ngram,"total.+")
            ngram<-str_remove_all(ngram,"[:space:]")
            ngram<-str_remove_all(ngram,"[:punct:]")
            ngram<-str_remove_all(ngram,"total")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 3))
              ngram_base<-as.character(ngram_base)
              ngram_base<-str_extract(ngram_base,"total.+")
              ngram_base<-str_remove_all(ngram_base,"[:space:]")
              ngram_base<-str_remove_all(ngram_base,"[:punct:]")
              ngram_base<-str_remove_all(ngram_base,"total")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
            if(input$resolution=="Année"){url=str_c("https://trove.nla.gov.au/search/advanced/category/newspapers?keyword=",mot1,"&l-advArtType=newspapers&date.from=",y,"-01-01&date.to=",y,"-12-31")}
            if(input$resolution=="Mois"){url=str_c("https://trove.nla.gov.au/search/advanced/category/newspapers?keyword=",mot1,"&l-advArtType=newspapers&date.from=",y,"-",z,"-01&date.to=",y,"-",z,"-",end_of_month[j])}
          }
          if(doc_type ==34){
            ngram<-read_html(RETRY("GET",url,times = 3))
            a<-html_text(ngram)
            a<-str_extract(a,".+Résultats")
            a=str_sub(a,-20)
            a<-str_extract(a,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 3))
              b<-html_text(ngram_base)
              b<-str_extract(b,".+Résultats")
              b=str_sub(b,-20)
              b<-str_extract(b,"[:digit:]+")
            }
          }
          if(doc_type ==36){
            ngram<-tryCatch(
              {ngram<-read_html(url)},error=function(cond){
                ngram<-tryCatch(
                  {ngram<-read_html(url)},error=function(cond){
                    ngram<-tryCatch(
                      {ngram<-read_html(url)},error=function(cond){
                        ngram<-tryCatch(
                          {ngram<-read_html(url)},error=function(cond){
                            ngram<-tryCatch(
                              {ngram<-read_html(url)},error=function(cond){
                                ngram<-tryCatch(
                                  {ngram<-read_html(url)},error=function(cond){
                                    ngram<-tryCatch(
                                      {ngram<-read_html(url)},error=function(cond){
                                        print("error")
                                        return(0)
                                      }
                                    )
                                  }
                                )
                              }
                            )
                          }
                        )
                      }
                    )
                  }
                )
              }
            )
            if(class(ngram[1])=="numeric"){a=0}
            else{
              ngram<-as.character(ngram)
              ngram<-str_extract(ngram,"items.+")
              ngram<-str_remove_all(ngram,"[:space:]")
              ngram<-str_remove_all(ngram,"[:punct:]")
              ngram<-str_remove_all(ngram,"items=")
              a<-str_extract(ngram,"[:digit:]+")}
            if(incr_mot==1){
              ngram_base<-tryCatch(
                {ngram_base<-read_html(url_base)},error=function(cond){
                  ngram_base<-tryCatch(
                    {ngram_base<-read_html(url_base)},error=function(cond){
                      ngram_base<-tryCatch(
                        {ngram_base<-read_html(url_base)},error=function(cond){
                          ngram_base<-tryCatch(
                            {ngram_base<-read_html(url_base)},error=function(cond){
                              ngram_base<-tryCatch(
                                {ngram_base<-read_html(url_base)},error=function(cond){
                                  ngram_base<-tryCatch(
                                    {ngram_base<-read_html(url_base)},error=function(cond){
                                      ngram_base<-tryCatch(
                                        {ngram_base<-read_html(url_base)},error=function(cond){
                                          print("error")
                                          return(0)
                                        }
                                      )
                                    }
                                  )
                                }
                              )
                            }
                          )
                        }
                      )
                    }
                  )
                }
              )
              if(class(ngram_base[1])=="numeric"){b=0}
              else{
                ngram_base<-as.character(ngram_base)
                ngram_base<-str_extract(ngram_base,"items.+")
                ngram_base<-str_remove_all(ngram_base,"[:space:]")
                ngram_base<-str_remove_all(ngram_base,"[:punct:]")
                ngram_base<-str_remove_all(ngram_base,"items=")
                b<-str_extract(ngram_base,"[:digit:]+")}
            }
            if (input$isidore=="_"){
              url<-str_c("https://isidore.science/s?q=%22",mot1,"%22&date=",y,"&type=http%3A%2F%2Fisidore.science%2Fontology%23thesis&type=http%3A%2F%2Fisidore.science%2Fontology%23seminar&type=http%3A%2F%2Fisidore.science%2Fontology%23recension&type=http%3A%2F%2Fisidore.science%2Fontology%23memorandum&type=http%3A%2F%2Fisidore.science%2Fontology%23preprint&type=http%3A%2F%2Fisidore.science%2Fontology%23periodical&type=http%3A%2F%2Fisidore.science%2Fontology%23course&type=http%3A%2F%2Fisidore.science%2Fontology%23conference&type=http%3A%2F%2Fisidore.science%2Fontology%23book&type=http%3A%2F%2Fisidore.science%2Fontology%23blogPost&type=http%3A%2F%2Fisidore.science%2Fontology%23article")}
            else{url<-str_c("https://isidore.science/s?q=%22",mot1,"%22&date=",y,"&type=http%3A%2F%2Fisidore.science%2Fontology%23thesis&type=http%3A%2F%2Fisidore.science%2Fontology%23seminar&type=http%3A%2F%2Fisidore.science%2Fontology%23recension&type=http%3A%2F%2Fisidore.science%2Fontology%23memorandum&type=http%3A%2F%2Fisidore.science%2Fontology%23preprint&type=http%3A%2F%2Fisidore.science%2Fontology%23periodical&type=http%3A%2F%2Fisidore.science%2Fontology%23course&type=http%3A%2F%2Fisidore.science%2Fontology%23conference&type=http%3A%2F%2Fisidore.science%2Fontology%23book&type=http%3A%2F%2Fisidore.science%2Fontology%23blogPost&type=http%3A%2F%2Fisidore.science%2Fontology%23article&discipline=http%3A%2F%2Faurehal.archives-ouvertes.fr%2Fsubject%2Fshs.",input$isidore)}
          }

          if(input$doc_type == 30){
            
            page<-read_html(RETRY("GET",url,times = 6))
            ngram<-page
            ngram<-html_text(html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.river__pagination"))
            ngram<-str_squish(ngram)
            a<-str_extract(ngram,"([:digit:]+)$")
            
            if(is.na(a)){
              ngram<-page
              ngram<-html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.js-river-search")
              ngram<-as.character(ngram)
              a<-str_count(ngram,"teaser teaser--inline-picture")
            }
            else{
              url<-str_c(url,"&page=",a)
              page<-read_html(RETRY("GET",url,times = 6))
              ngram<-page
              ngram<-html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.js-river-search")
              ngram<-as.character(ngram)
              c<-str_count(ngram,"teaser teaser--inline-picture")
              a<-as.integer(a)
              c<-as.integer(c)
              a<-(a-1)*40+c
            }
            if(incr_mot==1){
              ngram<-read_html(RETRY("GET",url_base,times = 6))
              ngram<-html_text(html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.river__pagination"))
              ngram<-str_squish(ngram)
              b<-str_extract(ngram,"([:digit:]+)$")
              b<-as.integer(b)
              b=40*b
            }
          }
          if(input$doc_type == 31){
            ngram<-read_html(RETRY("GET",url,times = 6))
            ngram<-as.character(html_node(ngram,".facettes__nombre"))
            ngram<-str_replace_all(ngram,"[:space:]","")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 6))
              ngram_base<-as.character(html_node(ngram_base,".facettes__nombre"))
              ngram_base<-str_replace_all(ngram_base,"[:space:]","")
              b<-str_extract(ngram_base,"[:digit:]+")
            }        
          }
          if(doc_type==2){
            url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(",mot1,or,")%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)")
            url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords=")
            if(input$dewey!="999"){
              if(str_length(input$dewey)==1){url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(",mot1,or,")%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)%20and%20(dewey%20all%20%22",input$dewey,"%22)")}
              if(str_length(input$dewey)>1){url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(",mot1,or,")%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)%20and%20(sdewey%20all%20%22",input$dewey,"%22)")}
            }
            ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
            a<-str_extract(str_extract(ngram,"numberOfRecords>[:digit:]+"),"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
              b<-str_extract(str_extract(ngram_base,"numberOfRecords>[:digit:]+"),"[:digit:]+")
            }
            if(input$dewey!="999"){
              if(str_length(input$dewey)==1){url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)%20and%20(dewey%20all%20%22",input$dewey,"%22)")}
              if(str_length(input$dewey)>1){url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=1&page=1&collapsing=false&exactSearch=true&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",y,"%22%20and%20gallicapublication_date%3C=%22",y,"%22)%20and%20(sdewey%20all%20%22",input$dewey,"%22)")}
              if(incr_mot==1){
                ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
                b<-str_extract(str_extract(ngram_base,"numberOfRecords>[:digit:]+"),"[:digit:]+")
              }
            }
            
          }
          if(doc_type==37 | doc_type==38 | doc_type==39 | doc_type==40){
            remDr$navigate("https://www.google.fr/")
            remDr$navigate(url)
            Sys.sleep(2) # give the page time to fully load
            ngram <- remDr$getPageSource()[[1]]
            ngram=read_html(ngram)
            ngram <- html_node(ngram,"#root > div > div > div > div.order-1.order-md-2.col-12.col-lg-8 > h1 > span.text-secondary2")
            ngram<-html_text(ngram)
            a<-str_remove_all(ngram,"[:punct:]")
            if(incr_mot==1){
              remDr$navigate("https://www.google.fr/")
              remDr$navigate(url_base)
              Sys.sleep(2.5) # give the page time to fully load
              ngram <- remDr$getPageSource()[[1]]
              ngram=read_html(ngram)
              ngram <- html_node(ngram,"#root > div > div > div > div.order-1.order-md-2.col-12.col-lg-8 > h1 > span.text-secondary2")
              ngram<-html_text(ngram)
              b<-str_remove_all(ngram,"[:punct:]")
            }
          }
          
          if(input$doc_type == 42){
            ngram<-read_html(RETRY("GET",url,times = 6))
            ngram<-as.character(html_text(ngram))
            ngram<-str_replace_all(ngram,"[:punct:]","")
            ngram<-str_replace_all(ngram,"[:space:]","")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 6))
              ngram_base<-as.character(html_text(ngram_base))
              ngram_base<-str_replace_all(ngram_base,"[:punct:]","")
              ngram_base<-str_replace_all(ngram_base,"[:space:]","")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
            if(resolution=="Mois"){
              z = as.character(j)
              if(nchar(z)<2){z<-str_c("0",z)}
              beginning = str_c(y,"-",z,"-01")
              end = str_c(y,"-",z,"-",end_of_month[j])
              url<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=",z,"%2F01%2F",y,"&date2=",z,"%2F",end_of_month[j],"%2F",y,"&language=eng&ortext=&andtext=&phrasetext=",mot1,"&proxtext=&proxdistance=5&rows=100&searchType=advanced&sort=date")
              url_base<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=",z,"%2F01%2F",y,"&date2=",z,"%2F",end_of_month[j],"%2F",y,"&language=eng&ortext=&andtext=&proxtext=&proxdistance=5&rows=100&searchType=advanced&sort=date")
            }
            if (resolution=="Année"){
              url<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=01%2F01%2F",y,"&date2=12%2F31%2F",y,"&language=eng&ortext=&andtext=&phrasetext=",mot1,"&proxtext=&proxdistance=5&rows=100&searchType=advanced&sort=date")
              url_base<-str_c("https://chroniclingamerica.loc.gov/search/pages/results/?state=&dateFilterType=range&date1=01%2F01%2F",y,"&date2=12%2F31%2F",y,"&language=eng&ortext=&andtext=&proxtext=&proxdistance=5&rows=100&searchType=advanced&sort=date")
            }
          }
          
          if(input$doc_type == 43 & input$search_mode == 1){
            ngram<-read_html(RETRY("GET",url,times = 6))
            ngram<-as.character(ngram)
            ngram<-str_replace_all(ngram,"[:space:]","")
            ngram<-str_replace_all(ngram,"[:punct:]","")
            ngram<-str_extract(ngram,"resultscount.+")
            a<-str_extract(ngram,"[:digit:]+")
            if(incr_mot==1){
              ngram_base<-read_html(RETRY("GET",url_base,times = 6))
              ngram_base<-as.character(ngram_base)
              ngram_base<-str_replace_all(ngram_base,"[:space:]","")
              ngram_base<-str_replace_all(ngram_base,"[:punct:]","")
              ngram_base<-str_extract(ngram_base,"resultscount.+")
              b<-str_extract(ngram_base,"[:digit:]+")
            }
          }
          
          if(doc_type == 45 | doc_type==46 | doc_type==47 | doc_type==48 | doc_type==49){
            ngram<-as.character(read_html(RETRY("GET",url,times = 6)))
            a<-str_extract(ngram,'available":+[:digit:]+')
            a<-str_remove(a,'available":')
            if(incr_mot==1){
              ngram_base<-as.character(read_html(RETRY("GET",url_base,times = 6)))
              b<-str_extract(ngram_base,'available":+[:digit:]+')
              b<-str_remove(b,'available":')
            }
          }
          
          if(doc_type == 55){
            remDr$navigate(url)
            ngram <- remDr$getPageSource()[[1]]
            ngram=read_html(ngram)
            a<-str_extract(ngram,'Nombre de résultats :.+')
            a<-str_extract(a,'sur [:digit:]+')
            a<-str_remove(a,'sur ')
            if(incr_mot==1){
              remDr$navigate(url_base)
              ngram_base <- remDr$getPageSource()[[1]]
              ngram_base=read_html(ngram_base)
              b<-str_extract(ngram_base,'Nombre de résultats :.+')
              b<-str_extract(b,'sur [:digit:]+')
              b<-str_remove(b,'sur ')
            }
          }
          
          
          # if(input$doc_type == 65){
          #   ngram<-read_html(RETRY("GET",url,times = 6))
          #   ngram<-html_node(ngram,'#site-content > div.css-1wa7u5r > div.css-1npexfx > div.css-nhmgdh > p')
          #   ngram<-html_text(ngram)
          #   ngram<-str_replace_all(ngram,"[:punct:]","")
          #   a<-str_extract(ngram,"[:digit:]+")
          #   if(incr_mot==1){
          #     ngram_base<-read_html(RETRY("GET",url_base,times = 6))
          #     ngram_base<-html_node(ngram_base,'#site-content > div.css-1wa7u5r > div.css-1npexfx > div.css-nhmgdh > p')
          #     ngram_base<-html_text(ngram_base)
          #     ngram_base<-str_replace_all(ngram_base,"[:punct:]","")
          #     b<-str_extract(ngram_base,"[:digit:]+")
          #   }
          # }
          if(doc_type != 43){
          if(length(b)==0L){b=0}
          tableau[nrow(tableau)+1,] = NA
          date=y
          if(resolution=="Mois"){date = paste(y,z,sep="/")}
          tableau[nrow(tableau),]<-c(date,a,b,mot,url)
          progress$inc(1/((to-from+1)*length(I)*length(mots)), detail = paste("Gallicagram ratisse l'an", i))}
        }
      }
      
      
      
      
    }
  }
  colnames(tableau)<-c("date","count","base","mot","url")
  tableau$count[is.na(tableau$count)]<-0
  tableau$url = str_replace(tableau$url,"SRU","services/engine/search/sru")
  tableau$url = str_replace(tableau$url,"maximumRecords=1","maximumRecords=25")
  
  if(doc_type==13 | doc_type==14 | doc_type==28 | doc_type==29| doc_type==37 | doc_type==38 | doc_type==39 | doc_type==40| doc_type==55){
    remDr$closeServer()
    print("-----")
    if(se=="windows"){
      rD$server$stop()
      rm(rD)
      gc()
      system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)}
  }
  
  tableau$count<-as.integer(tableau$count)
  tableau$base<-as.integer(tableau$base)
  tableau$ratio<-tableau$count/tableau$base
  tableau$ratio[is.na(tableau$ratio)]<-0
  #ngram_viewer
  
  if(doc_type==5 | doc_type==9 | doc_type==10 | doc_type==12){
    entree=paste(unlist(mots),collapse="%2C")
    entree=str_replace_all(entree,"\\+","%2B")
    entree=str_replace_all(entree," ","\\+")
    if(doc_type==5){corpus="fr-2019"}
    if(doc_type==9){corpus="de-2019"}
    if(doc_type==10){corpus="en-2019"}
    if(doc_type==12){corpus="es-2019"}
    url=str_c("https://books.google.com/ngrams/json?content=",entree,"&year_start=",from,"&year_end=",to,"&corpus=",corpus,"&smoothing=0")
    aie=as.character(RETRY("GET",url,times=6))
    ouille=fromJSON(aie)%>% as.data.frame
    outch=as.data.frame(ouille)
    tableau=cbind(c(""),c(""),c(""))
    colnames(tableau)=c("date","ratio","mot")
    i=0
    for (mot in mots) {
      i=i+1
      a=cbind(seq(from,to),unlist(outch$timeseries[i]))
      a=as.data.frame(a)
      a$mot=mot
      colnames(a)=c("date","ratio","mot")
      tableau=rbind(tableau,a)
    }
    tableau=tableau[-1,]
    tableau=as.data.frame(tableau)
    
    tableau$search_mode<-"match"
    tableau$corpus="Presse"
    tableau$date<-as.character(tableau$date)
    tableau$count<-0
    tableau$url<-""
    if(doc_type==5){
      for (i in 1:length(tableau$date)) {
        base<-read.csv("base_livres_annees_ngram_fr.csv")
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_fr") 
      }}
    if(doc_type==9){
      for (i in 1:length(tableau$date)) {
        base<-read.csv("base_livres_annees_ngram_de.csv")
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_de") 
      }}
    if(doc_type==10){
      for (i in 1:length(tableau$date)) {
        base<-read.csv("base_livres_annees_ngram_en.csv")
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_en") 
      }}
    if(doc_type==12){
      for (i in 1:length(tableau$date)) {
        base<-read.csv("base_livres_annees_ngram_es.csv")
        tableau$url[i]=str_c("https://www.google.com/search?q=%22",tableau$mot[i],"%22&tbm=bks&tbs=cdr:1,cd_min:",tableau$date[i],",cd_max:",tableau$date[i],"&lr=lang_es") 
      }}
    
    base$date<-as.character(base$date)
    tableau<-left_join(tableau,base,by="date")
    tableau$base[is.na(tableau$base)]<-0
    tableau$ratio<-as.double(tableau$ratio)
  }
  
  if(doc_type==44 | doc_type==58 | doc_type==59 | doc_type==60 | doc_type==61 | doc_type==62 | doc_type==63 | doc_type==64){
    if(to==2022){to=as.character(Sys.Date())}
    else{to=str_c(to,"-12-31")}
    from=str_c(from,"-01-01")
    if(doc_type==44){pays="FR"}
    if(doc_type==58){pays="GB"}
    if(doc_type==59){pays="US"}
    if(doc_type==60){pays="AU"}
    if(doc_type==61){pays="ES"}
    if(doc_type==62){pays="DE"}
    if(doc_type==63){pays="AT"}
    if(doc_type==64){pays="NL"}
    a<-tryCatch({gtrends(keyword = mots,geo = pays,time = str_c(from," ",to), onlyInterest = T)},error=function(cond) {})
    a<-tryCatch({a$interest_over_time},error=function(cond) {})
    if(is.null(a)){
      a=as.data.frame(cbind("0000-01-01",0,"erreur"))
      colnames(a)=c("date","hits","keyword")
    }
    print(a)
    a<-as.data.frame(a)
    a$date<-as.character(a$date)
    tableau<-cbind(a$date,a$hits,a$keyword)
    tableau<-as.data.frame(tableau)
    colnames(tableau)<-c("date","count","mot")
    tableau$base=100
    tableau$count<-as.integer(tableau$count)
    tableau$count[is.na(tableau$count)]<-0
    tableau$ratio=tableau$count/tableau$base
    tableau$url="https://trends.google.fr/trends/"
    for(i in 1:length(tableau$url)-1){
      tableau$url[i]=str_c("https://www.google.fr/search?q=",tableau$mot[i],"&tbs=cdr:1,cd_min:",as.character.Date(as.Date(tableau$date[i]),format="%m/%d/%Y"),",cd_max:",as.character.Date(as.Date(tableau$date[i+1])-1,format="%m/%d/%Y"),",lr:lang_1fr&tbm=nws&source=lnt&lr=lang_fr")
    }
    tableau$url[length(tableau$url)]="https://trends.google.fr/trends/"
    tableau$date<-str_replace_all(tableau$date,"-","/")
  }
  
  ####Zeitungsportal
  if(doc_type == 43 & input$search_mode == 4){
    library(crul)
    library(glue)
    period = input$beginning:input$end
    if(input$resolution=="Mois"){
    end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
    for(mot in mots){
    show_modal_spinner(text=str_c("Patientez environ ",as.character(ceiling(as.integer(length(period)/2.5))*length(mots)*(1+11*(input$resolution=="Mois")))," secondes"))
    result = data.frame(year = rep(period,each=12),month= rep(1:12,length(period)),count = 0,n_page = NA,mot = mot,url=NA)
    reqlist = list()
    for(year in period){
      for(month in 1:12){
        year2 = year
        month2 = month+1
        if(month==12){
          year2 = year+1
          month2=1
        }
        search = paste(str_c("plainpagefulltext:",str_split(mot,"\\+")[[1]]),collapse = " OR ")
        url = URLencode(glue("https://api.deutsche-digitale-bibliothek.de/search/index/newspaper-issues/select?q={search}%20AND%20publication_date:%5b{year}-{month}-01T00:00:00.000Z%20TO%20{year2}-{month2}-01T00:00:00.000Z%5d&rows=1000000000&fl="))
        for(mot1 in str_split(mot,"\\+")[[1]]){
          url = glue("{url}termfreq(plainpagefulltext,{mot1}),")}
        url = URLencode(url)
        reqlist[[length(reqlist)+1]] = HttpRequest$new(url = url,progress = httr::progress())$retry("get")
        i = which(result$year == year & result$month == month)
        result$url[i] = glue("https://www.deutsche-digitale-bibliothek.de/search/newspaper?fromDay=1&toYear={year}&fromYear={year}&toDay={end_of_month[month]}&toMonth={month}&fromMonth={month}&language=ger&rows=100&query={mot}")
      }}
      responses <- AsyncQueue$new(.list = reqlist,bucket_size=100,sleep=0,verbose=True)
      responses$request()
      for(z in 1:length(reqlist)){
        page = fromJSON(responses$responses()[[z]]$parse("UTF-8"))
        if(page$response$numFound>0){result$count[z] = sum(page$response$docs)}
        print(result[z,])
      }
    base = read.csv("base_presse_ddb_mois_und.csv") 
    base = base[base$year %in% period,]
    result$base = base$count
    #result$date = as.Date(paste(result$year,result$month,sep = "-","01"),format = "%Y-%m-%d")
    result$date = paste(result$year,result$month,sep="/")
    result$ratio = result$count/result$base
    if(mot==mots[1]){tableau=result}
    else{tableau=rbind(tableau,result)}
    }
    remove_modal_spinner()
    }
    if(input$resolution=="Année"){
      show_modal_spinner(text=str_c("Patientez environ ",as.character(ceiling(as.integer(length(period)/2.5))*length(mots))," secondes..."))
      for(mot in mots){
        result = data.frame(year = period,count = 0,mot = mot,url=NA)
        reqlist = list()
        for(year in period){
          search = paste(str_c("plainpagefulltext:",str_split(mot,"\\+")[[1]]),collapse = " OR ")
          url = URLencode(glue("https://api.deutsche-digitale-bibliothek.de/search/index/newspaper-issues/select?q={search}%20AND%20publication_date:%5b{year}-01-01T00:00:00.000Z%20TO%20{year+1}-01-01T00:00:00.000Z%5d&rows=1000000000&fl="))
          for(mot1 in str_split(mot,"\\+")[[1]]){
          url = glue("{url}termfreq(plainpagefulltext,{mot1}),")}
          url = URLencode(url)
          reqlist[[length(reqlist)+1]] = HttpRequest$new(url = url,progress = httr::progress())$retry("get")
          i = which(result$year == year)
          result$url[i] = glue("https://www.deutsche-digitale-bibliothek.de/search/newspaper?fromDay=1&toYear={year}&fromYear={year}&toDay=31&toMonth=12&fromMonth=1&language=ger&rows=100&query={mot}")
          }
          responses <- AsyncQueue$new(.list = reqlist,bucket_size=100,sleep=0,verbose=True)
          responses$request()
          for(year in period){
            i = which(result$year == year)
            page = fromJSON(responses$responses()[[i]]$parse("UTF-8"))
            if(page$response$numFound>0){result$count[i] = sum(page$response$docs)}
            print(result[i,])
          }
        base = read.csv("base_presse_ddb_mois_und.csv") %>% group_by(year) %>% summarise(count = sum(count))
        base = base[base$year %in% period,]
        result$base = base$count
        #result$date = as.Date(paste(result$year,result$month,sep = "-","01"),format = "%Y-%m-%d")
        result$date = as.character(result$year)
        result$ratio = result$count/result$base
        if(mot==mots[1]){tableau=result}
        else{tableau=rbind(tableau,result)}
      }
    }
    remove_modal_spinner()
  }
  
  
  ####NYT
  if(doc_type==65){
    if(input$resolution=="Année"){base=read.csv("base_presse_annees_nyt.csv",encoding = "UTF-8",sep = ",")}
    if(input$resolution=="Mois"){base=read.csv("base_presse_mois_nyt.csv",encoding = "UTF-8",sep = ",")}
    base=base[base$annee>=input$beginning & base$annee<=input$end,]
    period = input$beginning:input$end
    show_modal_spinner(text=str_c("Patientez environ ",as.character(ceiling(as.integer(length(period)/12))*length(mots)*(1+11*(input$resolution=="Mois")))," secondes...\n Attention : le moteur de recherche du NYT produit parfois d'étranges pics et creux, auquel cas il faut relancer les calculs quelques minutes plus tard."))
    library(crul)
    for(mot in mots){
      reqlist = list()
      end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
      months = c("01","02","03","04","05","06","07","08","09","10","11","12")
      for(i in 1:length(period)){
        if(input$resolution=="Année"){reqlist[[i]] = HttpRequest$new(url = str_c("https://www.nytimes.com/search?dropmab=false&endDate=",period[i],'1231&query=',mot,'&sort=best&startDate=',period[i],"0101&types=article"))$get()}
        if(input$resolution=="Mois"){
          for(month in 1:12){
            reqlist[[length(reqlist)+1]] = HttpRequest$new(url = str_c("https://www.nytimes.com/search?dropmab=false&endDate=",period[i],months[month],end_of_month[month],'&query=',mot,'&sort=best&startDate=',period[i],months[month],"01&types=article"))$get()}
        }
      }
      responses <- AsyncQueue$new(.list = reqlist,bucket_size=30,sleep=0)
      responses$request()
      if(input$resolution=="Année"){result = data.frame(date=period,count=NA)}
      if(input$resolution=="Mois"){result = data.frame(annee = rep(period,each=12),mois= rep(months,length(period)),count = NA)
      result$date = paste(result$annee,result$mois,sep="/")}
      z = vector()
      for(i in 1:length(reqlist)){
        count = str_split(responses$responses()[[i]]$parse(),'totalCount":')[[1]][2]
        result[i,"count"] = str_split(count,",")[[1]][1]
        result[i,"url"] = reqlist[[i]]$url
      }
      #return(result)}
      #result = nyt_scraper(period)
      row.names(result) = result$date
      result$count = as.integer(result$count)
      for(b in 1:2){
      z = which(is.na(result$count))
      zz  =which(as.logical((result$count==0)*c(0,result$count[0:(nrow(result)-1)])>5))
      zzz = which(as.logical((result$count>3*c(result$count[1],result$count[0:(nrow(result)-1)]))*(result$count>3*c(result$count[2:(nrow(result))],result$count[nrow(result)]))))
      argmax = which.max(result$count)
      if((!argmax %in% zzz) & max(result$count,na.rm = T) > 3*max(result$count[-argmax],na.rm = T)){zzz = c(zzz,argmax)}
      if(length(z)+length(zz)+length(zzz)>0){
        reqlist = list()
        for(i in z){reqlist[[length(reqlist)+1]] = HttpRequest$new(url = result$url[i])$get()}
        
          for(i in c(zz,zzz)){
            if(input$resolution=="Année"){
            month = sample(3:9,1)
            reqlist[[length(reqlist)+1]] = HttpRequest$new(url = str_c("https://www.nytimes.com/search?dropmab=false&endDate=",period[i],months[month],end_of_month[month],'&query=',mot,'&sort=best&startDate=',period[i],"0101&types=article"))$get()
            reqlist[[length(reqlist)+1]] = HttpRequest$new(url = str_c("https://www.nytimes.com/search?dropmab=false&endDate=",period[i],'1231&query=',mot,'&sort=best&startDate=',period[i],months[month]+1,"01&types=article"))$get()
          }
        if(input$resolution=="Mois"){
          day = sample(10:20,1)
          reqlist[[length(reqlist)+1]] = HttpRequest$new(url = str_c("https://www.nytimes.com/search?dropmab=false&endDate=",result$annee[i],result$mois[i],day,'&query=',mot,'&sort=best&startDate=',result$annee[i],result$mois[i],"01&types=article"))$get()
          reqlist[[length(reqlist)+1]] = HttpRequest$new(url = str_c("https://www.nytimes.com/search?dropmab=false&endDate=",result$annee[i],result$mois[i],end_of_month[as.integer(result$mois[i])],'&query=',mot,'&sort=best&startDate=',result$annee[i],result$mois[i],day+1,"&types=article"))$get()
        }
        }
        
        responses <- AsyncQueue$new(.list = reqlist,bucket_size=20,sleep=0)
        responses$request()
        print("b")
        r = vector()
        for(i in 1:length(responses$responses())){
          count = str_split(responses$responses()[[i]]$parse(),'totalCount":')[[1]][2]
          r[i] = str_split(count,",")[[1]][1]
        }
        r= as.integer(r)
        #result2 = nyt_scraper(period[z])
        result[z,"count"] = as.integer(r[1:length(z)])
        result[c(zz,zzz),"count"] = as.integer(r[(length(z)+1):length(r)][c(T,F)])+as.integer(r[(length(z)+1):length(r)][c(F,T)])##Sum every two
      }
      }

      result$count=as.integer(result$count)
      if(input$resolution=="Année"){result$date=as.integer(result$date)}
      result=left_join(result,base[c("date","base")],by="date")
      result$ratio=result$count/result$base
      result$ratio[result$ratio>2] = NA
      result$mot=mot
      
      if(mot==mots[1]){tableau=result}
      else{tableau=rbind(tableau,result)}
      
    }
    remove_modal_spinner()
    tableau$date=as.character(tableau$date)
    mot_nyt_url = tableau$mot
  
    
    print(tableau)
  }
  ####
  
  if(doc_type==79){
    show_modal_spinner("Attention : les accents et signes diacritiques produisent un bug sur les années 2009-2010, qui ne sont pas interprétables.")
    library(glue)
    library(rvest)
    library(crul)
    period = input$beginning:input$end
    end_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)
    for (word in mots){
      if(input$resolution=="Année"){result = data.frame(year = period,month=1,count = 0,base=NA,mot = word,url=NA)}
      if(input$resolution=="Mois"){result = data.frame(year = rep(period,each=12),month= rep(1:12,length(period)),count = 0,base = NA,mot = word,url=NA)}
      reqlist = list()
      reqlist_total = list()
      for(i in 1:nrow(result)){
        year = result$year[i]
        month = result$month[i]
        end_month = 12*(input$resolution=="Année") + month*(input$resolution=="Mois")
        url = glue("https://www.lorientlejour.com/archives?action=search&title=&q={word}&method=exact&author=&category=all&dateFrom=01-{month}-{year}&dateTo={end_of_month[end_month]}-{end_month}-{year}") %>% URLencode()
        url_total = glue("https://www.lorientlejour.com/archives?action=search&title=&q=&method=and&author=&category=all&dateFrom=01-{month}-{year}&dateTo={end_of_month[end_month]}-{end_month}-{year}") %>% URLencode()
        if(year==year(Sys.Date()) & input$resolution=="Année"){ # In the current year, we have to stop the search at today, otherwise there is a bug
          url = glue("https://www.lorientlejour.com/archives?action=search&title=&q={word}&method=exact&author=&category=all&dateFrom=01-{month}-{year}&dateTo={day(Sys.Date())}-{month(Sys.Date())}-{year}") %>% URLencode()
          url_total = glue("https://www.lorientlejour.com/archives?action=search&title=&q=&method=and&author=&category=all&dateFrom=01-{month}-{year}&dateTo={day(Sys.Date())}-{month(Sys.Date())}-{year}") %>% URLencode()
        }
        if(year==year(Sys.Date()) & month==month(Sys.Date()) & input$resolution=="Mois"){#Stop at current day to avoid bug
          url = glue("https://www.lorientlejour.com/archives?action=search&title=&q={word}&method=exact&author=&category=all&dateFrom=01-{month}-{year}&dateTo={day(Sys.Date())}-{end_month}-{year}") %>% URLencode()
          url_total = glue("https://www.lorientlejour.com/archives?action=search&title=&q=&method=and&author=&category=all&dateFrom=01-{month}-{year}&dateTo={day(Sys.Date())}-{end_month}-{year}") %>% URLencode()
        }
        if((year==year(Sys.Date()) & month>month(Sys.Date())) | year > year(Sys.Date())){#Skip if the time step is in the future
          next}
        reqlist[[i]] = HttpRequest$new(url = url)$get()
        reqlist_total[[i]] = HttpRequest$new(url = url_total)$get()
        responses <- AsyncQueue$new(.list = reqlist,bucket_size=50,sleep=0)
      }
      
      responses <- AsyncQueue$new(.list = reqlist,bucket_size=50,sleep=0)
      responses$request()
      responses_total <- AsyncQueue$new(.list = reqlist_total,bucket_size=30,sleep=0)
      responses_total$request()
      
      for(i in 1:length(reqlist)){
        count = responses$responses()[[i]]$parse() %>% read_html() %>% html_node("h4") %>% html_text()
        result[i,"count"] = max(0,as.integer(str_split(count," ")[[1]][6]),na.rm=T)
        total = responses_total$responses()[[i]]$parse() %>% read_html() %>% html_node("h4") %>% html_text()
        result[i,"base"] = as.integer(str_split(total," ")[[1]][6])
        result[i,"url"] = reqlist[[i]]$url
      }
      result$date = paste(result$year,result$month,sep="/")
      if(word==mots[1]){tableau=result}
      else{tableau=rbind(tableau,result)}
    }
    tableau$ratio = tableau$count/tableau$base
    remove_modal_spinner()
  }
  if(doc_type==50 | doc_type==51 | doc_type==52 | doc_type==53 | doc_type==54){
    jjj=as.character(min(input$dateRange))
    kkk=as.character(max(input$dateRange))
    iii=0
    for(mot in mots){
      iii=iii+1
    if(doc_type==50){lang="fr"}
    if(doc_type==51){lang="en"}
    if(doc_type==52){lang="de"}
    if(doc_type==53){lang="nl"}
    if(doc_type==54){lang="es"}
    
    if(input$resolution=="Année"){res="year"
    jjj=str_c(from,"-01-01")
    kkk=str_c(to,"-12-31")}
    if(input$resolution=="Mois"){res="month"
    jjj=str_c(from,"-01-01")
    kkk=str_c(to,"-12-31")}
    if(input$resolution=="Semaine"){res="week"}
    if(input$resolution=="Jour"){res="day"}
    mot<-str_replace_all(mot,"\\+","%20OR%20")
    
    url<-str_c("https://api.mediacloud.org/api/v2/stories_public/count?q=",mot,"&fq=publish_date:%5B",jjj,"T00:00:00.000Z+TO+",kkk,"T00:00:00.000Z%5D&language=",lang,"&split=T&split_period=",res,"&key=b2ef1a99a8fdbb84afafe742fd437c0942703072bb98242ba0eed9b8411e1735")
    url_base<-str_c("https://api.mediacloud.org/api/v2/stories_public/count?q=a&fq=publish_date:%5B",jjj,"T00:00:00.000Z+TO+",kkk,"T00:00:00.000Z%5D&language=",lang,"&split=T&split_period=",res,"&key=b2ef1a99a8fdbb84afafe742fd437c0942703072bb98242ba0eed9b8411e1735")
    url=str_replace(url," ","%20")
    url_base=str_replace(url_base," ","%20")
    print(url)
    print("---")
    a<-as.data.frame(fromJSON(url))
    b<-as.data.frame(fromJSON(url_base))
    colnames(a)<-c("count","date")
    colnames(b)<-c("base","date")
    
    tableau<-left_join(a,b,by="date")
    tableau$date<-str_extract(tableau$date,"..........")
    tableau$mot<-mot
    if(iii==1){tableau1<-tableau}
    if(iii>=1){tableau1<-bind_rows(tableau1,tableau)}
    }
    tableau<-tableau1
    tableau$ratio<-tableau$count/tableau$base
    tableau$url<-""
    for (i in 1:length(tableau$ratio)) {
      e<-str_extract(tableau$date[i],".....")
      f<-str_extract(tableau$date[i],".......")
      f<-str_remove(f,".....")
      g<-tableau$date[i]
      g<-str_remove(g,"........")
      h<-str_extract(tableau$date[i+1],".....")
      j<-str_extract(tableau$date[i+1],".......")
      j<-str_remove(j,".....")
      k<-tableau$date[i+1]
      k<-str_remove(k,"........")
      #tableau$url[i]=str_c("https://www.google.fr/search?q=%22",mot,"%22&tbs=cdr:1,cd_min:",f,"/",g,"/",e,",cd_max:",j,"/",k,"/",h,",lr:lang_1fr&tbm=nws&source=lnt&lr=lang_fr&sa=X&ved=2ahUKEwiEjIKk3uH3AhUHbRoKHTMOBC8QpwV6BAgBEBU&biw=1280&bih=577&dpr=1.5")
      if(input$resolution=="Année"){tableau$url[i]=str_c("https://www.google.fr/search?q=%22",mot,"%22&tbs=cdr:1,cd_min:01/01/",e,",cd_max:31/12/",e,",lr:lang_1fr&tbm=nws&source=lnt&lr=lang_fr&sa=X&ved=2ahUKEwiEjIKk3uH3AhUHbRoKHTMOBC8QpwV6BAgBEBU&biw=1280&bih=577&dpr=1.5")}
      if(input$resolution=="Mois"){tableau$url[i]=str_c("https://www.google.fr/search?q=%22",mot,"%22&tbs=cdr:1,cd_min:01/",f,"/",e,",cd_max:01/",j,"/",h,",lr:lang_1fr&tbm=nws&source=lnt&lr=lang_fr&sa=X&ved=2ahUKEwiEjIKk3uH3AhUHbRoKHTMOBC8QpwV6BAgBEBU&biw=1280&bih=577&dpr=1.5")}
      if(input$resolution=="Semaine"){tableau$url[i]=str_c("https://www.google.fr/search?q=%22",mot,"%22&tbs=cdr:1,cd_min:",g,"/",f,"/",e,",cd_max:",k,"/",j,"/",h,",lr:lang_1fr&tbm=nws&source=lnt&lr=lang_fr&sa=X&ved=2ahUKEwiEjIKk3uH3AhUHbRoKHTMOBC8QpwV6BAgBEBU&biw=1280&bih=577&dpr=1.5")}
    }
    tableau$date<-str_replace_all(tableau$date,"-","/")
  }
  
  
  tableau$resolution<-resolution
  
  if(tableau$resolution[1]=="Mois"){
    auj<-Sys.Date()
    auj<-str_extract(auj,".......")
    auj<-str_replace(auj,"-","/")
    tableau<-tableau[tableau$date<=auj,]
    }
  
  if(doc_type==1){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Gallica"
  tableau$search_mode<-"Document"}
  if(doc_type==2){tableau$corpus="Livres"
  tableau$langue="Français"
  tableau$bibli="Gallica"
  tableau$search_mode<-"Document"}
  if(doc_type==56){tableau$corpus="Presse et livres"
  tableau$langue="Français"
  tableau$bibli="Gallica"
  tableau$search_mode<-"Document"}
  if(doc_type==5){tableau$corpus="Livres"
  tableau$langue="Français"
  tableau$bibli="Ngram Viewer"
  tableau$search_mode<-"N-gramme"}
  if(doc_type == 3){tableau$corpus="Titres de presse"
  tableau$langue="Français"
  tableau$bibli="Gallica"
  tableau$search_mode<-"Document"}
  if(doc_type==4){tableau$corpus="Corpus personnalisé"
  tableau$langue="Français"
  tableau$bibli="Gallica"
  tableau$search_mode<-"Document"}
  if(doc_type==6){tableau$corpus="Presse"
  tableau$langue="Allemand"
  tableau$bibli="Europeana"
  tableau$search_mode<-"Document"}
  if(doc_type==7){tableau$corpus="Presse"
  tableau$langue="Néerlandais"
  tableau$bibli="Europeana"
  tableau$search_mode<-"Document"}
  if(doc_type==8){tableau$corpus="Presse"
  tableau$langue="Anglais"
  tableau$bibli="BNA"
  tableau$search_mode<-"Article"}
  if(doc_type==9){tableau$corpus="Livres"
  tableau$langue="Allemand"
  tableau$bibli="Ngram Viewer"
  tableau$search_mode<-"N-gramme"}
  if(doc_type==10){tableau$corpus="Livres"
  tableau$langue="Anglais"
  tableau$bibli="Ngram Viewer"
  tableau$search_mode<-"N-gramme"}
  if(doc_type==11){tableau$corpus="Presse"
  tableau$langue="Espagnol"
  tableau$bibli="BNE"
  tableau$search_mode<-"Page"}
  if(doc_type==12){tableau$corpus="Livres"
  tableau$langue="Espagnol"
  tableau$bibli="Ngram Viewer"
  tableau$search_mode<-"N-gramme"}
  if(doc_type==13){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="KBR"
  tableau$search_mode<-"Page"}
  if(doc_type==14){tableau$corpus="Presse"
  tableau$langue="Néerlandais"
  tableau$bibli="KBR"
  tableau$search_mode<-"Page"}
  if(doc_type==15){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="BNS"
  tableau$search_mode<-"Article"}
  if(doc_type==16){tableau$corpus="Presse"
  tableau$langue="Allemand"
  tableau$bibli="BNS"
  tableau$search_mode<-"Article"}
  if(doc_type==17){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Lectura"
  tableau$search_mode<-"Page"}
  if(doc_type==18){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Limédia"
  tableau$search_mode<-"Document"}
  if(doc_type==19){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Mémonum"
  tableau$search_mode<-"Document"}
  if(doc_type==20){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Commun-Patrimoine"
  tableau$search_mode<-"Document"}
  if(doc_type==21){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Yroise"
  tableau$search_mode<-"Document"}
  if(doc_type==22){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Pireneas"
  tableau$search_mode<-"Document"}
  if(doc_type==23){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Rosalis"
  tableau$search_mode<-"Document"}
  if(doc_type==24){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="BDN"
  tableau$search_mode<-"Document"}
  if(doc_type==25){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="RFN"
  tableau$search_mode<-"Document"}
  if(doc_type==26){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Numistral"
  tableau$search_mode<-"Document"}
  if(doc_type==27){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="BN-R"
  tableau$search_mode<-"Document"}
  if(doc_type==28){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="BAnQ"
  tableau$search_mode<-"Document"}
  if(doc_type==29){tableau$corpus="Presse"
  tableau$langue="Allemand"
  tableau$bibli="ANNO"
  tableau$search_mode<-"Document"}
  if(doc_type==30){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Le Monde"
  tableau$search_mode<-"Article"}
  if(doc_type==31){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Le Figaro"
  tableau$search_mode<-"Article"}
  if(doc_type==32){tableau$corpus="Scientifique"
  tableau$langue="Français"
  tableau$bibli="Cairn"
  tableau$search_mode<-"Document"}
  if(doc_type==33){tableau$corpus="Scientifique"
  tableau$langue="Français"
  tableau$bibli="Theses.fr"
  tableau$search_mode<-"Document"}
  if(doc_type==34){tableau$corpus="Scientifique"
  tableau$langue="Français"
  tableau$bibli="Persée"
  tableau$search_mode<-"Document"}
  if(doc_type==35){tableau$corpus="Presse"
  tableau$langue="Anglais"
  tableau$bibli="Trove"
  tableau$search_mode<-"Article"}
  if(doc_type==36){tableau$corpus="Scientifique"
  tableau$langue="Français"
  tableau$bibli="Isidore"
  tableau$search_mode<-"Document"}
  if(doc_type==37 | doc_type==38 | doc_type==39 | doc_type==40){tableau$corpus="Presse"
  tableau$langue="Anglais"
  tableau$bibli="Newspapers.com"
  tableau$search_mode<-"Page"}
  if(doc_type==42){tableau$corpus="Presse"
  tableau$langue="Anglais"
  tableau$bibli="LOC"
  tableau$search_mode<-"Page"}
  if(doc_type==43){tableau$corpus="Presse"
  tableau$langue="Allemand"
  tableau$bibli="DDB"
  tableau$search_mode<-"Document"}
  if(doc_type==44){tableau$corpus="Web"
  tableau$langue="Français"
  tableau$bibli="Google Trends FR"
  tableau$search_mode<-"gtrends"}
  if(doc_type==45){tableau$corpus="Paroles"
  tableau$langue="Français"
  tableau$bibli="MusixMatch"
  tableau$search_mode<-"Document"}
  if(doc_type==46){tableau$corpus="Paroles"
  tableau$langue="Anglais"
  tableau$bibli="MusixMatch"
  tableau$search_mode<-"Document"}
  if(doc_type==47){tableau$corpus="Paroles"
  tableau$langue="Allemand"
  tableau$bibli="MusixMatch"
  tableau$search_mode<-"Document"}
  if(doc_type==48){tableau$corpus="Paroles"
  tableau$langue="Néerlandais"
  tableau$bibli="MusixMatch"
  tableau$search_mode<-"Document"}
  if(doc_type==49){tableau$corpus="Paroles"
  tableau$langue="Espagnol"
  tableau$bibli="MusixMatch"
  tableau$search_mode<-"Document"}
  if(doc_type==50){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="MediaCloud"
  tableau$search_mode<-"Document"}
  if(doc_type==51){tableau$corpus="Presse"
  tableau$langue="Anglais"
  tableau$bibli="MediaCloud"
  tableau$search_mode<-"Document"}
  if(doc_type==52){tableau$corpus="Presse"
  tableau$langue="Allemand"
  tableau$bibli="MediaCloud"
  tableau$search_mode<-"Document"}
  if(doc_type==53){tableau$corpus="Presse"
  tableau$langue="Néerlandais"
  tableau$bibli="MediaCloud"
  tableau$search_mode<-"Document"}
  if(doc_type==54){tableau$corpus="Presse"
  tableau$langue="Espagnol"
  tableau$bibli="MediaCloud"
  tableau$search_mode<-"Document"}
  if(doc_type==54){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Le Marin"
  tableau$search_mode<-"Page"}
  if(doc_type==58){tableau$corpus="Web"
  tableau$langue="Anglais"
  tableau$bibli="Google Trends GB"
  tableau$search_mode<-"gtrends"}
  if(doc_type==59){tableau$corpus="Web"
  tableau$langue="Anglais"
  tableau$bibli="Google Trends US"
  tableau$search_mode<-"gtrends"}
  if(doc_type==60){tableau$corpus="Web"
  tableau$langue="Anglais"
  tableau$bibli="Google Trends AU"
  tableau$search_mode<-"gtrends"}
  if(doc_type==61){tableau$corpus="Web"
  tableau$langue="Espagnol"
  tableau$bibli="Google Trends ES"
  tableau$search_mode<-"gtrends"}
  if(doc_type==62){tableau$corpus="Web"
  tableau$langue="Allemand"
  tableau$bibli="Google Trends DE"
  tableau$search_mode<-"gtrends"}
  if(doc_type==63){tableau$corpus="Web"
  tableau$langue="Allemand"
  tableau$bibli="Google Trends AT"
  tableau$search_mode<-"gtrends"}
  if(doc_type==64){tableau$corpus="Web"
  tableau$langue="Néerlandais"
  tableau$bibli="Google Trends NL"
  tableau$search_mode<-"gtrends"}
  if(doc_type==65){tableau$corpus="Presse"
  tableau$langue="Anglais"
  tableau$bibli="New York Times"
  tableau$search_mode<-"Article"}
  if(doc_type==79){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="L'Orient-Le Jour"
  tableau$search_mode<-"Article"}
  

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
    df$mot<-str_c(df$mot,"_",df$search_mode,"_",df$corpus,"_",df$resolution,"_",df$bibli)
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
    #require(Hmisc)
    
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
    
    R[is.nan(R)]<-0
    R[is.na(R)]<-0
    p[is.nan(p)]<-1
    p[is.na(p)]<-1
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

contempo<-function(input){
  mots = str_split(input$mot,"&")[[1]]
  from=min(input$dateRange)
  to=max(input$dateRange)
  y=from
  
  tableau<-as.data.frame(matrix(nrow=0,ncol=5),stringsAsFactors = FALSE)
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  
  while(y<to){
    z=y+days(6)
    incr_mot=0
    for(mot in mots){
      incr_mot=incr_mot+1
      mot2 = str_replace_all(mot," ","%20")
      mot2=URLencode(mot2)
      mot1=mot2
      
      if(input$doc_type == 30){
        url<-str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",mot1,"%22&start_at=",day(y),"%2F",month(y),"%2F",year(y),"&end_at=",day(z),"%2F",month(z),"%2F",year(z),"&search_sort=relevance_desc")
        url_base<-str_c("https://www.lemonde.fr/recherche/?search_keywords=le&start_at=",day(y),"%2F",month(y),"%2F",year(y),"&end_at=",day(z),"%2F",month(z),"%2F",year(z),"&search_sort=relevance_desc")
        
        page<-read_html(RETRY("GET",url,times = 6))
        ngram<-page
        ngram<-html_text(html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.river__pagination"))
        ngram<-str_squish(ngram)
        a<-str_extract(ngram,"([:digit:]+)$")
        
        if(is.na(a)){
          ngram<-page
          ngram<-html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.js-river-search")
          ngram<-as.character(ngram)
          a<-str_count(ngram,"teaser teaser--inline-picture")
        }
        else{
          url<-str_c(url,"&page=",a)
          page<-read_html(RETRY("GET",url,times = 6))
          ngram<-page
          ngram<-html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.js-river-search")
          ngram<-as.character(ngram)
          c<-str_count(ngram,"teaser teaser--inline-picture")
          a<-as.integer(a)
          c<-as.integer(c)
          a<-(a-1)*40+c
        }
        if(incr_mot==1){
          ngram<-read_html(RETRY("GET",url_base,times = 6))
          ngram<-html_text(html_node(ngram,"#js-body > main > article > section > section.page__float > section.page__content.river.river--rubrique.river--search > section.river__pagination"))
          ngram<-str_squish(ngram)
          b<-str_extract(ngram,"([:digit:]+)$")
          b<-as.integer(b)
          b=40*b
        }
      }
      if(input$doc_type == 31){
        url<-str_c("https://recherche.lefigaro.fr/recherche/",mot1,"/?publication=lefigaro.fr&datemin=",day(y),"-",month(y),"-",year(y),"&datemax=",day(z),"-",month(z),"-",year(z))
        url_base<-str_c("https://recherche.lefigaro.fr/recherche/_/?publication=lefigaro.fr&datemin=",day(y),"-",month(y),"-",year(y),"&datemax=",day(z),"-",month(z),"-",year(z))
        ngram<-read_html(RETRY("GET",url,times = 6))
        ngram<-as.character(html_node(ngram,".facettes__nombre"))
        ngram<-str_replace_all(ngram,"[:space:]","")
        a<-str_extract(ngram,"[:digit:]+")
        if(incr_mot==1){
          ngram_base<-read_html(RETRY("GET",url_base,times = 6))
          ngram_base<-as.character(html_node(ngram_base,".facettes__nombre"))
          ngram_base<-str_replace_all(ngram_base,"[:space:]","")
          b<-str_extract(ngram_base,"[:digit:]+")
        }        
      }
      if(length(b)==0L){b=0}
      tableau[nrow(tableau)+1,] = NA
      tableau[nrow(tableau),]<-c(as.character(y),a,b,mot,url)
      progress$inc(7/(as.integer(to-from)*length(mots)), detail = str_c(""))
      
      
    }
    y=y+days(7)
  }
  colnames(tableau)<-c("date","count","base","mot","url")
  tableau$count[is.na(tableau$count)]<-0
  tableau$count<-as.integer(tableau$count)
  tableau$base<-as.integer(tableau$base)
  tableau$ratio<-tableau$count/tableau$base
  tableau$ratio[is.na(tableau$ratio)]<-0
  tableau$resolution<-input$resolution
  if(input$doc_type==30){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Le Monde"
  tableau$search_mode<-"Article"}
  if(input$doc_type==31){tableau$corpus="Presse"
  tableau$langue="Français"
  tableau$bibli="Le Figaro"
  tableau$search_mode<-"Article"}
  
  memoire<<-bind_rows(tableau,memoire)
  data = list(tableau,paste(mots,collapse="&"),input$resolution)
  names(data) = c("tableau","mot","resolution")
  return(data)
}


cartographie<-function(input){
  from=min(input$cartoRange)
  to=max(input$cartoRange)
  from=str_replace_all(from,"-","/")
  to=str_replace_all(to,"-","/")
  mot=as.character(input$cartoMot)
  fra <- readRDS("gadm36_FRA_2_sf.rds")
  villes<-read.csv("villes.csv",encoding = "UTF-8",sep=",")
  
  fra$count<-0
  fra$base<-0
  fra$CC_2<-as.integer(fra$CC_2)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Patience...", value = 0)
  
  mot2 = str_replace_all(mot," ","%20")
  mot2=URLencode(mot2)
  mot1=""
  
  
  or<-""
  or_end<-""
  if(str_detect(mot2,"[+]")){
    mots_or = str_split(mot2,"[+]")[[1]]
    or1<-NA
    or1_end<-NA
    
    if(str_detect(mots_or[1],"[*]")){
      mots_co = str_split(mots_or[1],"[*]")[[1]]
      mot1<-str_c("(%20text%20adj%20%22",mots_co[1],"%22%20%20prox/unit=word/distance=",prox,"%20%22",mots_co[2],"%22)")
    }
    if(str_detect(mots_or[1],"[*]")==F){
      mot1<-str_c("(%20text%20adj%20%22",mots_or[1],"%22%20)")
    }
    
    for (j in 2:length(mots_or)) {
      if(str_detect(mots_or[j],"[*]")==F){
        or1[j]<-str_c("or%20(%20text%20adj%20%22",mots_or[j],"%22%20)")
        if(mot1==""){mot1<-str_c("(%20text%20adj%20%22",mots_or[1],"%22%20)")}
      }
      else{
        mots_co = str_split(mots_or[j],"[*]")[[1]]
        or1[j]<-str_c("%20or%20(%20text%20adj%20%22",mots_co[1],"%22%20%20prox/unit=word/distance=",prox,"%20%22",mots_co[2],"%22)")
      }
      
      or<-str_c(or,or1[j])
      or_end<-str_c(or_end,or1_end[j])
      
    }
  }
  else{mot1=str_c("%20text%20adj%20%22",mot2,"%22%20")}
  
  for (i in 1:length(fra$CC_2)) {
    v<-NA
    v<-villes[villes$Code_postal==i,]
    if(length(v$Code_postal)==0){next}
    if(length(v$Code_postal)==1){
      url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(notice%20all%20%22",v$Nom_commune[1],"%22%20)%20and%20(dc.language%20all%20%22fre%22)%20and%20(",mot1,or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",from,"%22%20and%20gallicapublication_date%3C=%22",to,"%22)")
      url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=(notice%20all%20%22",v$Nom_commune[1],"%22%20)%20and%20(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",from,"%22%20and%20gallicapublication_date%3C=%22",to,"%22)&suggest=10&keywords=")
    }
    if(length(v$Code_postal)>=2){
      chain<-str_c("notice%20all%20%22",v$Nom_commune[1],"%22%20")
      for(j in 2:length(v$Code_postal)){
        chain<-str_c(chain,"or%20notice%20all%20%22",v$Nom_commune[j],"%22%20")
      }
      chain<-str_c("(",chain,")%20and%20")
      chain<-str_replace_all(chain," ","%20")
      
      url<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=",chain,"(dc.language%20all%20%22fre%22)%20and%20(",mot1,or,")%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",from,"%22%20and%20gallicapublication_date%3C=%22",to,"%22)")
      url_base<-str_c("https://gallica.bnf.fr/SRU?operation=searchRetrieve&exactSearch=true&maximumRecords=1&page=1&collapsing=false&version=1.2&query=",chain,"(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%22",from,"%22%20and%20gallicapublication_date%3C=%22",to,"%22)&suggest=10&keywords=")
    }
    ngram<-as.character(read_xml(RETRY("GET",url,times = 6)))
    a<-str_extract(str_extract(ngram,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
    ngram_base<-as.character(read_xml(RETRY("GET",url_base,times = 6)))
    b<-str_extract(str_extract(ngram_base,"numberOfRecordsDecollapser&gt;+[:digit:]+"),"[:digit:]+")
    
    
    fra$count[fra$CC_2==i]<-a
    fra$base[fra$CC_2==i]<-b
    progress$inc(1/length(fra$CC_2), detail = paste("Gallicagram fouille le département ", fra$CC_2[i]))
  }
  fra$count<-as.integer(fra$count)
  fra$base<-as.integer(fra$base)
  fra$count[is.na(fra$count)]<-0
  fra$base[is.na(fra$base)]<-0
  fra$val=fra$count/fra$base
  fra$val=100*fra$val
  fra$val[is.na(fra$val)]<-0
  
  return(fra)
}

cartoPlot<-function(input,fra){
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = fra$val
  )
  labels <- sprintf("<strong>%s</strong><br/>%s",fra$NAME_2, str_c(fra$count,"/",fra$base,"=",round(fra$val,1),"%")) %>% lapply(htmltools::HTML)
  
  leaflet(fra) %>% addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
    setView( lat=46, lng=2 , zoom=5) %>%
    addPolygons( data=fra, fill=T, fillColor = ~pal(fra$val), fillOpacity = 1,color = "white",
                 dashArray = "3", stroke=F,label=labels,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"))%>%
    addLegend("bottomright", pal = pal, values = ~val,
              title = "",
              labFormat = labelFormat(suffix = "%"),
              opacity = 1
    )
}

cartoPicture<-function(fra,titre,from,to,colorscale){
  fra$val<-fra$val/100
  titre=str_c(titre,"\n",from," - ",to)
  plot=ggplot(data = fra) + geom_sf(aes(fill = val))+
    scale_fill_viridis_b(option = "B",direction = -1)+
    theme_classic()+theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.ticks.x = element_blank(),axis.text.x = element_blank(),
                          axis.ticks.y = element_blank(),axis.text.y = element_blank(),
                          line = element_blank())+ labs(fill = titre)
  
  return(plot)
}

cartoGramme<-function(fra,titre,from,to,colorscale){
  fra$val<-fra$val/100
  st_crs(fra)
  fra<-st_transform(fra,crs = 2154)
  titre=str_c(titre,"\n",from," - ",to)
  #  plot<-cartogram_cont(fra, "base", itermax = 5,prepare="adjust",threshold = 0.3)
  plot<-cartogram_dorling(fra, "base", itermax = 5)
  plot=ggplot(data = plot) + geom_sf(aes(fill = val))+
    scale_fill_viridis_b(option = "B",direction = -1)+
    theme_classic()+theme(plot.background = element_rect(fill = 'white', colour = 'white'),axis.ticks.x = element_blank(),axis.text.x = element_blank(),
                          axis.ticks.y = element_blank(),axis.text.y = element_blank(),
                          line = element_blank())+ labs(fill = titre)
  
  return(plot)
}
willisation <- function(input,will){
  if(will==""){
    mois=""
    fromm=isolate(input$beginning)
    too=isolate(input$end)
    word = str_split(input$mot,"&")[[1]]
    word = str_split(word,"[+]")[[1]]
    word=word[1]
  }
  else{
    fromm=str_extract(will,"gallicapublication_date.+")
    fromm=str_remove_all(fromm,"%22%20.+")
    fromm=str_remove_all(fromm,"gallicapublication_date%3E=%22")
    too=str_extract(will,"and%20gallicapublication_date.+")
    too=str_remove_all(too,"and%20gallicapublication_date%3C=%22")
    too=str_remove_all(too,"%22.+")
    word=str_extract(will,"text%20adj%20%22.+")
    word=str_remove_all(word,"text%20adj%20%22")
    word=str_remove_all(word,"%22%20.+")
    mois=""
    if(isolate(input$resolution=="Mois")){mois=str_c("&month=",str_extract(str_remove(fromm,"....."),".."))}
  
    gpt_start<<-fromm
    gpt_end<<-too
    gpt_word<<-word
  }
  c=data.frame(titre_journal=character(),
               date=character(),
               contexte_gauche=character(),
               pivot=character(), 
               contexte_droit=character(), 
               stringsAsFactors=FALSE) 
  colnames(c)=c("Titre du journal","Date de publication","Contexte gauche","Pivot","Contexte droit")
  pages = 0
  if(nchar(input$apikey)>0){pages = 0:3}
  for (i in pages) {
    
    if(isolate(input$doc_type==1)){
      will_url=str_c("https://gallica-grapher.ew.r.appspot.com/api/gallicaRecords?terms=%22",word,"%22&source=periodical&sort=relevance&year=",str_extract(fromm,"...."),mois,"&row_split=true&cursor=",i,"0")
    }
    if(isolate(input$doc_type==2)){
      will_url=str_c("https://gallica-grapher.ew.r.appspot.com/api/gallicaRecords?terms=%22",word,"%22&source=book&sort=relevance&year=",str_extract(fromm,"...."),mois,"&row_split=true&cursor=",i,"0")
    }
    if(isolate(input$doc_type)==56){
      will_url=str_c("https://gallica-grapher.ew.r.appspot.com/api/gallicaRecords?terms=%22",word,"%22&sort=relevance&year=",str_extract(fromm,"...."),mois,"&row_split=true&cursor=",i,"0")
    }
    if(isolate(input$doc_type) %in% 66:76){
      codes = c("cb34355551z","cb327877302","cb32747578p","cb327986698","cb34452336z","cb34431794k","cb32895690j","cb34419111x","cb34378481r","cb39294634r","cb34448033b")
      names(codes) = as.character(66:76)
      will_url=str_c("https://gallica-grapher.ew.r.appspot.com/api/gallicaRecords?terms=",word,"&source=periodical&sort=relevance&year=",str_extract(fromm,"...."),mois,"&row_split=true&cursor=",i,"0&codes=",codes[as.character(input$doc_type)])
      print(word)
      print(fromm)
      print(mois)
    }
    will_url=URLencode(will_url)
    will_url = str_replace_all(will_url,pattern=" ",replacement = "%20")
    wurl<<-will_url
    print(will_url)
    show_spinner(spin_id = "contexte")
    a<-tryCatch({fromJSON(will_url)%>%data.frame()},error=function(cond){return(NULL)})
    if(is.null(a)==F){
      b=data.frame(titre_journal=character(),
                   date=character(),
                   contexte_gauche=character(),
                   pivot=character(), 
                   contexte_droit=character(), 
                   stringsAsFactors=FALSE) 
      for (i in 1:length(a$records.paper_title)) {
        url_titre=str_c("<a href='",a$records.context[[i]]$page_url,"' target='_blank'>",a$records.paper_title[i],"</a>")
        if(is.null(a$records.context[[i]]$pivot)){
          a$records.context[[i]] = data.frame(pivot="",left_context="",right_context="",page_url="",page_num="")
        }
        b=rbind(b,cbind(url_titre,a$records.date[i],a$records.context[[i]]$left_context,a$records.context[[i]]$pivot,a$records.context[[i]]$right_context))
      }
      for (j in 1:length(b$V1)) {
        b$V1[j]=HTML(b$V1[j])
      }
      colnames(b)=c("Titre du journal","Date de publication","Contexte gauche","Pivot","Contexte droit")
      b=b[,-6]
      c=bind_rows(c,b)
    }
    else{b=NULL
         c=NULL}
  
}
  gpt_context<<-paste(c[,2],c[,3],c[,4],c[,5])
  gpt_context<<- str_replace_all(gpt_context, "\\s+", " ")
  gpt_context<<- str_replace_all(gpt_context, "[^A-Za-z0-9.,?!;:()/ ]", "")
  gpt_context<<-paste(gpt_context,collapse = "\n")
  return(c)
}

gptiseur<-function(input){
  print(str_c("Context length : ",str_length(gpt_context)))
  if(input$apikey==""){return("")}
  api_key <- input$apikey
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo-16k",
      messages = list(list(role = "system", content = "Tu es un robot historien. A partir d\'extraits de journaux tu dois trouver dans quel contexte historique ils ont été écrits. Tu dois expliquer en détail ce qu\'il s\'est passé durant la période que tu vas identifier. Sois précis, détaille le contexte historique, les évènements, les acteurs en présence, les enjeux politiques, sociaux ou économiques. Les extraits peuvent faire référence à plusieurs évènements, distingue les. Réponds de façon concise. Ecris au présent de l'indicatif ou au passé composé. Réponds par une liste à tiret pour distinguer ces évènements. Ne fais pas de conclusion."),
                      list(role = "user", content = str_c('Voilà des extraits de journaux parus durant la période ',gpt_start,' - ',gpt_end,' et contenant le mot ',gpt_word,'. Trouve dans quel contexte historique ce mot a été écrit. Explique en détail ce qu\'il s\'est passé durant cette période là. 400 mots maximum, pas de phrase de conclusion, pas de résumé de ta réponse. Réponse au présent. \n Extraits de journaux : \n <|startoftext|> ',gpt_context,' <|endoftext|>'))
                      ),
      max_tokens=600
    )
  )
  print(str_c("Prompt tokens : ",content(response)$usage$prompt_tokens))
  print(str_c("Completion tokens : ",content(response)$usage$completion_tokens))
  
  chatGPT_answer <- content(response)$choices[[1]]$message$content
  chatGPT_answer <- stringr::str_trim(chatGPT_answer)
  chatGPT_answer<-str_replace_all(chatGPT_answer,"\n","<br>")
  chatGPT_answer<-str_c('<font color="#ffffff">',str_c("Context length : ",str_length(gpt_context)),str_c(" Prompt tokens : ",content(response)$usage$prompt_tokens),str_c(" Completion tokens : ",content(response)$usage$completion_tokens),'</font> <br>',chatGPT_answer)
  chatGPT_answer<-str_c("<b>Synthèse du contexte proposée par ChatGPT </b>",chatGPT_answer)
  
  chatGPT_answer<-HTML(chatGPT_answer)
  
  
  return(chatGPT_answer)
}



gallicapresse<-function(input){
  show_spinner(spin_id="gpresse")
  mot=str_replace_all(isolate(input$cartoMot)," ","+")
  mot1=str_replace_all(isolate(input$cartoMot)," ","%20")
  mot=URLencode(mot)
  mot1=URLencode(mot1)
  debut=min(isolate(input$cartoRange))
  fin=max(isolate(input$cartoRange))
  year=str_extract(debut,"....")
  month=str_extract(debut,".......")
  month=str_remove(month,".....")
  day=str_remove(debut,"........")
  year_end=str_extract(fin,"....")
  month_end=str_extract(fin,".......")
  month_end=str_remove(month_end,".....")
  day_end=str_remove(fin,"........")
  debut=URLencode(str_replace_all(debut,"-","/"))
  fin=URLencode(str_replace_all(fin,"-","/"))
  
  url=str_c("https://gallica-grapher.ew.r.appspot.com/api/top?term=",mot,"&year=",year,"&month=",month,"&day=",day,"&end_year=",year_end,"&end_month=",month_end,"&end_day=",day_end)

  a<-tryCatch(
    {a=fromJSON(url)},error=function(cond){
      a<-tryCatch(
        {a=fromJSON(url)},error=function(cond){
          a<-tryCatch(
            {a=fromJSON(url)},error=function(cond){
            }
          )
        }
      )
    }
  )
  
  b=as.data.frame(cbind(a$top_papers$paper$title,a$top_papers$count,a$top_papers$paper$code))
  colnames(b)=c("title","count","url")
  b$url=str_c("https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&version=1.2&startRecord=0&maximumRecords=50&page=1&collapsing=disabled&query=arkPress%20all%20%22",b$url,"_date%22%20and%20%28gallica%20adj%20%22",mot1,"%22%20and%20%28gallicapublication_date%3E%3D%22",debut,"%22%20and%20gallicapublication_date%3C%3D%22",fin,"%22%29%20sortby%20dc.date%2Fsort.ascending")
  b$title=str_replace_all(b$title,":.+","")
  b$title=str_replace_all(b$title,"/.+","")
  b$count=as.integer(b$count)
  plot=plot_ly(data=b,x=~count,y=~reorder(title,count),type = "bar",orientation="h",customdata=b$url)%>%
    layout(xaxis = list(title = ""),yaxis = list(title =""))
  c=as.data.frame(cbind(a$top_cities$city,a$top_cities$count))
  colnames(c)=c("ville","count")
  c$ville=str_replace_all(c$ville,"\\[s\\.n\\.\\]","")
  c$ville=str_replace_all(c$ville,"\\(","")
  c$ville=str_replace_all(c$ville,"\\)","")
  c$ville=str_trim(c$ville,side="both")
  c$count=as.integer(c$count)
  c %>%group_by(ville) %>%
    summarise(count = sum(count))
  plot1=plot_ly(data=c,x=~count,y=~reorder(ville,count),type = "bar",orientation="h")%>%
    layout(xaxis = list(title = ""),yaxis = list(title =""),showlegend = F)
  plot=subplot(plot,plot1,nrows = 2)
  
  hide_spinner(spin_id="gpresse")
  return(onRender(plot,js))
}
  
options(shiny.maxRequestSize = 100*1024^2)

shinyServer(function(input, output,session){
  shinyalert(
    title = "Chargement...",
    text = "Benjamin Azoulay et Benoît de Courson, « Gallicagram, un outil de lexicométrie pour la recherche », 2021, https://doi.org/10.31235/osf.io/84bf3",
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#377EB8",
    timer = 3000,
    imageUrl = "",
    animation = TRUE
  )
  show_modal_spinner()
  
  
  will<-""
  wurl<<-""
  observe({
    data$e <- event_data("plotly_click")
    if(is.null(data$e)==F& isolate(input$contextualisation)==T&(isolate(input$doc_type==1) | isolate(input$doc_type==2) | isolate(input$doc_type==56) | isolate(input$doc_type %in% 66:76))){
      will<<-as.character(unlist(data$e$customdata))
      b=willisation(input,will)
      output$gpt=renderUI(HTML(gptiseur(input)))
        if(is.null(b)==F){
        wurl=str_replace(wurl,"https://gallica-grapher.ew.r.appspot.com/api/gallicaRecords","https://www.gallicagrapher.com/context")
        wurl=str_remove_all(wurl,"&row_split=true&cursor=00t")
        output$lien=renderUI(HTML(str_c("<b><font size=\"5\">Contexte</font><br>Crédit : Will Gleason avec <a href='","https://www.gallicagrapher.com/","' target='_blank'>","Gallicagrapher","</a></b></font>","<br><a href='",will,"' target='_blank'>","Ouvrir la recherche dans Gallica","</a>",
                                        "<br><a href='",wurl,"' target='_blank'>","Plus de contexte","</a>")))
        require("DT")
        output$frame<-renderDataTable(b,escape = F,options = list(pageLength = 10, lengthChange = FALSE, columnDefs = list(list(className = 'dt-body-right', targets = 3))))
    
        
        shinyjs::runjs("const target = document.querySelector('#legende');
                                                     target.scrollIntoView(behavior='smooth');")
        }
    }
    hide_spinner(spin_id = "contexte")
  })
  observeEvent(input$visualiseur,{
    if(input$visualiseur==6 | input$visualiseur==9){
      shinyjs::hide(id="afcspace",anim = F)
      shinyjs::show(id="afcline",anim = F)
      # shinyjs::hide(id="afcspace1",anim = F)
      # shinyjs::show(id="afcmois",anim = F)
      output$legende0=renderText(HTML(paste("Affichage : Gallicagram feat. ","<a href = 'http://hyperbase.unice.fr/hyperbase/', target=\'_blank\'> ","Hyperbase","</a>"),sep = ""))
      }
    else{
      shinyjs::hide(id="afcline",anim = F)
      shinyjs::show(id="afcspace",anim = F)
      shinyjs::hide(id="afcmois",anim = F)
      shinyjs::hide(id="afcspace1",anim = F)#à changer  en show pour activer la saisonnalité
      output$legende0=renderText("Affichage : Gallicagram par B. Azoulay et B. de Courson")
    }
    if(input$visualiseur==2 | input$visualiseur==3 | input$visualiseur==6 | input$visualiseur==7 | input$visualiseur==9  | input$visualiseur==10 | input$visualiseur==11){shinyjs::hide(id="span",anim = F)}
    else{shinyjs::show(id="span",anim = F)}
  })
  
  observeEvent(input$isMobile,{
    if(input$isMobile==T){
      shinyjs::hide(id="Sidebar",anim = T)
      shinyjs::hide(id="leg")
      shinyjs::hide(id="clip")
      shinyjs::hide(id="article")
      shinyjs::hide(id="pyllicagram")
      shinyjs::hide(id="mode")
      shinyjs::hide(id="modemob2")
      observeEvent(input$do, {
        shinyjs::hide(id = "Sidebar")
      })
    }
    if(input$isMobile==F){
      shinyjs::hide(id="menumob")
      shinyjs::hide(id="menumob2")
      shinyjs::hide(id="modemob")
    }
  })
  
  observeEvent(input$showSidebar, {
    shinyjs::toggle(id = "Sidebar",anim = T)
  })
  observeEvent(input$showSidebar2, {
    shinyjs::toggle(id = "Sidebar",anim = T)
  })
  observeEvent(input$mode, {
  output$style <- renderUI({
    if(isTRUE(input$mode)){
        includeCSS("www/cyborg.css")
      } else {
        includeCSS("www/default.css")
      }
  })
  })
  observeEvent(input$modemob, {
    output$style <- renderUI({
      if(isTRUE(input$modemob)){
        includeCSS("www/cyborg.css")
      } else {
        includeCSS("www/default.css")
      }
    })
  })
  observeEvent(input$correlation_test, {
    shinyjs::toggle(id = "corr",anim = T,condition = input$correlation_test)
    shinyjs::toggle(id = "corr2",anim = T,condition = input$correlation_test)
    shinyjs::toggle(id = "pvalue",anim = T,condition = input$correlation_test)
  })
  observeEvent(input$gallicloud, {
    shinyjs::toggle(id = "cloud",anim = F,condition = input$gallicloud)
    shinyjs::toggle(id = "plot",anim = F,condition = input$gallicloud==F)
    shinyjs::toggle(id = "mot",anim = F,condition = input$gallicloud==F)
    shinyjs::toggle(id = "resolution",anim = F,condition = input$gallicloud==F)
    shinyjs::toggle(id = "mess",anim = F,condition = input$gallicloud==F)
  })
  observeEvent(input$joker, {
    if(input$joker==T & (input$doc_type==1 | input$doc_type==2 | input$doc_type==30) & input$search_mode==3){updateSelectInput(session,"visualiseur", "",choices = list("Courbes"=1, "Sommes"=2, "Histogramme"=3, "Bulles"=4,"Aires"=5,"Nuage de mots"=7,"Polaires"=8,"ACP"=6,"AFC"=9,"Ctre de gravité"=10,"Rayures"=11),selected = 2)}
    else{updateSelectInput(session,"visualiseur", "",choices = list("Courbes"=1, "Sommes"=2, "Histogramme"=3, "Bulles"=4,"Aires"=5,"Nuage de mots"=7,"Polaires"=8,"ACP"=6,"AFC"=9,"Ctre de gravité"=10,"Rayures"=11),selected = 1)}
    
  })
  
  hide(id="gallicloud")
  hideTab("#navbar","Gallicapresse")
  hideTab("#navbar","Gallicanet")
  hideTab("#navbar","English version")
  
  temp<<-list()
  data=list(read.csv("exemple.csv",encoding = "UTF-8"),"liberté&république","Mois")
  names(data)=c("tableau","mot","resolution")
  memoire<<-read.csv("exemple.csv",encoding="UTF-8")
  memoire$date<<-as.character(memoire$date)
  recherche_precedente<<-"liberté&république_1788_1805_Année"
  corpus_precedent<<-"1_1"
  counter<<-0
  output$themes_presse<- renderUI({selectizeInput("theme_presse","Thématique",choices = list("Liste de titres personnalisée"=1))})
  output$theme<- renderUI({selectizeInput("dewey","Thématique",choices = list("-"="999"))})
  options(warn = -1)
  set.seed(42)
  initcloud=data.frame(mot=c("liberté","république"), count=c(168035,226300))
  cl=ggplot(initcloud, aes(label = mot, size = count)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 24) +
    theme_minimal()
  output$cloud=renderPlot(cl)
  
  observeEvent(input$doc_type,{observeEvent(input$search_mode,{observeEvent(input$cooccurrences,{observeEvent(input$prox,{
    observeEvent(input$joker,{
      observeEvent(input$gallicloud,{
      if(input$cooccurrences==T & ((input$doc_type == 1 & input$search_mode == 1)|(input$doc_type == 2 & input$search_mode == 1)|(input$doc_type == 3 & input$search_mode == 1))){
        output$instructions <- renderUI(HTML(str_c('<ul><li>Utiliser "a*b" pour rechercher a à ',input$prox,' mots maximum de b</li><li>Séparer les termes par un "&" pour une recherche multiple</li><li>Utiliser "a+b" pour rechercher a OU b</li><li>Cliquer sur un point du graphique pour accéder aux documents dans la bibliothèque numérique correspondante</li></ul>')))
        
      }else if(input$joker==T & ((input$doc_type == 1 & input$search_mode == 3)|(input$doc_type == 2 & input$search_mode == 3))){
        output$instructions <- renderUI(HTML(str_c('<ul><li>Utiliser "a _" ou "_ a" pour rechercher les n+1 grammes contenant a. "Napoléon _" renvoie Napoléon Ier, Napoléon III, etc. "_ Napoléon" renvoie Code Napoléon, etc. </li><li>Les mots vides sont les mots les plus fréquents de la langue française.</li><li>Cliquer sur un point du graphique pour accéder aux documents dans la bibliothèque numérique correspondante</li></ul>')))
        
      }else if((input$doc_type == 1 & input$search_mode == 1)|(input$doc_type == 2 & input$search_mode == 1)|(input$doc_type == 3 & input$search_mode == 1)|(input$doc_type == 1 & input$search_mode == 3)|(input$doc_type == 2 & input$search_mode == 3)|input$doc_type == 5|input$doc_type == 6|input$doc_type == 7|input$doc_type == 8|input$doc_type == 9|input$doc_type == 10|input$doc_type == 11|input$doc_type == 12|input$doc_type == 15|input$doc_type == 16|input$doc_type == 19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 29){
        output$instructions <- renderUI(HTML('<ul><li>Séparer les termes par un "&" pour une recherche multiple</li><li>Utiliser "a+b" pour rechercher a OU b</li><li>Cliquer sur un point du graphique pour accéder aux documents dans la bibliothèque numérique correspondante</li></ul>'))
        
      }else if(input$doc_type==13|input$doc_type==14|input$doc_type==17|input$doc_type==18 | input$doc_type == 27 | input$doc_type == 28 | input$doc_type == 30 | input$doc_type == 31 | input$doc_type == 32| input$doc_type == 33| input$doc_type == 34| input$doc_type == 35 | input$doc_type == 37 | input$doc_type == 38 | input$doc_type == 39 | input$doc_type == 40 | input$doc_type == 42 | input$doc_type == 43 | input$doc_type == 45 | input$doc_type == 46 | input$doc_type == 47 | input$doc_type == 48 |  input$doc_type == 49 | input$doc_type == 65){
        output$instructions <- renderUI(HTML('<ul><li>Séparer les termes par un "&" pour une recherche multiple</li><li>Cliquer sur un point du graphique pour accéder aux documents dans la bibliothèque numérique correspondante</li></ul>'))
      }
      else{output$instructions <- renderUI("")}
      if(input$gallicloud==T){output$instructions <- renderUI(HTML("<br><br><br><br>"))}
    })})
  })})})})
  
  
  indicator_file <- reactive({
    if(is.null(input$target_upload)) {return(0)}
    else{return(1)}
  })
  output$fileUploaded <- reactive({
    return(indicator_file())
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  observeEvent(input$doc_type,{
    if(input$doc_type == 2 & input$search_mode==1)
    {
      sdewey<-read.csv("sdewey.csv",encoding="UTF-8")
      output$theme<-renderUI({selectizeInput("dewey","Thématique",choices = setNames(sdewey$sdewey,sdewey$sdewey_nom),selected="999",multiple=F)})
    }})
  
  observeEvent(input$doc_type,{observeEvent(input$filtre,{
    
    if(input$doc_type == 3 & input$filtre==1)
    {
      liste_themes<-read.csv("liste_themes.csv",encoding = "UTF-8")
      output$themes_presse<-renderUI({selectizeInput("theme_presse","Thématique",choices = setNames(as.character(liste_themes$num),as.character(liste_themes$titre)))})
      
    }
    if(input$doc_type == 3 & input$filtre==2)
    {
      liste_departements<-read.csv("liste_departements.csv",encoding = "UTF-8")
      output$themes_presse<-renderUI({pickerInput("theme_presse","Presse locale",choices = setNames(as.character(liste_departements$num),as.character(liste_departements$titre)), options = list(`actions-box` = TRUE),multiple = T, selected = 51)})
    }
  })})
  
  observeEvent(input$lemmatiseur, {
    if(exists("lemme_table")==F){lemme_table<-read.csv("morphalou.csv",encoding="UTF-8")}
    lem<-lemme_table$lemme[lemme_table$flexion==input$mot]
    lem=unique(lem)
    flex<-lemme_table$flexion[lemme_table$lemme==lem]
    flex=unique(flex)
    flex<-paste(unlist(flex),collapse = "+")
    updateTextInput(session,"mot","Recherche",value = flex)
  })
  
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
        output$titres<-renderUI({pickerInput("titres","Titre des journaux",choices = setNames(as.character(liste_journaux$ark),as.character(liste_journaux$titre)), options = list(`actions-box` = TRUE),multiple = T,selected = as.character(liste_journaux$ark))})
      }
      else if(as.integer(input$theme_presse[1])>=51)
      {
        departement<-read.csv("liste_departements.csv",encoding = "UTF-8")
        fichier<-as.character(departement$csv[as.character(departement$num)==as.character(input$theme_presse[1])])
        if(length(input$theme_presse)>=2){
          for (h in 1:length(input$theme_presse)) {
            
            fichier<-c(fichier,as.character(departement$csv[as.character(departement$num)==as.character(input$theme_presse[h])]))
          }}
        liste_journaux<-read.csv(fichier[1],encoding="UTF-8")
        if(length(fichier)>=2){
          for (h in 2:length(fichier)) {
            liste_journaux<-bind_rows(liste_journaux,read.csv(fichier[h],encoding="UTF-8"))
          }}
        liste_journaux$titre<-str_remove_all(liste_journaux$titre,"\n")
        output$titres<-renderUI({pickerInput("titres","Titre des journaux",choices = setNames(as.character(liste_journaux$ark),as.character(liste_journaux$titre)), options = list(`actions-box` = TRUE),multiple = T,selected = as.character(liste_journaux$ark))})
      }
    }
    
  })})
  
  
  output$plot <- renderPlotly({Plot(data,input)})
  output$corr<-renderTable(correlation_matrix(prepare_correlation(data),"corr1"),rownames = TRUE)
  output$pvalue=renderText("***p<.001 ; **p<.01 ; *p<.05")
  
  output$legende=renderText(HTML(paste("Source : ","<a href = 'https://gallica.bnf.fr/', target=\'_blank\'> ","gallica.bnf.fr","</a>"),sep = ""))
  if(nrow(data$tableau)==346){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://nytimes.com/', target=\'_blank\'> ","nytimes.com","</a>"),sep = ""))}
  if(nrow(data$tableau)==226){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://huggingface.co/datasets/dell-research-harvard/AmericanStories', target=\'_blank\'> ","American Stories dataset","</a>"),sep = ""))}
  if(nrow(data$tableau)==188){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://lemonde.fr/', target=\'_blank\'> ","lemonde.fr","</a>"),sep = ""))}
  if(nrow(data$tableau)==3264){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.deutsche-digitale-bibliothek.de/', target=\'_blank\'> ","deutsche-digitale-bibliothek.de","</a>"),sep = ""))}
  output$legende0=renderText("Affichage : Gallicagram par B. Azoulay et B. de Courson")
  nb_mots<-length(unique(data[["tableau"]]$mot))
  output$legende2<-renderText(str_c(""))
  output$legende3<-renderText(str_c("Résultats trouvés : ",as.character(sum(data[["tableau"]]$count))))
  output$legende4=renderText("Langue : français")
  output$legende1<-renderText(str_c("Corpus : presse\n"))
  
  recherche_texte<-reactive({input$mot})
  recherche_from<-reactive({input$beginning})
  recherche_to<-reactive({input$end})
  recherche_doc_type<-reactive({input$doc_type})
  recherche_titres<-reactive({input$titres})
  
  observeEvent(input$language,{
    observeEvent(input$bibli,{
      if(input$language == 1 & input$bibli==0){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Gallica-presse 1789-1950 / Le Monde 1945-2022" = 0,"Presse française / Gallica (~1789-1950)" = 1,"Livres / Gallica (~1600-1940)" = 2, "Livres+Presse / Gallica"=56,"Le Monde (1944-2022)"=30,"Le Journal des Débats (1789-1944)"=75,"L'Humanité (1904-1952)"=67,"Le Petit Journal (1863-1942)"=72,"Le Petit Parisien (1876-1944)"=73,"Le Journal de Paris (1777-1827)"=69,"Le Temps (1861-1942)"=71,"Le Figaro (1854-1952)"=66,"Le Moniteur universel (1789-1901)"=70),selected = 1)
      }
      else if(input$language == 1 & input$bibli==1){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse française / Gallica (~1789-1950)" = 1,"Recherche par titre de presse / Gallica" = 3, "Livres / Gallica (~1600-1940)" = 2, 
                                                                      "Corpus personnalisé / Gallica"=4, "Livres+Presse / Gallica"=56,
                                                                      "Le Figaro (1854-1952)"=66,"L'Humanité (1904-1952)"=67,"Le Constitutionnel (1821-1913)"=68,
                                                                      "Le Journal de Paris (1777-1827)"=69,"Le Moniteur universel (1789-1901)"=70,
                                                                      "Le Temps (1861-1942)"=71,"Le Petit Journal (1863-1942)"=72,"Le Petit Parisien (1876-1944)"=73,"Le Journal Officiel (1869-1952)"=74,"Le Journal des Débats (1789-1944)"=75,"La Presse (1836-1930)"=76),selected = 1)
      }
      else if(input$language == 1 & input$bibli==2){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse suisse-romande / Bibliothèque nationale suisse"=15),selected = 15)
        #, "Presse wallonne / KBR"=13, "Presse québécoise / BAnQ"=28
      }
      else if(input$language == 1 & input$bibli==3){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27),selected = 17)
      }
      else if(input$language == 1 & input$bibli==4){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Le Monde (1944-2022)"=30, "Le Figaro (2006-)"=31, "L'Orient-Le Jour (1997-)"=79,"Presse française / MediaCloud"=50),selected = 30)#Le Marin 55
      }
      else if(input$language == 1 & input$bibli==5){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Isidore"=36,"Cairn.info"=32,"Theses.fr"=33,"Persée"=34),selected = 36)
      }
      else if(input$language == 1 & input$bibli==7){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("MusixMatch / Français"=45),selected = 45)
      }
      if(input$language == 1 & input$bibli==8){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Livres / Ngram Viewer - Google Books" = 5, "Google Trends / France"=44),selected = 5)
      }
      if(input$language == 1 & input$bibli==9){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Opensubtitles / Films en français"=77, "Opensubtitles / Films en anglais"=78),selected = 78)
      }
      else if(input$language == 2){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse allemande / Deutsche digitale bibliothek" = 43,"Presse allemande / Europeana" = 6, "Presse austro-hongroise / ANNO"=29, "Presse suisse-allemande / Bibliothèque nationale suisse"=16 ,"Presse germanophone / MediaCloud"=52, "Livres / Ngram Viewer Allemand" = 9,"MusixMatch / Allemand"=47,"Google Trends / Allemagne"=62,"Google Trends / Autriche"=63),selected = 43)
      }else if(input$language == 3){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse néerlandaise / Europeana" = 7,"Presse flamande / KBR"=14, "Presse néerlandophone / MediaCloud"=53, "MusixMatch / Néerlandais"=48,"Google Trends / Pays-Bas"=64),selected = 7)
      }else if(input$language == 4){
        #updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse britannique / BNA" = 8,"Presse australienne / Trove"=35,"Presse américaine / newspapers.com"=37,"Presse canadienne / newspapers.com"=38,"Presse britannique / newspapers.com"=39,"Presse australienne / newspapers.com"=40,"Presse américaine / Library of Congress"=42, "Livres / Ngram Viewer Anglais" = 10),selected = 8)
        ## readd "The New York Times"=65, when it works again
        updateSelectInput(session,"doc_type", "Corpus",choices = list("American Stories (1798-1963)"=99,"Presse britannique / BNA" = 8,"Presse australienne / Trove"=35,"Presse américaine / Library of Congress"=42, "Presse anglophone / MediaCloud"=51, "Livres / Ngram Viewer Anglais" = 10,"MusixMatch / Anglais"=46,"Google Trends / Grande-Bretagne"=58,"Google Trends / Etats-Unis"=59,"Google Trends / Australie"=60),selected = 99)
      }else if(input$language == 5){
        updateSelectInput(session,"doc_type", "Corpus",choices = list("Presse espagnole / BNE"=11, "Presse hispanophone / MediaCloud"=54, "Livres / Ngram Viewer Espagnol"=12,"MusixMatch / Espagnol"=49,"Google Trends / Espagne"=61),selected = 11)
      }
    })})
  
  
  observeEvent(input$doc_type,{
    if(input$doc_type == 99){
      updateSelectInput(session,"search_mode",choices = list("Par n-gramme"=3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if(input$doc_type == 2 | input$doc_type == 56){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1,"Par n-gramme"=3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if( input$doc_type == 1 ){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1,"Par n-gramme"=3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Mois",inline = T)
    }
    if( input$doc_type == 30 | input$doc_type == 66 | input$doc_type == 67 | input$doc_type == 68 | input$doc_type == 69 | input$doc_type == 70 | input$doc_type == 71 ){
      updateSelectInput(session,"search_mode",choices = list("Par n-gramme" = 3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois","Semaine"),selected = "Année",inline = T)
    }
    if( input$doc_type == 77 | input$doc_type == 78 ){
      updateSelectInput(session,"search_mode",choices = list("Par n-gramme" = 3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if( input$doc_type == 0 ){
      updateSelectInput(session,"search_mode",choices = list("Par n-gramme" = 3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if(input$doc_type == 32 | input$doc_type == 33 | input$doc_type == 34 | input$doc_type == 36){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1),selected = 1)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if(input$doc_type == 4){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1),selected = 1)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if( input$doc_type == 6 | input$doc_type == 7 | input$doc_type == 18 | input$doc_type == 19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27 | input$doc_type == 28 | input$doc_type == 29 | input$doc_type == 45 | input$doc_type == 46 | input$doc_type == 47 | input$doc_type == 48 | input$doc_type == 49){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1),selected = 1)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if(input$doc_type == 43){
      updateSelectInput(session,"search_mode",choices = list("Par document" = 1,"Par ngramme" = 3, "Par API" = 4),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Mois",inline = T)
    }
    if(input$doc_type == 5 | input$doc_type == 9 | input$doc_type == 10 | input$doc_type == 12){
      updateSelectInput(session,"search_mode",choices = list("Par n-gramme" = 3),selected = 3)
      updateRadioButtons(session,"resolution",choices = c("Année"),selected = "Année",inline = T)
    }
    if(input$doc_type == 44 | input$doc_type == 58 | input$doc_type == 59 | input$doc_type == 60 | input$doc_type == 61 | input$doc_type == 62 | input$doc_type == 63 | input$doc_type == 64){
      updateSelectInput(session,"search_mode",choices = list("gtrends" = 5),selected = 5)
      updateRadioButtons(session,"resolution",choices = c("Mois"),selected = "Mois",inline = T)
    }
    if(input$doc_type == 11 | input$doc_type == 13 | input$doc_type == 14 | input$doc_type == 17 | input$doc_type == 37 | input$doc_type == 38 | input$doc_type == 39 | input$doc_type == 40 | input$doc_type == 42 | input$doc_type == 55){
      updateSelectInput(session,"search_mode",choices = list("Par page" = 2),selected = 2)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if(input$doc_type == 8 | input$doc_type == 15 | input$doc_type == 16| input$doc_type == 35| input$doc_type == 57| input$doc_type == 65 | input$doc_type == 79){
      updateSelectInput(session,"search_mode",choices = list("Par article" = 4),selected = 4)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
    }
    if(input$doc_type == 31 | input$doc_type == 50 | input$doc_type == 51 | input$doc_type == 52 | input$doc_type == 53 | input$doc_type == 54){
      updateSelectInput(session,"search_mode",choices = list("Par article" = 4),selected = 4)
      updateRadioButtons(session,"resolution",choices = c("Année","Mois","Semaine"),selected = "Semaine",inline = T)
    }
    if(input$doc_type == 3){
      observeEvent(input$theme_presse,{observeEvent(input$titres,{
        if(input$theme_presse == 1 & length(input$titres)<=15){
          updateSelectInput(session,"search_mode",choices = list("Par document" = 1),selected = 1)
          updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
        }
        else{
          updateSelectInput(session,"search_mode",choices = list("Par document" = 1),selected = 1)
          updateRadioButtons(session,"resolution",choices = c("Année","Mois"),selected = "Année",inline = T)
          
        }
      })})}
  })
  
  
  observeEvent(
    input$search_mode,{
      if(input$search_mode==2 & (input$doc_type ==1 |input$doc_type ==2 |input$doc_type ==4)){
        output$avertissement<-renderText(message(recherche_texte(),recherche_from(),recherche_to(),recherche_doc_type(),recherche_titres()))
      }else if (input$search_mode==2 & input$doc_type == 3){
        if (length(input$titres)<=15 & input$theme_presse==1){
          output$avertissement<-renderText(message(recherche_texte(),recherche_from(),recherche_to(),recherche_doc_type(),recherche_titres()))
        }
      }
      
    })
  table_corpus<-read.csv("corpus.csv",fileEncoding = "UTF-8",sep=",")
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_",input$mot,"_",input$beginning,"_",input$end,"_",table_corpus$Corpus[input$doc_type==table_corpus$ID],'.csv', sep='')
    },
    content = function(con) {
      write.csv(data$tableau, con,row.names = F,fileEncoding = "UTF-8")
    })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$mot,"_",input$beginning,"_",input$end,"_",table_corpus$Corpus[input$doc_type==table_corpus$ID],'.html', sep='')
    },
    content = function(con) {
      htmlwidgets::saveWidget(as_widget(Plot(data,input)), con)
    })
  output$downloadSPlot <- downloadHandler(
    filename = function() {
      paste(input$mot,"_",input$beginning,"_",input$end,"_",table_corpus$Corpus[input$doc_type==table_corpus$ID],'.png', sep='')
    },
    content = function(filename) {
      save_plot(filename,SPlot(data,input))
    })
  output$data_session <- downloadHandler(
    filename = function() {
      paste('data-session_', Sys.Date(),'.csv', sep='')
    },
    content = function(con) {
      write.csv(memoire, con,row.names = F,fileEncoding = "UTF-8")
    })
  
  corpus_table<-function(bloblo){
    blabla<-as.data.frame(unique(bloblo[,2]))
    colnames(blabla)<-c("Categorie")
    blabla$Total<-0
    for (i in 1:length(blabla$Total)) {
      blabla$Total[i]<-sum(bloblo[,3][bloblo[,2]==blabla$Categorie[i]])
    }
    blabla$Part<-blabla$Total/sum(blabla$Total)
    blabla$Part<-str_c(round(blabla$Part*100,digits = 2),"%")
    blabla<-blabla[order(blabla$Total,decreasing = T),]
    blabla$Total<-as.integer(blabla$Total)
    output$total_table<-renderTable({blabla})
  }
  corpus_table_bis<-function(bloblo){
    blabla<-as.data.frame(unique(bloblo[,2]))
    colnames(blabla)<-c("Categorie")
    blabla$Total<-0
    for (i in 1:length(blabla$Total)) {
      blabla$Total[i]<-sum(bloblo[,3][bloblo[,2]==blabla$Categorie[i]])
    }
    blabla$Part<-blabla$Total/sum(blabla$Total)
    blabla$Part<-str_c(round(blabla$Part*100,digits = 2),"%")
    blabla<-blabla[order(blabla$Total,decreasing = T),]
    blabla$Total<-as.integer(blabla$Total)
    output$total_table_bis<-renderTable({blabla})
  }
  observeEvent(input$topButton,{
    output$top_presse<-renderPlotly({gallicapresse(input)})
    })
  
  observeEvent(input$cartoButton,{
    fra<<-cartographie(input)
    iscarto<<-T
    # output$carto<-renderLeaflet({cartoPlot(input,fra)})
    cartog<-cartoPicture(fra,input$cartoMot,min(input$cartoRange),max(input$cartoRange),input$colorscale)
    car<-cartoGramme(fra,input$cartoMot,min(input$cartoRange),max(input$cartoRange),input$colorscale)
    
    output$carto2<-renderPlotly({ggplotly(car)})
    
    output$cartogramme<-downloadHandler(
      filename = function() {
        paste('cartogramme_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
      },
      content = function(filename) {
        save_plot(filename,car)
      })
    # output$downloadCarto <- downloadHandler(
    #   filename = function() {
    #     paste('carte_', min(input$cartoRange),"_",max(input$cartoRange),"_",input$cartoMot, '.html', sep='')
    #   },
    #   content = function(con) {
    #     htmlwidgets::saveWidget(as_widget(cartoPlot(input,fra)), con)
    #   })
    # output$cartoPng <- downloadHandler(
    #   filename = function() {
    #     paste('carto_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
    #   },
    #   content = function(filename) {
    #     save_plot(filename,cartog)
    #   })
    
  })
  
  iscarto<<-F
  fru <- readRDS("gadm36_FRA_2_sf.rds")
  fri<-read.csv("cartoInit.csv",fileEncoding = "UTF-8",sep=",")
  fru$val=fri$val
  fru$count=fri$count
  fru$base=fri$base
  #output$carto<-renderLeaflet({cartoPlot(input,fru)})
  #output$carto2<-renderPlotly({ggplotly(cartoGramme(fru,"Général Boulanger","1885-01-01","1890-01-01",F))})
  #output$top_presse<-renderPlotly({gallicapresse(input)})
  
  observeEvent(input$colorscale,{
    if(iscarto==F){
      output$carto2<-renderPlotly({ggplotly(cartoGramme(fru,"Général Boulanger","1885-01-01","1890-01-01",input$colorscale))})
      output$cartogramme<-downloadHandler(
        filename = function() {
          paste('cartogramme_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
        },
        content = function(filename) {
          save_plot(filename,cartoGramme(fru,"Général Boulanger","1885-01-01","1890-01-01",input$colorscale))
        })
      output$cartoPng <- downloadHandler(
        filename = function() {
          paste('carto_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
        },
        content = function(filename) {
          save_plot(filename,cartoPicture(fru,"Général Boulanger","1885-01-01","1890-01-01",input$colorscale))
        })
    }
    else{
      car<-cartoGramme(fra,input$cartoMot,min(input$cartoRange),max(input$cartoRange),input$colorscale)
      
      output$carto2<-renderPlotly({ggplotly(car)})
      
      output$cartogramme<-downloadHandler(
        filename = function() {
          paste('cartogramme_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
        },
        content = function(filename) {
          save_plot(filename,car)
        })
      output$cartoPng <- downloadHandler(
        filename = function() {
          paste('carto_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
        },
        content = function(filename) {
          save_plot(filename,cartog)
        })
    }
    
  })
  
  output$cartogramme<-downloadHandler(
    filename = function() {
      paste('cartogramme_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
    },
    content = function(filename) {
      save_plot(filename,cartoGramme(fru,"Général Boulanger","1885-01-01","1890-01-01",F))
    })
  output$downloadCarto <- downloadHandler(
    filename = function() {
      paste('carte_', min(input$dateRange),"_",max(input$dateRange),"_",input$cartoMot, '.html', sep='')
    },
    content = function(con) {
      htmlwidgets::saveWidget(as_widget(cartoPlot(input,fru)), con)
    })
  output$cartoPng <- downloadHandler(
    filename = function() {
      paste('carto_',input$cartoMot,"_",min(input$cartoRange),"_",max(input$cartoRange),'.png', sep='')
    },
    content = function(filename) {
      save_plot(filename,cartoPicture(fru,"Général Boulanger","1885-01-01","1890-01-01",F))
    })
  
  observeEvent(input$do,{
    print(input$mot)
    if(counter==0){
      memoire<<-memoire[0,]
      counter<<-1
    }
    
    
    if ((input$doc_type==1 & input$search_mode==1) |(input$doc_type == 3 & input$search_mode==1) | input$doc_type==5 | (input$doc_type==2 & input$search_mode==1) | input$doc_type==6 | input$doc_type==7 | input$doc_type==8 | input$doc_type == 9 | input$doc_type == 10 | 
        input$doc_type == 11 | input$doc_type == 12 | input$doc_type == 13 | input$doc_type == 14 | input$doc_type == 15 | input$doc_type == 16 | input$doc_type == 17 | input$doc_type == 18 | input$doc_type == 19 | input$doc_type == 20 | 
        input$doc_type == 21 | input$doc_type == 22 | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27 | input$doc_type == 28 | input$doc_type == 29 | 
        input$doc_type == 32 | input$doc_type == 33 | input$doc_type == 34 | input$doc_type == 35 | input$doc_type == 36 | input$doc_type == 37 | input$doc_type == 38 | input$doc_type == 39 | input$doc_type == 40 | 
        input$doc_type == 42 | (input$doc_type == 43 & input$search_mode %in% c(1,4)) | input$doc_type == 44 | ((input$doc_type==31)&(input$resolution=="Mois"|input$resolution=="Année") ) |
        input$doc_type == 45 | input$doc_type == 46 | input$doc_type == 47 | input$doc_type == 48  | input$doc_type == 49 |
        input$doc_type == 50 | input$doc_type == 51 | input$doc_type == 52 | input$doc_type == 53  | input$doc_type == 54 |
        input$doc_type == 55 | (input$doc_type == 56 & input$search_mode==1)| input$doc_type == 57 | input$doc_type == 58 |
        input$doc_type == 59 | input$doc_type == 60 | input$doc_type == 61 | input$doc_type == 62 | input$doc_type == 63 | input$doc_type == 64| 
        input$doc_type == 65 | input$doc_type == 79){
      df = get_data(input$mot,input$beginning,input$end,input$resolution,input$doc_type,input$titres,input,input$cooccurrences,input$prox)}
    else if(input$doc_type==4){
      inFile<-input$target_upload
      tot_df<- read.csv(inFile$datapath, header = TRUE,sep = ";",encoding = "UTF-8")
      colnames(tot_df)<-str_remove_all(colnames(tot_df),"'")
      colnames(tot_df)<-str_remove_all(colnames(tot_df),"\\.")
      colnames(tot_df)<-str_remove_all(colnames(tot_df)," ")
      colnames(tot_df)<-str_remove_all(colnames(tot_df),"XUFEFF")
      colnames(tot_df)<-iconv(colnames(tot_df),from="UTF-8",to="ASCII//TRANSLIT")
      tot_df<-select(tot_df,URL,TITRE,TYPE,AUTEURS,CONTRIBUTEURS,EDITEURS,DATES,DESCRIPTIONS,NOMBREDEVUES,PROVENANCE,DROITS,ARKCATALOGUE)
      colnames(tot_df)<-c("identifier","title","type","creator","contributor","publisher","date","description","format","source","rights","relation")
      tot_df$identifier<-str_remove_all(tot_df$identifier," .+")
      df=page_search(input$mot,input$beginning,input$end,input$resolution,tot_df,input$doc_type,input$search_mode,input$titres)
    }
    else if(input$search_mode==3){
      print("here")
      agregator=0
      if(input$gallicloud==T){
        cloudplot=cloudify(input)
      }
      else{
      gallicagram=0
      nouvrequette=NA
      if(input$joker==T){
        jokertable<-jokerize(input)
        l<-as.character(jokertable$gram)
        m=l[1]
        mm=l[2]
        if(is.na(mm)==F){
          for (h in 2:length(l)) {
            m<-str_c(m,"&",l[h])
          }}
        nouvrequette=m
        df=ngramize(input,nouvrequette,gallicagram,agregator)
      }
      if(input$doc_type == 43 & input$search_mode==3){
        gallicagram=0
        nouvrequette=NA
        agregator=0
        df=ngramize(input,nouvrequette,gallicagram,agregator)
      }
      else if(input$joker==F & input$doc_type!=0 & input$doc_type!=56){df=ngramize(input,nouvrequette,gallicagram,agregator)}
      else if(input$joker==F & input$doc_type==0){
        df = list()
        for(i in 1:2){
          df[[i]] = ngramize(input,nouvrequette,i,agregator)
          z = df[[i]][["tableau"]]$date < 1951
        }
        zz = df[[2]][["tableau"]]$date>1945
        df[[1]][["tableau"]] = bind_rows(df[[1]][["tableau"]],df[[2]][["tableau"]][zz,])
        df = df[[1]]
        rrr<-df[["tableau"]]
        rrr<-rrr[rrr$date>=input$beginning,]
        df[["tableau"]]<-rrr
        memoire<<-bind_rows(df[["tableau"]],memoire)
      }
      else if(input$joker==F & input$doc_type==56){
        agregator = 1
        df=ngramize(input,nouvrequette,gallicagram,agregator)
        agregator = 2
        dfbis=ngramize(input,nouvrequette,gallicagram,agregator)
        df[["tableau"]]$count[is.na(df[["tableau"]]$count)]=0
        dfbis[["tableau"]]$count[is.na(dfbis[["tableau"]]$count)]=0
        df[["tableau"]]$base[is.na(df[["tableau"]]$base)]=0
        dfbis[["tableau"]]$base[is.na(dfbis[["tableau"]]$base)]=0
        
        df[["tableau"]]$count=df[["tableau"]]$count+dfbis[["tableau"]]$count
        df[["tableau"]]$base=df[["tableau"]]$base+dfbis[["tableau"]]$base
        df[["tableau"]]$ratio=df[["tableau"]]$count/df[["tableau"]]$base
        df[["tableau"]]$ratio[is.na(df[["tableau"]]$ratio)]=0
      }
      }
    }
    else if((input$doc_type==31)&input$resolution=="Semaine"){
      df=contempo(input)
    }
    
    if(input$gallicloud==F){output$plot <- renderPlotly({Plot(df,input)})}
    if(input$gallicloud==T & input$search_mode==3){output$cloud<-renderPlot({cloudplot})}
    
    if((input$doc_type==1 & input$search_mode==1) | (input$doc_type==2 & input$search_mode==1) | (input$doc_type == 3 & input$search_mode==1) | (input$doc_type==56 & input$search_mode==1) | input$doc_type==6 | input$doc_type==7 | input$doc_type == 18 | input$doc_type == 19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27  | input$doc_type == 28 | input$doc_type == 29 | input$doc_type == 32| input$doc_type == 33| input$doc_type == 34| input$doc_type == 36| (input$doc_type == 43 & input$search_mode==1)| input$doc_type == 45| input$doc_type == 46| input$doc_type == 47| input$doc_type == 48| input$doc_type == 49){
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2<-renderText(str_c("Documents épluchés : ",as.character(sum(df[["tableau"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Résultats trouvés : ", as.character(sum(df[["tableau"]]$count))))
    }
    else if (input$doc_type==5 | input$doc_type==9 | input$doc_type==10 | input$doc_type==12| input$doc_type==44| input$doc_type==58| input$doc_type==59| input$doc_type==60| input$doc_type==61| input$doc_type==62| input$doc_type==63| input$doc_type==64){
      output$legende2<-NULL
      output$legende3<-NULL
    }
    else if(input$search_mode==3){
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2<-NULL
      output$legende3<-renderText(str_c("Nombre d'occurrences trouvées : ", as.character(sum(df[["tableau"]]$count))))
    }
    else if(input$doc_type==4 & input$search_mode==1){
      nb_mots<-length(unique(df[["tableau_volume"]]$mot))
      output$legende2<-renderText(str_c("Documents épluchées : ", as.character(sum(df[["tableau_volume"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Résultats trouvés : ", as.character(sum(df[["tableau_volume"]]$count))))
    }
    else if(input$doc_type==65){
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2 <- renderText(str_c("Documents épluchées : ", as.character(as.integer(sum(df[["tableau"]]$base,na.rm=T)/nb_mots))))
      output$legende3<-renderText(str_c("Résultats trouvés : ", as.character(sum(df[["tableau"]]$count,na.rm=T))))
    }
    else if (input$doc_type==11 | input$doc_type==13 | input$doc_type==14 | input$doc_type==17 | input$doc_type == 37 | input$doc_type == 38 | input$doc_type == 39 | input$doc_type == 40| input$doc_type == 42| input$doc_type == 55) {
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2<-renderText(str_c("Pages épluchées : ", as.character(sum(df[["tableau"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Pages correspondant à la recherche : ", as.character(sum(df[["tableau"]]$count))))
    }
    else if (input$doc_type==8 | input$doc_type==15 | input$doc_type==16 | input$doc_type==31| input$doc_type == 35 | input$doc_type == 50 | input$doc_type == 51 | input$doc_type == 52 | input$doc_type == 53 | input$doc_type == 54| input$doc_type == 57| input$doc_type == 65 | input$doc_type == 79) {
      nb_mots<-length(unique(df[["tableau"]]$mot))
      output$legende2<-renderText(str_c("Articles épluchés : ", as.character(sum(df[["tableau"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Articles correspondant à la recherche : ", as.character(sum(df[["tableau"]]$count))))
    }
    
    else {
      nb_mots<-length(unique(df[["tableau_page"]]$mot))
      output$legende2<-renderText(str_c("Pages épluchées : ", as.character(sum(df[["tableau_page"]]$base)/nb_mots)))
      output$legende3<-renderText(str_c("Pages correspondant à la recherche : ", as.character(sum(df[["tableau_page"]]$count))))
    }
    
    if(input$doc_type==1 | input$doc_type==2 | (input$doc_type == 3 & input$theme_presse==1) | input$doc_type==4 | input$doc_type==56 | input$doc_type %in% 66:76){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://gallica.bnf.fr/', target=\'_blank\'> ","gallica.bnf.fr","</a>"),sep = ""))}
    if(input$doc_type==5 | input$doc_type==9 | input$doc_type==10 | input$doc_type==12){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://books.google.com/ngrams/', target=\'_blank\'> ","books.google.com/ngrams","</a>"),sep = ""))}
    if(input$doc_type==6 | input$doc_type==7){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.europeana.eu/', target=\'_blank\'> ","europeana.eu","</a>"),sep = ""))}
    if(input$doc_type==8){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.britishnewspaperarchive.co.uk/', target=\'_blank\'> ","britishnewspaperarchive.co.uk","</a>"),sep = ""))}
    if(input$doc_type==11){output$legende=renderText(HTML(paste("Source : ","<a href = 'http://www.hemerotecadigital.bne.es/', target=\'_blank\'> ","hemerotecadigital.bne.es","</a>"),sep = ""))}
    if(input$doc_type==13 | input$doc_type==14){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.belgicapress.be/', target=\'_blank\'> ","belgicapress.be","</a>"),sep = ""))}
    if(input$doc_type==15 | input$doc_type==16){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.e-newspaperarchives.ch/', target=\'_blank\'> ","e-newspaperarchives.ch","</a>"),sep = ""))}
    if(input$doc_type==17){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.lectura.plus/Presse/', target=\'_blank\'> ","lectura.plus/Presse","</a>"),sep = ""))}
    if(input$doc_type==18){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://kiosque.limedia.fr/', target=\'_blank\'> ","kiosque.limedia.fr","</a>"),sep = ""))}
    if(input$doc_type==19){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://memonum-mediatheques.montpellier3m.fr/', target=\'_blank\'> ","memonum-mediatheques.montpellier3m.fr","</a>"),sep = ""))}
    if(input$doc_type==20){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.communpatrimoine.fr/', target=\'_blank\'> ","communpatrimoine.fr","</a>"),sep = ""))}
    if(input$doc_type==21){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://yroise.biblio.brest.fr/', target=\'_blank\'> ","yroise.biblio.brest.fr","</a>"),sep = ""))}
    if(input$doc_type==22){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.pireneas.fr/', target=\'_blank\'> ","pireneas.fr","</a>"),sep = ""))}
    if(input$doc_type==23){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://rosalis.bibliotheque.toulouse.fr/', target=\'_blank\'> ","rosalis.bibliotheque.toulouse.fr","</a>"),sep = ""))}
    if(input$doc_type==24){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://bibliotheque-numerique.diplomatie.gouv.fr/', target=\'_blank\'> ","bibliotheque-numerique.diplomatie.gouv.fr","</a>"),sep = ""))}
    if(input$doc_type==25){output$legende=renderText(HTML(paste("Source : ","<a href = 'http://www.rfnum-bibliotheque.org/', target=\'_blank\'> ","rfnum-bibliotheque.org","</a>"),sep = ""))}
    if(input$doc_type==26){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.numistral.fr/', target=\'_blank\'> ","numistral.fr","</a>"),sep = ""))}
    if(input$doc_type==27){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.bn-r.fr/', target=\'_blank\'> ","bn-r.fr","</a>"),sep = ""))}
    if(input$doc_type==28){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.banq.qc.ca/', target=\'_blank\'> ","banq.qc.ca","</a>"),sep = ""))}
    if(input$doc_type == 3 & input$theme_presse != 1){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://gallica.bnf.fr/html/und/presse-et-revues/presse-et-revues', target=\'_blank\'> ","gallica.bnf.fr","</a>"),sep = ""))}
    if(input$doc_type==29){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://anno.onb.ac.at/', target=\'_blank\'> ","anno.onb.ac.at","</a>"),sep = ""))}
    if(input$doc_type==31){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.lefigaro.fr/', target=\'_blank\'> ","lefigaro.fr","</a>"),sep = ""))}
    if(input$doc_type==30){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.lemonde.fr/', target=\'_blank\'> ","lemonde.fr","</a>"),sep = ""))}
    if(input$doc_type==32){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.cairn.info/', target=\'_blank\'> ","cairn.info","</a>"),sep = ""))}
    if(input$doc_type==33){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.theses.fr/', target=\'_blank\'> ","theses.fr","</a>"),sep = ""))}
    if(input$doc_type==34){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.persee.fr/', target=\'_blank\'> ","persee.fr","</a>"),sep = ""))}
    if(input$doc_type==35){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://trove.nla.gov.au/', target=\'_blank\'> ","trove.nla.gov.au","</a>"),sep = ""))}
    if(input$doc_type==36){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://isidore.science/', target=\'_blank\'> ","isidore.science","</a>"),sep = ""))}
    if(input$doc_type == 37 | input$doc_type == 38 | input$doc_type == 39 | input$doc_type == 40){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://ww.newspapers.com', target=\'_blank\'> ","newspapers.com","</a>"),sep = ""))}
    if(input$doc_type==42){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://chroniclingamerica.loc.gov/', target=\'_blank\'> ","chroniclingamerica.loc.gov","</a>"),sep = ""))}
    if(input$doc_type==43){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.deutsche-digitale-bibliothek.de/', target=\'_blank\'> ","deutsche-digitale-bibliothek.de","</a>"),sep = ""))}
    if(input$doc_type==0){output$legende=renderText("gallica.bnf.fr et lemonde.fr")}
    if(input$doc_type==44){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://trends.google.fr//', target=\'_blank\'> ","trends.google.fr","</a>"),sep = ""))}
    if(input$doc_type == 45 | input$doc_type == 46 | input$doc_type == 47 | input$doc_type == 48  | input$doc_type == 49){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.musixmatch.com/', target=\'_blank\'> ","musixmatch.com","</a>"),sep = ""))}
    if(input$doc_type == 50 | input$doc_type == 51 | input$doc_type == 52 | input$doc_type == 53  | input$doc_type == 54){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://mediacloud.org/', target=\'_blank\'> ","mediacloud.org","</a>"),sep = ""))}
    if(input$doc_type==55){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://lemarin.ouest-france.fr/', target=\'_blank\'> ","lemarin.ouest-france.fr","</a>"),sep = ""))}
    if(input$doc_type==57){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://eluxemburgensia.lu/', target=\'_blank\'> ","eluxemburgensia.fr","</a>"),sep = ""))}
    if(input$doc_type==58 | input$doc_type==59 | input$doc_type==60 | input$doc_type==61 | input$doc_type==62 | input$doc_type==63 | input$doc_type==64){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://trends.google.fr//', target=\'_blank\'> ","trends.google.fr","</a>"),sep = ""))}
    if(input$doc_type==65){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.nytimes.com/', target=\'_blank\'> ","nytimes.com","</a>"),sep = ""))}
    if(input$doc_type==77 | input$doc_type==78){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.opensubtitles.org/', target=\'_blank\'> ","opensubtitles.org","</a>"),sep = ""))}
    if(input$doc_type==99){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://huggingface.co/datasets/dell-research-harvard/AmericanStories', target=\'_blank\'> ","https://huggingface.co/datasets/dell-research-harvard/AmericanStories","</a>"),sep = ""))}
    if(input$doc_type==79){output$legende=renderText(HTML(paste("Source : ","<a href = 'https://www.lorientlejour.com/', target=\'_blank\'> ","lorientlejour.com","</a>"),sep = ""))}
    
    
    if(input$doc_type==0 | input$doc_type==1 | input$doc_type==2 | input$doc_type == 3 | input$doc_type==4 | input$doc_type==5 | input$doc_type==13 | input$doc_type==15 | input$doc_type==17 | input$doc_type==18 | input$doc_type==19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26 | input$doc_type == 27 | input$doc_type == 28 | input$doc_type == 30 | input$doc_type == 31 | input$doc_type == 32| input$doc_type == 33| input$doc_type == 34| input$doc_type == 36| input$doc_type == 44| input$doc_type == 45| input$doc_type == 50| input$doc_type == 55 | input$doc_type==56| input$doc_type==57 | input$doc_type %in% 66:76 | input$doc_type==79){output$legende4=renderText("Langue : français")}
    if(input$doc_type==6 | input$doc_type==9 | input$doc_type==16 |input$doc_type==29|input$doc_type==43|input$doc_type==47| input$doc_type == 52 | input$doc_type == 62 | input$doc_type == 63){output$legende4=renderText("Langue : allemand")}
    if(input$doc_type==7 | input$doc_type==14|input$doc_type==48| input$doc_type == 53| input$doc_type == 64){output$legende4=renderText("Langue : néerlandais")}
    if(input$doc_type==8 | input$doc_type==10| input$doc_type==35 | input$doc_type == 37 | input$doc_type == 38 | input$doc_type == 39 | input$doc_type == 40|input$doc_type==42|input$doc_type==46| input$doc_type == 51 | input$doc_type == 58 | input$doc_type == 59 | input$doc_type == 60 | input$doc_type == 65){output$legende4=renderText("Langue : anglais")}
    if(input$doc_type==11 | input$doc_type==12|input$doc_type==49| input$doc_type == 54 | input$doc_type == 61){output$legende4=renderText("Langue : espagnol")}
    
    
    
    
    if(input$doc_type==0 | input$doc_type==1 | input$doc_type==6 | input$doc_type==7 | input$doc_type==8 | input$doc_type==11 | input$doc_type==13 | input$doc_type==14 | input$doc_type==15 | input$doc_type==16 | input$doc_type==17 | input$doc_type==18 | input$doc_type==19 | input$doc_type == 20 | input$doc_type == 21 | input$doc_type == 22  | input$doc_type == 23 | input$doc_type == 24 | input$doc_type == 25 | input$doc_type == 26  | input$doc_type == 27 | input$doc_type == 28 | input$doc_type == 29 | input$doc_type == 30 | input$doc_type == 31| input$doc_type==35 | input$doc_type == 37 | input$doc_type == 38 | input$doc_type == 39 | input$doc_type == 40 | input$doc_type == 42 | input$doc_type == 43 | input$doc_type == 50 | input$doc_type == 51 | input$doc_type == 52 | input$doc_type == 53 | input$doc_type == 54 | input$doc_type == 55| input$doc_type==57  | input$doc_type == 65 | input$doc_type %in% 66:76 | input$doc_type == 79 ){output$legende1<-renderText("Corpus : presse")}
    if(input$doc_type==2 | input$doc_type==5 | input$doc_type==9 | input$doc_type==10 | input$doc_type==12){output$legende1<-renderText("Corpus : livres")}
    if(input$doc_type==4){output$legende1<-renderText("Corpus : personnalisé")}
    if(input$doc_type==56){output$legende1<-renderText("Corpus : presse et livres")}
    if(input$doc_type==32| input$doc_type == 33| input$doc_type == 34| input$doc_type == 36){output$legende1<-renderText("Corpus : scientifique")}
    if(input$doc_type==44 | input$doc_type==58 | input$doc_type==59 | input$doc_type==60 | input$doc_type==61 | input$doc_type==62 | input$doc_type==63 | input$doc_type==64){output$legende1<-renderText("Corpus : web")}
    if(input$doc_type == 45 | input$doc_type == 46 | input$doc_type == 47 | input$doc_type == 48  | input$doc_type == 49){output$legende1<-renderText("Corpus : paroles de chansons")}
    if(input$doc_type == 77 | input$doc_type == 78){output$legende1<-renderText("Corpus : films")}
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
    
    
    recherche_precedente<<-str_c(input$mot,"_",input$beginning,"_",input$end,"_",input$resolution)
    corpus_precedent<<-str_c(input$doc_type,"_",input$search_mode)
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if(str_length(input$mot)<=30){paste("data_",input$mot,"_",input$beginning,"_",input$end,table_corpus$Corpus[input$doc_type==table_corpus$ID],'.csv', sep='')}
        else(paste("data.csv"))
      },
      content = function(con) {
        if((input$doc_type==2 | input$doc_type == 3 | input$doc_type==4) & input$search_mode==2){write.csv(df$tableau_page, con,row.names = F,fileEncoding = "UTF-8")}
        else if(input$search_mode==1 & input$doc_type==4){write.csv(df$tableau_volume, con,row.names = F,fileEncoding = "UTF-8")}
        else{write.csv(df$tableau, con,row.names = F,fileEncoding = "UTF-8")}
      })
    output$downloadPlot <- downloadHandler(
      filename = function() {
        if(str_length(input$mot)<=30){paste(input$mot,"_",input$beginning,"_",input$end,"_",table_corpus$Corpus[input$doc_type==table_corpus$ID],'.html', sep='')}
        else{paste("plot.html")}
      },
      content = function(con) {
        htmlwidgets::saveWidget(as_widget(Plot(df,input)), con)
      })
    output$downloadSPlot <- downloadHandler(
      filename = function() {
        if(str_length(input$mot)<=30){paste(input$mot,"_",input$beginning,"_",input$end,"_",table_corpus$Corpus[input$doc_type==table_corpus$ID],'.png', sep='')}
        else{paste("plot.png")}
      },
      content = function(filename) {
        save_plot(filename,SPlot(df,input))
      })
    temp<<-df
    updateCheckboxInput(session,"correlation_test",value=FALSE)
    
    
    if(isolate(input$contextualisation)==T & (isolate(input$doc_type==1) | isolate(input$doc_type==2) | isolate(input$doc_type==56)| isolate(input$doc_type %in% 66:76))){
      will<<-""
      b=willisation(input,will)
      if(is.null(b)==F){
        wurl=str_replace(wurl,"https://gallica-grapher.ew.r.appspot.com/api/gallicaRecords","https://www.gallicagrapher.com/context")
        wurl=str_remove_all(wurl,"&row_split=true&cursor=0")
        output$lien=renderUI(HTML(str_c("<b><font size=\"5\">Contexte</font><br>Crédit : Will Gleason avec <a href='","https://www.gallicagrapher.com/","' target='_blank'>","Gallicagrapher","</a></b></font>","<br><a href='",will,"' target='_blank'>","Ouvrir la recherche dans Gallica","</a>",
                                        "<br><a href='",wurl,"' target='_blank'>","Plus de contexte","</a>")))
        require("DT")
        output$frame<-renderDataTable(b,escape = F,options = list(pageLength = 10, lengthChange = FALSE, columnDefs = list(list(className = 'dt-body-right', targets = 3))))
      }
      hide_spinner(spin_id = "contexte")}
  })
  
  ###
  output$notice_corp<-renderDataTable(read.csv("corpus.csv",sep=",",encoding = "UTF-8"),options = list(pageLength = 100,lengthChange = FALSE))
  ###
  observeEvent(input$correlation_test,{
  if(input$correlation_test==TRUE){
    if(str_detect(input$mot,".+&.+"))
    {output$corr<-renderTable(correlation_matrix(prepare_correlation(temp),"corr1"),rownames = TRUE)}
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
  }
  })
  ###
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  session$onSessionEnded(function() {
    
    stopApp()})
  invalid<<-0
  observe({
    invalidateLater(3600000, session)
    if(invalid==0){invalid<<-1}
    else{stopApp()}
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
        corpus_table(p_villes)
        plot7<-plot_ly(p_villes,x=~as.integer(p_villes$date),y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot7<-layout(plot7, title="Distribution des numéros de presse en français \nselon la ville d'édition", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot7)
      }
      else if(input$corpus_structure_p==5){
        p_themes<-read.csv("p_themes.csv",encoding = "UTF-8")
        corpus_table(p_themes)
        plot11<-plot_ly(p_themes,x=~as.integer(p_themes$date),y=~n,color=~principaux_themes,type='bar',colors="Dark2")
        plot11<-layout(plot11, title="Distribution des numéros de presse en français \nselon le thème du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot11)
      }
      else if(input$corpus_structure_p==6){
        periodicite<-read.csv("periodicite.csv",encoding = "UTF-8")
        corpus_table(periodicite)
        plot16<-plot_ly(periodicite,x=~as.integer(periodicite$date),y=~n,color=~is_quotidien,type='bar',colors="Dark2")
        plot16<-layout(plot16, title="Distribution des numéros de presse en français \nselon la périodicité du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot16)
      }
      else if(input$corpus_structure_p==3){
        p_droits<-read.csv("p_droits.csv",encoding = "UTF-8")
        corpus_table(p_droits)
        plot5<-plot_ly(p_droits,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, title="Distribution des numéros de presse en français \nselon leur mode d'accès", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot5)
      }
      else if(input$corpus_structure_p==4){
        p_sources<-read.csv("p_sources.csv",encoding = "UTF-8")
        corpus_table(p_sources)
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
        corpus_table(p_villes)
        plot8<-plot_ly(p_villes,x=~as.integer(p_villes$date),y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot8<-layout(plot8, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon la ville d'édition", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot8)
      }
      else if(input$corpus_structure_p==5){
        p_themes<-read.csv("p_themes.csv",encoding = "UTF-8")
        corpus_table(p_themes)
        plot12<-plot_ly(p_themes,x=~as.integer(p_themes$date),y=~n,color=~principaux_themes,type='bar',colors="Dark2")
        plot12<-layout(plot12, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon le thème du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot12)
      }
      else if(input$corpus_structure_p==6){
        periodicite<-read.csv("periodicite.csv",encoding = "UTF-8")
        corpus_table(periodicite)
        plot17<-plot_ly(periodicite,x=~as.integer(periodicite$date),y=~n,color=~is_quotidien,type='bar',colors="Dark2")
        plot17<-layout(plot17, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon la périodicité du journal d'origine", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",barnorm="percent",bargap=0)
        return(plot17)
      }
      else if(input$corpus_structure_p==3){
        p_droits<-read.csv("p_droits.csv",encoding = "UTF-8")
        corpus_table(p_droits)
        plot5<-plot_ly(p_droits,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des numéros de presse en français \nselon leur mode d'accès", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",barnorm="percent",bargap=0)
        return(plot5)
      }
      else if(input$corpus_structure_p==4){
        p_sources<-read.csv("p_sources.csv",encoding = "UTF-8")
        corpus_table(p_sources)
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
        corpus_table_bis(p_villes_livres)
        plot3<-plot_ly(p_villes_livres,x=~date,y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot3<-layout(plot3, title="Distribution des livres en français \nselon leur ville de publication", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot3)
      }
      else if(input$corpus_structure_l==3){
        p_droits_livres<-read.csv("p_droits_livres.csv",encoding = "UTF-8")
        corpus_table_bis(p_droits_livres)
        plot5<-plot_ly(p_droits_livres,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, title="Distribution des livres en français \nselon leur régime juridique", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack",bargap=0)
        return(plot5)
      }
      else if(input$corpus_structure_l==4){
        p_sources_livres<-read.csv("p_sources_livres.csv",encoding = "UTF-8")
        corpus_table_bis(p_sources_livres)
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
        for (i in 1:(tableau1$date[1]-1)) {
          a<-as.data.frame(cbind(i,0))
          colnames(a)<-c("date","base")
          tableau1<-bind_rows(tableau1,a)
          tableau1<-tableau1[order(tableau1$date),]
          rownames(tableau1)<-NULL
        }
        for (i in 1:(tableau3$date[1]-1)) {
          a<-as.data.frame(cbind(i,0))
          colnames(a)<-c("date","base")
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
        tableau2<-select(tableau2,date,corpus,base)
        corpus_table_bis(tableau2)
        
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
        corpus_table_bis(p_dewey_livres)
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
        corpus_table_bis(p_villes_livres)
        plot3<-plot_ly(p_villes_livres,x=~date,y=~n,color=~principales_villes,type='bar',colors="Dark2")
        plot3<-layout(plot3, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des livres en français \nselon leur ville de publication", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot3)
      }
      else if(input$corpus_structure_l==3){
        p_droits_livres<-read.csv("p_droits_livres.csv",encoding = "UTF-8")
        corpus_table_bis(p_droits_livres)
        plot5<-plot_ly(p_droits_livres,x=~date,y=~n,color=~rights,type='bar',colors="Dark2")
        plot5<-layout(plot5, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4), title="Distribution des livres en français \nselon leur régime juridique", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Part des documents à chaque période"),barmode="stack",bargap=0,barnorm="percent")
        return(plot5)
      }
      else if(input$corpus_structure_l==4){
        p_sources_livres<-read.csv("p_sources_livres.csv",encoding = "UTF-8")
        corpus_table_bis(p_sources_livres)
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
        for (i in 1:(tableau1$date[1]-1)) {
          a<-as.data.frame(cbind(i,0))
          colnames(a)<-c("date","base")
          tableau1<-bind_rows(tableau1,a)
          tableau1<-tableau1[order(tableau1$date),]
          rownames(tableau1)<-NULL
        }
        for (i in 1:(tableau3$date[1]-1)) {
          a<-as.data.frame(cbind(i,0))
          colnames(a)<-c("date","base")
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
        tableau2<-select(tableau2,date,corpus,base)
        corpus_table_bis(tableau2)
        
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
        corpus_table_bis(p_dewey_livres)
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
      ploplo<-layout(ploplo, title="Distribution cumulée des livres en français \nselon leur date de numérisation", xaxis=list(title="Date",tickangle="-45"),yaxis=list(title="Nombre de documents"),barmode="stack")
      
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
  
  observeEvent(input$distribution,{
    if(input$distribution==1){
      table<-read.csv("base_presse_annees.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(gallicapublication_date%3E=%221380%22%20and%20gallicapublication_date%3C=%222021%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Gallica<b> </a>")
      type="de documents"
    }
    if(input$distribution==2){
      table<-read.csv("base_livres_annees.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22monographie%22)%20and%20(gallicapublication_date%3E=%221380%22%20and%20gallicapublication_date%3C=%222021%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," livres en français océrisés\ndans Gallica<b> </a>")
      type="de documents"
    }
    if(input$distribution==5){
      table<-read.csv("base_livres_annees_ngram_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://books.google.com/ngrams/graph?content=Joffre%252CP%C3%A9tain%252CFoch&year_start=1914&year_end=1920&corpus=30&smoothing=0&direct_url=t1%253B%252CJoffre%253B%252Cc0%253B.t1%253B%252CP%C3%A9tain%253B%252Cc0%253B.t1%253B%252CFoch%253B%252Cc0"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," livres en français exploités par\nNgram Viewer<b> </a>")
      type="de documents"
    }
    if(input$distribution==9){
      table<-read.csv("base_livres_annees_ngram_de.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://books.google.com/ngrams/graph?content=Hitler&year_start=1918&year_end=1945&corpus=31&smoothing=0&direct_url=t1%3B%2CHitler%3B%2Cc0"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," livres en allemand exploités par\nNgram Viewer<b> </a>")
      type="de documents"
    }
    if(input$distribution==10){
      table<-read.csv("base_livres_annees_ngram_en.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://books.google.com/ngrams/graph?content=Churchill&year_start=1918&year_end=1945&corpus=26&smoothing=0&direct_url=t1%3B%2CChurchill%3B%2Cc0"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," livres en anglais exploités par\nNgram Viewer<b> </a>")
      type="de documents"
    }
    if(input$distribution==12){
      table<-read.csv("base_livres_annees_ngram_es.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://books.google.com/ngrams/graph?content=Primo+de+Rivera&year_start=1918&year_end=1945&corpus=32&smoothing=0&direct_url=t1%3B%2CPrimo%20de%20Rivera%3B%2Cc0"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," livres en espagnol exploités par\nNgram Viewer<b> </a>")
      type="de documents"
    }
    if(input$distribution==6){
      table<-read.csv("base_presse_annees_europeana_de.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://classic.europeana.eu/portal/fr/collections/newspapers?f%5BLANGUAGE%5D%5B%5D=de&f%5BMEDIA%5D%5B%5D=true&f%5BTYPE%5D%5B%5D=TEXT&f%5Bapi%5D%5B%5D=collection&q="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en allemand océrisés\ndans Europeana<b> </a>")
      type="de documents"
    }
    if(input$distribution==7){
      table<-read.csv("base_presse_annees_europeana_nl.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://classic.europeana.eu/portal/fr/collections/newspapers?f%5BMEDIA%5D%5B%5D=true&f%5BTYPE%5D%5B%5D=TEXT&f%5Bapi%5D%5B%5D=collection&q=&f%5BLANGUAGE%5D%5B%5D=nl"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en néerlandais océrisés\ndans Europeana<b> </a>")
      type="de documents"
    }
    if(input$distribution==8){
      table<-read.csv("base_presse_annees_bna_en.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.britishnewspaperarchive.co.uk/search/results/1699-01-01/2021-12-31?basicsearch=a&exactsearch=false&contenttype=article&retrievecountrycounts=false"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," articles en anglais océrisés\ndans The British Newspaper Archive<b> </a>")
      type="d'articles"
    }
    if(input$distribution==11){
      table<-read.csv("base_presse_annees_bne_es.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="http://hemerotecadigital.bne.es/results.vm?d=creation&d=1700&d=01&d=01&d=2021&d=12&d=31&t=%2Bcreation&l=700&s=0&view=&lang=fr"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," pages de presse en espagnol océrisées\ndans Hemeroteca digital<b> </a>")
      type="de pages"
    }
    if(input$distribution==28){
      table<-read.csv("base_presse_annees_banq_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://numerique.banq.qc.ca/rechercheExterne/encoded/Kg==/false/D/asc/W3sibm9tIjoiY29ycHVzIiwidmFsZXVyIjoiUGF0cmltb2luZSUyMHF1w6liw6ljb2lzIn0seyJub20iOiJ0eXBlX2RvY19mIiwidmFsZXVyIjoiUmV2dWVzJTIwZXQlMjBqb3VybmF1eCJ9LHsibm9tIjoiZ2VucmVfZiIsInZhbGV1ciI6IkpvdXJuYXV4In0seyJub20iOiJhdmVjX3RleHRlX2ludGVncmFsIiwidmFsZXVyIjoib3VpIn0seyJub20iOiJsYW5ndWVzX2NvbnRlbnUiLCJ2YWxldXIiOiJmcmFuw6dhaXMifV0=/Toutes%20les%20ressources/true/false/"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans BAnQ<b> </a>")
      type="de documents"
    }
    if(input$distribution==13){
      table<-read.csv("base_presse_annees_kbr_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=&none_q=&from_d=1814&to_d=1970&per_lang=fr&per=&lang=FR&per_type=1"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," pages de presse en français océrisés\ndans KBR<b> </a>")
      type="de pages"
    }
    if(input$distribution==14){
      table<-read.csv("base_presse_annees_kbr_nl.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.belgicapress.be/pressshow.php?adv=1&all_q=&any_q=&exact_q=&none_q=&from_d=1814&to_d=1970&per_lang=nl&per=&lang=FR&per_type=1"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," pages de presse en néerlandais océrisés\ndans KBR<b> </a>")
      type="de pages"
    }
    if(input$distribution==15){
      table<-read.csv("base_presse_annees_e-newspaperarchives_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=der+OR+die+OR+das+OR+ein+OR+ich+OR+du+OR+er+OR+sie+OR+es&dafdq=01&dafmq=01&dafyq=1698&datdq=31&datmq=12&datyq=1698&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," articles de presse en français océrisés\ndans e-newspaperarchives<b> </a>")
      type="d'articles"
    }
    if(input$distribution==16){
      table<-read.csv("base_presse_annees_e-newspaperarchives_de.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.e-newspaperarchives.ch/?a=q&hs=1&r=1&results=1&txq=der+OR+die+OR+das+OR+ein+OR+ich+OR+du+OR+er+OR+sie+OR+es&dafdq=01&dafmq=01&dafyq=1698&datdq=31&datmq=12&datyq=1698&laq=de&puq=&txf=txIN&ssnip=&ccq=&l=fr"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," articles de presse en français océrisés\ndans e-newspaperarchives<b> </a>")
      type="d'articles"
    }
    if(input$distribution==17){
      table<-read.csv("base_presse_annees_lectura_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.lectura.plus/Presse/search/?query=&fromDate=01%2F01%2F1800&untilDate=31%2F12%2F2021"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," pages de presse en français océrisés\ndans Lectura<b> </a>")
      type="de pages"
    }
    if(input$distribution==18){
      table<-read.csv("base_presse_annees_limedia_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://kiosque.limedia.fr/recherche/?query=&search_type=exact&uniform_title=&date=&period_start=01%2F01%2F1600&period_end=31%2F12%2F2021"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Limedia<b> </a>")
      type="de documents"
    }
    if(input$distribution==19){
      table<-read.csv("base_presse_annees_memonum_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://memonum-mediatheques.montpellier3m.fr/form.aspx?SC=MEMONUM_ENCART_SEARCH#/Search/(query:(ForceSearch:!t,Grid:%7B%7D,Page:0,PageRange:3,QueryString:!n,ResultSize:10,ScenarioCode:MEMONUM_ENCART_SEARCH,SearchContext:1))"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Mémonum<b> </a>")
      type="de documents"
    }
    if(input$distribution==20){
      table<-read.csv("base_presse_annees_communpatrimoine_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.communpatrimoine.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Commun patrimoine<b> </a>")
      type="de documents"
    }
    if(input$distribution==21){
      table<-read.csv("base_presse_annees_yroise_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://yroise.biblio.brest.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Yroise<b> </a>")
      type="de documents"
    }
    if(input$distribution==22){
      table<-read.csv("base_presse_annees_pireneas_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.pireneas.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Pireneas<b> </a>")
      type="de documents"
    }
    if(input$distribution==23){
      table<-read.csv("base_presse_annees_rosalis_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://rosalis.bibliotheque.toulouse.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Rosalis<b> </a>")
      type="de documents"
    }
    if(input$distribution==24){
      table<-read.csv("base_presse_annees_bdn_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://bibliotheque-numerique.diplomatie.gouv.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans la Bibliothèque diplomatique numérique<b> </a>")
      type="de documents"
    }
    if(input$distribution==25){
      table<-read.csv("base_presse_annees_rfnum_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="http://rfnum-bibliotheque.org/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans la Bibliothèque francophone numérique<b> </a>")
      type="de documents"
    }
    if(input$distribution==26){
      table<-read.csv("base_presse_annees_numistral_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.numistral.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans Numistral<b> </a>")
      type="de documents"
    }
    if(input$distribution==27){
      table<-read.csv("base_presse_annees_bn-r_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.bn-r.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=null&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20sortby%20dc.date/sort.ascending&suggest=10&keywords="
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en français océrisés\ndans la Bibliothèque numérique de Roubaix<b> </a>")
      type="de documents"
    }
    if(input$distribution==29){
      table<-read.csv("base_presse_annees_anno_de.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://anno.onb.ac.at/anno-suche#searchMode=complex&language=ger&dateMode=period&from=1"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," numéros de presse en allemand océrisés\ndans AustriaN Newspapers Online<b> </a>")
      type="de documents"
    }
    if(input$distribution==30){
      table<-read.csv("base_presse_annees_lemonde_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://www.lemonde.fr/archives-du-monde/"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," monogrammes\n extraits du journal Le Monde\n jusqu'au 31 juillet 2022<b> </a>")
      type="de monogrammes"
    }
    if(input$distribution==31){
      table<-read.csv("base_presse_annees_lefigaro_fr.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://recherche.lefigaro.fr/recherche/"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," articles du journal\n Le Figaro (numérique)<b> </a>")
      type="d'articles"
    }
    if(input$distribution==36){
      table<-read.csv("base_isidore.csv",encoding="UTF-8")
      somme<-sum(table$base)
      lien="https://isidore.science/"
      Title = str_c("<a href = '",lien,"'> <b>Répartition des ",somme," documents de la\n base isidore.science<b> </a>")
      type="de documents"
    }
    
    table$hovers = str_c(table$date,": N = ",table$base)
    plot2<-plot_ly(table, x=~date,y=~base,text=~hovers,type='bar',hoverinfo="text")
    y <- list(title = str_c("Nombre ",type),titlefont = 41)
    x <- list(title = "Date",titlefont = 41)
    plot2 = layout(plot2, yaxis = y, xaxis = x,title = Title)
    output$d_plot<-renderPlotly({plot2})
  })
  
  observeEvent(input$d_language,{
    observeEvent(input$d_bibli,{
      if(input$d_language == 1 & input$d_bibli==1){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Presse française / Gallica" = 1, "Livres / Gallica" = 2),selected = 1)
      }
      else if(input$d_language == 1 & input$d_bibli==2){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse wallonne / KBR"=13, "Presse québécoise / BAnQ"=28, "Livres / Ngram Viewer - Google Books" = 5),selected = 5)
      }
      else if(input$d_language == 1 & input$d_bibli==3){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27),selected = 17)
      }
      else if(input$d_language == 2){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Presse allemande / Europeana" = 6,"Presse austro-hongroise / ANNO"=29,"Presse suisse-allemande / Bibliothèque nationale suisse"=16 , "Livres / Ngram Viewer Allemand" = 9),selected = 6)
      }else if(input$d_language == 3){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Presse néerlandaise / Europeana" = 7,"Presse flamande / KBR"=14),selected = 7)
      }else if(input$d_language == 4){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Presse britannique / BNA" = 8, "Livres / Ngram Viewer Anglais" = 10),selected = 8)
      }else if(input$d_language == 5){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12),selected = 11)
      }
      else if(input$d_language == 1 & input$d_bibli==4){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Le Monde"=30, "Le Figaro"=31),selected = 30)
      }
      else if(input$d_language == 1 & input$d_bibli==5){
        updateSelectInput(session,"distribution", "Corpus",choices = list("Isidore"=36),selected = 36)
      }
    })})
  
  output$pdfview <- renderUI({
    tags$iframe(style="height:600px; width:100%", src="presentation_gallicagram.pdf")
  })
  
  ##################
  if(data[[2]]=="liberté&république"){
    output$lien=renderUI(HTML(str_c("<b><font size=\"5\">Contexte</font><br>Crédit : Will Gleason avec <a href='","https://www.gallicagrapher.com/","' target='_blank'>","Gallicagrapher","</a></b></font>","<br><a href='","https://gallica.bnf.fr/services/engine/search/sru?operation=searchRetrieve&exactSearch=true&maximumRecords=20&startRecord=0&collapsing=false&version=1.2&query=(dc.language%20all%20%22fre%22)%20and%20(text%20adj%20%22libert%C3%A9%22%20)%20%20and%20(dc.type%20all%20%22fascicule%22)%20and%20(ocr.quality%20all%20%22Texte%20disponible%22)%20and%20(gallicapublication_date%3E=%221788/01/01%22%20and%20gallicapublication_date%3C=%221788/01/31%22)&suggest=10&keywords=libert%C3%A9","' target='_blank'>","Ouvrir la recherche dans Gallica","</a>",
                                    "<br><a href='","https://www.gallicagrapher.com/context?terms=libert%C3%A9&source=periodical&sort=relevance&year=1788","' target='_blank'>","Plus de contexte","</a>")))
    output$frame<-renderDataTable(read.csv("DTexemple.csv",header = T,encoding = "UTF-8",sep = ","),escape = F,options = list(pageLength = 10, lengthChange = FALSE, columnDefs = list(list(className = 'dt-body-right', targets = 3))))
  }
  ##################
  
  shinyOptions(progress.style="old")
  
  remove_modal_spinner()
})
