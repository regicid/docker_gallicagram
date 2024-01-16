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
  if(input$doc_type==34 & isolate(input$persee_by_revue) & "revue" %in% colnames(tableau)){
    tableau$date = tableau$annee
    tableau$date<-str_c(tableau$date,"/01/01")
    tableau$date<-as.Date.character(tableau$date,format = c("%Y/%m/%d"))
    if(!input$visualiseur %in% c(6,9)){
      tableau = tableau %>% group_by(revue,date) %>% dplyr::summarise(count=sum(count))
      tableau_sum = tableau %>% group_by(revue) %>% dplyr::summarise(count=sum(count))
      tableau_sum = tableau_sum[tableau_sum$count > 0,]
      tableau_sum = tableau_sum[order(tableau_sum$count,decreasing = T),][1:input$nb_max_revues,]
      tableau = tableau[tableau$revue %in% tableau_sum$revue,]
    }
    revues_persee = fromJSON("revues_persee.json")
    names_revues = unique(unlist(revues_persee))
    codes_revues=unique(str_remove(str_extract(unique(names(unlist(revues_persee))),"\\..+"),"\\."))
    names(names_revues) = codes_revues
    if(input$visualiseur==1){
      tableau$count_loess = tableau$count
      for(revue in unique(tableau$revue)){
        if(input$span>0){
          z = which(tableau$revue==revue)
          for(i in 1:length(z)){
            j = max(i-floor(input$span/2),0)
            k = i+ceiling(input$span/2)
            if(is.na(tableau$count[i])){
              next
            }
            tableau$count_loess[z][i] = mean(tableau$count[z][j:k],na.rm=T)
          }}
      }
      
      tableau$revue = names_revues[tableau$revue]
      plot_persee_par_doc = ggplot(tableau,aes(date,count_loess,fill=revue)) + 
        geom_area() + theme_minimal() + xlab("Date") + ylab("Nombre d'occurrences")
      #plot_persee_par_doc = ggplot(tableau,aes(date,count,fill=revue)) + geom_bar(position="stack", stat="identity") + theme_minimal()
      if(length(unique(tableau$revue))>15){plot_persee_par_doc = plot_persee_par_doc + guides(fill="none")}
      plot_persee_par_doc = ggplotly(plot_persee_par_doc)
    }
    if(input$visualiseur==2){
      tableau = tableau %>% dplyr::group_by(revue) %>% dplyr::summarise(count = sum(count)) 
      #ggplot(tableau,aes(y=reorder(revue,x=count)) + geom_bar(stat = 'identity')
      tableau$url = str_c("https://www.persee.fr/search?l=fre&da=",input$from,"-",input$to,"&q=%22",data$mot,"%22","&c=",tableau$revue)
      tableau$revue = names_revues[tableau$revue]
      plot_persee_par_doc<-plot_ly(x=~tableau$count,y=reorder(tableau$revue,tableau$count),type="bar",customdata=tableau$url)
      plot_persee_par_doc = plot_persee_par_doc %>% layout(yaxis = list(title = "Nombres d'occurrences"))
    }
    if(input$visualiseur %in% c(6,9)){
      library(FactoMineR)
      library(factoextra)
      library(tidyr)
      total = tableau %>% group_by(revue,mot) %>% dplyr::summarise(count=sum(count))
      a=data.frame(spread(total, mot,count))
      rownames(a)=names_revues[a$revue]
      a = a[-1]
      z = rowSums(a)
      z = z[order(z,decreasing = T)][1:input$nb_max_revues]
      a = a[row.names(a) %in% names(z),]
      if(input$visualiseur==6){
        res.pca=PCA(a,scale.unit = TRUE)
        rownames(res.pca$ind$coord)=rownames(a)
        bb<-fviz_pca_biplot(res.pca,geom.var = c("text"),geom.ind = c("text"), label="all",labelsize=3,col.var="red")+labs(title="") 
      }
      if(input$visualiseur==9){
        res.pca=CA(a)
        rownames(res.pca$row$coord)=rownames(a)
        bb<-fviz_ca_biplot(res.pca,geom.col = c("text"),geom.row = c("text"), label="all",labelsize=3,col.col="red")+labs(title="") 
      }
      plot_persee_par_doc = ggplotly(bb)
    }
    return(onRender(plot_persee_par_doc,js))
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
  if(input$daltonien){
    linetype = tableau$mot
  }else{linetype=NULL}
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
    plot = plot_ly(tableau, x=~date,y=~loess,color =~mot,type='scatter',mode='lines+markers',line = list(shape = "spline"),linetype = linetype,customdata=tableau$url,colors=customPalette,legendgroup=~mot,text=~hovers,hoverinfo="text",connectgaps = FALSE)
    if(isolate(input$doc_type)!=5 & isolate(input$doc_type)!=9 & isolate(input$doc_type)!=10 & isolate(input$doc_type)!=12 & isolate(input$doc_type)!=44 & isolate(input$doc_type)!=58 & isolate(input$doc_type)!=59 & isolate(input$doc_type)!=60 & isolate(input$doc_type)!=61 & isolate(input$doc_type)!=62 & isolate(input$doc_type)!=63 & isolate(input$doc_type)!=64 & input$scale==F & input$multicourbes==F){
      plot=plot%>%add_ribbons(data=tableau,x=~date,ymin=~ribbon_down,ymax=~ribbon_up,legendgroup=~mot,fillcolor=~mot,showlegend=F,opacity=.2)
    }
    plot = plot %>% add_trace(x=~date,y=~loess,color=~mot,legendgroup=~mot,showlegend=F,connectgaps = FALSE)
  }  else{
    plot = plot_ly(data=tableau, x=~date,y=~loess,color =~mot,type='scatter',mode='lines',line = list(shape = "spline"),linetype = linetype,customdata=tableau$url,colors=customPalette,legendgroup=~mot,text=~hovers,hoverinfo="text",connectgaps = FALSE)
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
