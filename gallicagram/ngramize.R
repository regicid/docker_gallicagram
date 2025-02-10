extract_mot = function(mot){
  table<-unnest_tokens(as.data.frame(mot),ngram,mot, token = "ngrams", n = 1)
  nb<-length(table$ngram)
  return(paste(table$ngram,collapse=" "))
}

ngramize<-function(input,nouvrequette,gallicagram,agregator){
  library(glue)
  show_spinner(spin_id="ngram")
  require("RSQLite")
  require("DBI")
  from<-input$beginning
  to<-input$end
  resolution <- recode(input$resolution, "Année"="annee","Mois"="mois")
  url_base = "https://shiny.ens-paris-saclay.fr/guni" #If you are not on the gallicagram server
  if(Sys.info()["nodename"]=="shiny"){url_base = "http://127.0.0.1:8000"} #Use localhost when on shiny server
  if(input$doc_type==30 & input$cooccurrences){
    mots = str_split(input$mot,"&")[[1]]
    for(mot_cooccur in mots){
      if(str_detect(mot_cooccur,"\\*")){
        resolution = recode(input$resolution,"Mois"="mois","Année"="annee")
        mot1 = str_split(mot_cooccur,"\\*")[[1]][1] %>% tolower()
        mot2 = str_split(mot_cooccur,"\\*")[[1]][2] %>% tolower()
        if(input$scale_cooccur==1){df = read.csv(glue("{url_base}/cooccur?mot1={mot1}&mot2={mot2}&from={from}&to={to}&resolution={resolution}"))
        df = dplyr::rename(df,count=n,base = total)
        }else{df = read.csv(glue("{url_base}/contain?mot1={mot1}&mot2={mot2}&from={from}&to={to}&resolution={resolution}"))
        df = dplyr::rename(df,count=n,base=total)
        }
      }
      df$mot = mot_cooccur
      if(mot_cooccur==mots[1]){tableau = df
      }else{tableau = rbind(tableau,df)}
    }
    tableau$url = "https://www.lemonde.fr/"
    tableau$corpus="Presse"
    tableau$langue="Français"
    tableau$bibli="Le Monde"
  }
  
  ##Persée
  if(input$doc_type == 34){
    mots = str_split(input$mot,"&")[[1]]
    for(mots1 in mots){
      mots2 = str_split(mots1,"[+]")[[1]]
      for(mot in mots2){
        mot = extract_mot(mot)
        df = read.csv(glue("{url_base}/query_persee?mot={URLencode(mot)}&from={from}&to={to}&by_revue={str_to_title(tolower(input$persee_by_revue))}&revue=",paste(input$rev_persee,collapse="+")))
        df = dplyr::rename(df,count=n,base = total,mot=gram)
        print(df)
        if(mot == extract_mot(mots2[1])){df_long=df
        }else{df_long = rbind(df_long,df)}
      }
      if(input$persee_by_revue){
        df_sum = df_long %>% dplyr::group_by(annee,revue) %>% dplyr::summarise(count=sum(count),base = mean(base),revue=unique(revue))
        }else{df_sum = df_long %>% dplyr::group_by(annee) %>% dplyr::summarise(count=sum(count),base = mean(base))}
      df_sum$mot = mots1
      if(mots1==mots[1]){tableau = df_sum
      }else{tableau = rbind(tableau,df_sum)}
    }
    tableau$url =str_c("https://www.persee.fr/search?l=fre&da=",tableau$annee,"&q=",tableau$mot)
    if(length(input$rev_persee) < 362){tableau$url = str_c(tableau$url,paste(str_c("&c=",input$rev_persee),collapse=""))}
    #!identical(input$rev_persee,"all")
    tableau$corpus="Presse"
    tableau$langue="Français"
    tableau$bibli="Persée"
  }
  if(input$doc_type == 30){##LeMonde_rubriques
    mots = str_split(input$mot,"&")[[1]]
    for(mots1 in mots){
      mots2 = str_split(mots1,"[+]")[[1]]
      for(mot in mots2){
        mot = extract_mot(mot)
        df = read.csv(glue("{url_base}/query?corpus=lemonde_rubriques&mot={URLencode(mot)}&from={from}&to={to}&rubrique={paste(input$rubrique_lemonde,collapse='+')}&by_rubrique={str_to_title(tolower(input$lemonde_by_rubrique))}&resolution={resolution}"))
        df = dplyr::rename(df,count=n,base = total,mot=gram)
        print(df)
        if(mot == extract_mot(mots2[1])){df_long=df
        }else{df_long = rbind(df_long,df)}
      }
      if(input$lemonde_by_rubrique){
        df_sum = df_long %>% dplyr::group_by_at(vars(intersect(c("annee","mois","rubrique"), names(df_long)))) %>%
           dplyr::summarise(count=sum(count),base = mean(base),rubrique=unique(rubrique)) %>% ungroup()
      }else{
        group_cols <- c("annee", if ("mois" %in% colnames(df_long)) "mois" else NULL)
        df_sum = df_long %>% dplyr::group_by(across(all_of(group_cols))) %>% dplyr::summarise(count=sum(count),base = mean(base))  %>% ungroup()}
      df_sum$mot = mots1
      if(mots1==mots[1]){tableau = df_sum
      }else{tableau = rbind(tableau,df_sum)}
    }
    tableau$url =str_c("https://www.lemonde.fr/recherche/?search_keywords=%22",tableau$mot,"%22&start_at=01%2F01%2F",tableau$annee,"&end_at=31%2F12%2F",tableau$annee)
    #!identical(input$rev_persee,"all")
    if("mois" %in% colnames(tableau)){
      z = tableau$mois < 10
      tableau$mois[z] = str_c("0",tableau$mois[z])
    }
    tableau$corpus="Le Monde"
    tableau$langue="Français"
    tableau$bibli="Le Monde"
  }
  if(input$doc_type %in% c(81,82)){
    mots = str_split(input$mot,"&")[[1]]
    for(mots1 in mots){
      mots2 = str_split(mots1,"[+]")[[1]]
      for(mot in mots2){
        mot = extract_mot(mot)
        corpus = recode(input$doc_type,`81`="rap",`82`="prenoms")
        df = read.csv(glue("{url_base}/query?corpus={corpus}&mot={URLencode(mot)}&from={from}&to={to}"))
        df = dplyr::rename(df,count=n,base = total,mot=gram)
        print(df)
        if(mot == extract_mot(mots2[1])){df_long=df
        }else{df_long = rbind(df_long,df)}
      }
     df_sum = df_long %>% dplyr::group_by(annee) %>% dplyr::summarise(count=sum(count),base = mean(base))
      df_sum$mot = mots1
      if(mots1==mots[1]){tableau = df_sum
      }else{tableau = rbind(tableau,df_sum)}
    }
    mot_url = str_replace_all(tableau$mot," ","%20")
    mot_url = str_replace_all(mot_url,"\\+","\\|")
    tableau$url =str_c("https://shiny.ens-paris-saclay.fr/guni/source_rap?mot=",mot_url,"&year=",tableau$annee)
    #!identical(input$rev_persee,"all")
    tableau$corpus="Rap"
    tableau$langue="Français"
    tableau$bibli="Genius"
  }
  if(input$doc_type %in% c(30,34,81,82)){
    if(input$resolution=="Mois"){tableau$date = paste(tableau$annee,tableau$mois,sep="/")
    tableau = select(tableau,-annee,-mois)}
    if(input$resolution=="Année"){tableau$date = tableau$annee}
    tableau$search_mode<-"N-gramme"
    print(tableau)
    tableau$ratio = tableau$count/tableau$base
    data = list(tableau,paste(input$mot,collapse="&"),input$resolution)
    names(data) = c("tableau","mot","resolution")
    hide_spinner(spin_id="ngram")
    return(data)
  }
  
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
      if((input$doc_type==30&input$cooccurrences==FALSE) | gallicagram==2){
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
      if((input$doc_type==1 | (input$doc_type==30&input$cooccurrences==FALSE) | input$doc_type==0 |
          input$doc_type %in% 66:76 | input$doc_type %in% 77:78 | input$doc_type==43 | (input$doc_type==99&nb<4)|
          (input$doc_type==56 & agregator==1)) & input$resolution=="Année"){
        #q=str_c('SELECT n,annee FROM gram',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'"')
        q=str_c('SELECT sum(n),annee FROM gram',' WHERE annee BETWEEN ',from," AND ",to ,' AND ',gram,'="',mot,'" group by annee')
        if((input$doc_type==30&input$cooccurrences==FALSE) | input$doc_type %in% 66:76){
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
      if((input$doc_type==1 | (input$doc_type==30&input$cooccurrences==FALSE) | input$doc_type==0 | input$doc_type==43| input$doc_type %in% 66:76)  & input$resolution=="Mois"){
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
