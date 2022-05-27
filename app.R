library(shiny)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(stringr)
library(rvest)
library(fst)
library(tm)
library(jsonlite)
library(htmltools)


liste <- read_html("https://fr.wikipedia.org/wiki/Projet:Wikip%C3%A9dia_1.0/Les_plus_consult%C3%A9s/Pages_populaires") %>%
  html_nodes("td:nth-child(2)") %>%
  html_text()



# liste <- read_html("https://pageviews.wmcloud.org/topviews/?project=fr.wikipedia.org&platform=all-access&date=2022-05-21&excludes=") %>%
#   html_nodes(".topview-entry--label a") %>%
#   html_text()


liste <- str_remove_all(liste,"\n")
liste <- liste[nchar(liste) < 25]
liste <- liste[nchar(liste) > 3]
liste <- liste[-(which(str_detect(liste,"\\)")))]

name <- liste[sample(1:length(liste),1)]
texte <- c()
titre <- c()
name <- URLencode(name)
json <- fromJSON(paste0("https://fr.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=", name))
if(as.numeric(names(json$query$pages)) == -1){
  texte <- c(texte, " ")
    
}else{
  texte <-  c(texte,json[["query"]][["pages"]][[1]][["extract"]],collapse = " ")
  titre <- c(titre,json[["query"]][["pages"]][[1]][["title"]],collapse = " ")
}
texte <- texte[texte != " "]
titre <- titre[titre != " "]

x <- texte

x <- str_replace_all(x,"\n", " \n ")
x <- str_replace_all(x,"\\.", " \\. ")
x <- str_replace_all(x,"-", " - ")
x <- str_replace_all(x,",", " , ")
x <- str_replace_all(x,":", " : ")
x <- str_replace_all(x,"\\)", " \\) ")
x <- str_replace_all(x,"\\(", " \\( ")
x <- str_replace_all(x,"—", " — ")
x <- str_replace_all(x,"…", " … ")
x <- str_replace_all(x,"'", " ' ")
x <- str_replace_all(x,"=", " = ")
x <- str_replace_all(x,";", " ; ")
x <- str_replace_all(x,"/", " / ")
x <- str_replace_all(x,"\\[", " \\[ ")
x <- str_replace_all(x,"\\]", " \\] ")
x <- str_replace_all(x,"\\+", " \\+ ")
x <- str_replace_all(x,"\\’", " \\’ ")

q <- titre

q <- str_replace_all(q,"\n", " \n ")
q <- str_replace_all(q,"\\.", " \\. ")
q <- str_replace_all(q,"-", " - ")
q <- str_replace_all(q,",", " , ")
q <- str_replace_all(q,":", " : ")
q <- str_replace_all(q,"\\)", " \\) ")
q <- str_replace_all(q,"\\(", " \\( ")
q <- str_replace_all(q,"—", " — ")
q <- str_replace_all(q,"…", " … ")
q <- str_replace_all(q,"'", " ' ")
q <- str_replace_all(q,"=", " = ")
q <- str_replace_all(q,";", " ; ")
q <- str_replace_all(q,"/", " / ")
q <- str_replace_all(q,"\\[", " \\[ ")
q <- str_replace_all(q,"\\]", " \\] ")
q <- str_replace_all(q,"\\+", " \\+ ")
q <- str_replace_all(q,"\\’", " \\’ ")


texte_titre <- strsplit(q,split=" ")
texte_titre <- texte_titre[[1]]
texte_titre <- texte_titre[ texte_titre != ""]
texte5_titre <- texte_titre
texte_titre <- tolower(texte_titre)
texte3_titre <- rep(" ", length(texte_titre))
for(i in 1:length(texte_titre)){
  if(texte_titre[i] %in% c("\n",".","-",",",":",")","(","—","…","'","«","»","=",";","/","[","]","+","’")){
    texte3_titre[i] <- texte_titre[i]
  }else{
    texte3_titre[i] <- paste(rep("■",nchar(texte_titre[i])),collapse = "")
  }
}

texte4_titre <- paste(texte3_titre,collapse = " ")

fin_titre <- HTML(paste('<br/>',texte4_titre,'<br/>',collapse= " "))



texte2 <- strsplit(x,split=" ")
texte2 <- texte2[[1]]
texte2 <- texte2[ texte2 != ""]
texte5 <- texte2
texte2 <- tolower(texte2)
texte3 <- rep(" ",length(texte2))
for(i in 1:length(texte2)){
  if(texte2[i] %in% c("\n",".","-",",",":",")","(","—","…","'","«","»","=",";","/","[","]","+","’")){
    texte3[i] <- texte2[i]
  }else{
    texte3[i] <- paste(rep("■",nchar(texte2[i])),collapse = "")
  }
}

texte4 <- paste(texte3,collapse = " ")


text6 <- str_replace_all(texte4," \n ","\n")
text6 <- str_replace_all(text6," \\. ","\\. ")
text6 <- str_replace_all(text6, " - ","-")
text6 <- str_replace_all(text6, " , ",", ")
text6 <- str_replace_all(text6, " \\: "," \\: ")
text6 <- str_replace_all(text6, " \\) ","\\) ")
text6 <- str_replace_all(text6, " \\( "," \\(")
text6 <- str_replace_all(text6, " … ","… ")
text6 <- str_replace_all(text6, " ' ","'")
text6 <- str_replace_all(text6, " = ","=")
text6 <- str_replace_all(text6, " ; ",";")
text6 <- str_replace_all(text6, " / "," / ")
text6 <- str_replace_all(text6, " \\[ "," \\[ ")
text6 <- str_replace_all(text6, " \\] "," \\] ")
text6 <- str_replace_all(text6, " \\+ "," \\+")
text6 <- str_replace_all(text6, " \\’ ","\\’")
text6 <- strsplit(text6, split = "\n")
text6 <- text6[[1]]
rr <- c()
for(i in 1:length(text6)){
  rr <- c(rr,c(text6[i],'<br/>','<br/>'))
}
#rr <- rr[-c(length(rr),length(rr)-1)]
rr <- c('<br/>',rr)
fin <- HTML(paste(rr,collapse= " "))

Play_titre <- function(x){
  x <- tolower(x)
  if(x %in% texte_titre){
    w <- which(texte_titre == x)
    for(k in w){
      texte3_titre[k] <- texte5_titre[k]
    }
    texte4_titre <- paste(texte3_titre,collapse = " ")
    texte3_titre <<- texte3_titre
    texte4_titre <<- texte4_titre
    fin_titre <- HTML(paste('<br/>',texte4_titre,'<br/>',collapse = " "))
    if(identical(texte3_titre,texte5_titre)==TRUE){
      fin_titre <- HTML(paste('<br/>',"🎉 Félicitations vous avez trouvé 🎉",'<br/>','<br/>',texte4_titre,collapse = " "))
      fin_titre <<- fin_titre
      enable("submit4")
      disable("submit3")
      disable("submit5")
      fin_titre
    }else{
      fin_titre <<- fin_titre
      fin_titre
    }
  }else{
    fin_titre
  }
}


u <- c()
Play <- function(x){
  g <- str_to_upper(x)
  m <- str_to_title(x)
  x <- tolower(x)
  if(x %in% c(""," ","  ","   ","    ","     ")){
    fin
  }else{
    if(x %in% u){
      fin
    }else{
      if(x %in% texte2){

        z <- which(texte2 == x)
        for(j in z){
            texte3[j] <- paste0("<span style='color:red;font-weight: 650'>",texte5[j],"</span>")
        }
        texte3[which(texte3 %in% paste0("<span style='color:red;font-weight: 650'>",u,"</span>"))] <- str_replace_all(texte3[which(texte3 %in% paste0("<span style='color:red;font-weight: 650'>",u,"</span>"))],"style='color:red;font-weight: 650'","style='color:rgb(38,65,71);font-weight: normal'")
        texte4 <- paste(texte3,collapse = " ")
    
        texte4 <- str_replace_all(texte4," \n ","\n")
        texte4 <- str_replace_all(texte4," \\. ","\\. ")
        texte4 <- str_replace_all(texte4, " - ","-")
        texte4 <- str_replace_all(texte4, " , ",", ")
        texte4 <- str_replace_all(texte4, " \\: "," \\: ")
        texte4 <- str_replace_all(texte4, " \\) ","\\) ")
        texte4 <- str_replace_all(texte4, " \\( "," \\(")
        texte4 <- str_replace_all(texte4, " … ","… ")
        texte4 <- str_replace_all(texte4, " ' ","'")
        texte4 <- str_replace_all(texte4, " = ","=")
        texte4 <- str_replace_all(texte4, " ; ",";")
        texte4 <- str_replace_all(texte4, " / "," / ")
        texte4 <- str_replace_all(texte4, " \\[ "," \\[ ")
        texte4 <- str_replace_all(texte4, " \\] "," \\] ")
        texte4 <- str_replace_all(texte4, " \\+ "," \\+ ")
        texte4 <- str_replace_all(texte4, " \\’ ","\\’")
        texte3 <<- texte3
        texte4 <<- texte4
        texte7 <- strsplit(texte4, split = "\n") %>% unlist()
        r <- c()
        for(i in 1:length(texte7)){
          r <- c(r,c(texte7[i],'<br/>','<br/>'))
        }
#        r <- r[-c(length(r),length(r)-1)]
        r <- c('<br/>',r)
        fin <- HTML(r,collapse= " ")
        fin <<- fin
        b <<- b + 1
        u <- c(u,x,g,m)
        u <<- u
        ind <- ind[-(ind == m)==F]
        ind <- ind[-(ind == g)==F]
        ind <- ind[-(ind %in% u)==F]
        ind <<- ind
        fin
      }else{
        u <- c(u,x,g,m)
        u <<- u
        b <<- b + 1
        fin
      }
    }
  }
}


NouvellePartie <- function(){
  
  enable("submit3")
  disable("submit4")
  disable("submit5")
  liste <- read_html("https://fr.wikipedia.org/wiki/Projet:Wikip%C3%A9dia_1.0/Les_plus_consult%C3%A9s/Pages_populaires") %>%
    html_nodes("td:nth-child(2)") %>%
    html_text()
  
  liste <- str_remove_all(liste,"\n")
  liste <- liste[nchar(liste) < 25]
  liste <- liste[-(which(str_detect(liste,"\\)")))]
  
  name <- liste[sample(1:length(liste),1)]
  texte <- c()
  titre <- c()
  name <- URLencode(name)
  json <- fromJSON(paste0("https://fr.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=", name))
  if(as.numeric(names(json$query$pages)) == -1){
    texte <- c(texte, " ")
    
  }else{
    texte <-  c(texte,json[["query"]][["pages"]][[1]][["extract"]],collapse = " ")
    titre <- c(titre,json[["query"]][["pages"]][[1]][["title"]],collapse = " ")
  }
  texte <- texte[texte != " "]
  texte <<- texte
  titre <- titre[titre != " "]
  titre <<- titre
  
  
  
  x <- texte
  
  x <- str_replace_all(x,"\n", " \n ")
  x <- str_replace_all(x,"\\.", " \\. ")
  x <- str_replace_all(x,"-", " - ")
  x <- str_replace_all(x,",", " , ")
  x <- str_replace_all(x,":", " : ")
  x <- str_replace_all(x,"\\)", " \\) ")
  x <- str_replace_all(x,"\\(", " \\( ")
  x <- str_replace_all(x,"—", " — ")
  x <- str_replace_all(x,"…", " … ")
  x <- str_replace_all(x,"'", " ' ")
  x <- str_replace_all(x,"=", " = ")
  x <- str_replace_all(x,";", " ; ")
  x <- str_replace_all(x,"/", " / ")
  x <- str_replace_all(x,"\\[", " \\[ ")
  x <- str_replace_all(x,"\\]", " \\] ")
  x <- str_replace_all(x,"\\+", " \\+ ")
  x <- str_replace_all(x,"\\’", " \\’ ")
  x <<- x
  
  q <- titre
  
  q <- str_replace_all(q,"\n", " \n ")
  q <- str_replace_all(q,"\\.", " \\. ")
  q <- str_replace_all(q,"-", " - ")
  q <- str_replace_all(q,",", " , ")
  q <- str_replace_all(q,":", " : ")
  q <- str_replace_all(q,"\\)", " \\) ")
  q <- str_replace_all(q,"\\(", " \\( ")
  q <- str_replace_all(q,"—", " — ")
  q <- str_replace_all(q,"…", " … ")
  q <- str_replace_all(q,"'", " ' ")
  q <- str_replace_all(q,"=", " = ")
  q <- str_replace_all(q,";", " ; ")
  q <- str_replace_all(q,"/", " / ")
  q <- str_replace_all(q,"\\[", " \\[ ")
  q <- str_replace_all(q,"\\]", " \\] ")
  q <- str_replace_all(q,"\\+", " \\+ ")
  q <- str_replace_all(q,"\\’", " \\’ ")
  q <<- q
  
  
  texte_titre <- strsplit(q,split=" ")
  texte_titre <- texte_titre[[1]]
  texte_titre <- texte_titre[ texte_titre != ""]
  texte5_titre <- texte_titre
  texte5_titre <<- texte5_titre
  texte_titre <- tolower(texte_titre)
  texte_titre <<- texte_titre
  texte3_titre <- rep(" ", length(texte_titre))
  for(i in 1:length(texte_titre)){
    if(texte_titre[i] %in% c("\n",".","-",",",":",")","(","—","…","'","«","»","=",";","/","[","]","+","’")){
      texte3_titre[i] <- texte_titre[i]
    }else{
      texte3_titre[i] <- paste(rep("■",nchar(texte_titre[i])),collapse = "")
    }
  }
  texte3_titre <<- texte3_titre
  texte4_titre <- paste(texte3_titre,collapse = " ")
  texte4_titre <<- texte4_titre
  
  fin_titre <- HTML(paste('<br/>',texte4_titre,'<br/>',collapse= " "))
  fin_titre <<- fin_titre
  
  
  texte2 <- strsplit(x,split=" ")
  texte2 <- texte2[[1]]
  texte2 <- texte2[ texte2 != ""]
  texte5 <- texte2
  texte5 <<- texte5
  texte2 <- tolower(texte2)
  texte2 <<- texte2
  texte3 <- rep(" ",length(texte2))
  for(i in 1:length(texte2)){
    if(texte2[i] %in% c("\n",".","-",",",":",")","(","—","…","'","«","»","=",";","/","[","]","+","’")){
      texte3[i] <- texte2[i]
    }else{
      texte3[i] <- paste(rep("■",nchar(texte2[i])),collapse = "")
    }
  }
  texte3 <<- texte3
  texte4 <- paste(texte3,collapse = " ")
  texte4 <<- texte4
  
  
  text6 <- str_replace_all(texte4," \n ","\n")
  text6 <- str_replace_all(text6," \\. ","\\. ")
  text6 <- str_replace_all(text6, " - ","-")
  text6 <- str_replace_all(text6, " , ",", ")
  text6 <- str_replace_all(text6, " \\: "," \\: ")
  text6 <- str_replace_all(text6, " \\) ","\\) ")
  text6 <- str_replace_all(text6, " \\( "," \\(")
  text6 <- str_replace_all(text6, " … ","… ")
  text6 <- str_replace_all(text6, " ' ","'")
  text6 <- str_replace_all(text6, " = ","=")
  text6 <- str_replace_all(text6, " ; ",";")
  text6 <- str_replace_all(text6, " / "," / ")
  text6 <- str_replace_all(text6, " \\[ "," \\[ ")
  text6 <- str_replace_all(text6, " \\] "," \\] ")
  text6 <- str_replace_all(text6, " \\+ "," \\+ ")
  text6 <- str_replace_all(text6, " \\’ "," \\’")
  text6 <- strsplit(text6, split = "\n")
  text6 <- text6[[1]]
  text6 <<- text6
  rr <- c()
  for(i in 1:length(text6)){
    rr <- c(rr,c(text6[i],'<br/>','<br/>'))
  }
#  rr <- rr[-c(length(rr),length(rr)-1)]
  rr <- c('<br/>',rr)
  rr <<- rr
  fin <- HTML(paste(rr,collapse= " "))
  
  b <<- 1
  i <<- 1
  ind <- texte5
  ind <- removeWords(ind,stopwords("french"))
  ind <- ind[ind != ""]
  ind <- ind[nchar(ind) >= 4]
  ind <- data.frame(ind)
  ind <- distinct(ind)
  ind <- ind[,1]
  ind <- ind[-(ind %in% texte5_titre)==F]
  ind <- ind[(ind %in% tolower(texte5_titre))==F]
  ind <<- ind
  
  u <<- c()
  
  fin <<- fin
}  

i <- 1
b <- 1
ind <- texte5
ind <- removeWords(ind,stopwords("french"))
ind <- ind[ind != ""]
ind <- ind[nchar(ind) >= 4]
ind <- data.frame(ind)
ind <- distinct(ind)
ind <- ind[,1]
ind <- ind[(ind %in% texte5_titre)==F]
ind <- ind[(ind %in% tolower(texte5_titre))==F]
Indice <- function(){
  ind <- removeWords(ind,stopwords("french"))
  ind <- ind[ind != ""]
  ind <- ind[nchar(ind) >= 4]
  if(length(ind)==0){
    fin_titre <- HTML(paste('<br/>',"Plus aucun indice disponible ☹️",'<br/>','<br/>',texte4_titre,collapse = " "))
    fin_titre
  }else{
    if(b >= 10){ # i <= 10
      has <- sample(1:length(ind),1)
      indi <- ind[has]
      fin_titre <- HTML(paste('<br/>',paste0("Indice n°",i),":",indi,'<br/>','<br/>',texte4_titre,collapse = " "))
      i <<- i + 1
      b <<- 1
      ind <- ind[-has]
      ind <<- ind
      fin_titre
    }else{
      fin_titre
    }
  }
}


NouvellePtitre <- function(){
  fin_titre
}

NouvellePtexte <- function(){
  fin
}

jscode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#submit").click();
}});'



ui <- navbarPage("Textatrou",

                 tabPanel("Jouer",
                          headerPanel(""),
                          useShinyjs(),
                          tags$style(HTML(".navbar-default .navbar-brand {color: white;}")),
                          tags$style(HTML('.navbar-static-top {background-color: rgb(174,8,1);}')),
                          tags$style(HTML(".navbar-default .navbar-nav > .active > a,
                                            .navbar-default .navbar-nav > .active > a:focus,
                                            .navbar-default .navbar-nav > .active > a:hover {color: white ;background-color: rgb(124,6,1);}")),
                          tags$style(HTML(".navbar-default .navbar-nav > li > a[data-value='Jouer'] {color: white;}
                                            .navbar-default .navbar-nav > li > a[data-value='Aide'] {color: white;}
                                            .navbar-default .navbar-nav > li > a[data-value='Informations'] {color: white;}")),
                          

                          fluidRow( 
                            
                            HTML('<center><img src="textatrou.png" width="270" height="130"></center>')
                            
                            ),
                          
                          fluidRow(
                            
                            column(1," "),

                            column(2,
                                   tags$h5("Débloquer un indice"),
                                   tags$head(tags$style(HTML("#submit5{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit5", label = "Indice")),
                                   br()),
                            
                            column(3,
                                   tags$h5("Abandonner et afficher le texte"),
                                   tags$head(tags$style(HTML("#submit3{ background-color: rgb(124,6,1); color: white}"))),
                                   actionButton("submit3", label = "J'abandonne"),
                                   br()),
                            
                            column(1," "),
                            
                            column(2,
                                   tags$h5("Afficher le texte"),
                                   tags$head(tags$style(HTML("#submit4{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit4", label = "Voir le texte")),
                                   br()),
                            
                            column(2,
                                   tags$h5("Lancer une nouvelle partie"),
                                   tags$head(tags$style(HTML("#submit2{ background-color: rgb(124,6,1); color: white}"))),
                                   actionButton("submit2", label = "Nouvelle Partie"),
                                   br()),
                            
                            column(1," ")
                            
                            
                          
                          ),
                          
                          
                          mainPanel(
                            br(),
                            br(),
                            #textInput("user_text", label = " ", placeholder = "Entrer un mot :"),
                            tags$head(tags$script(HTML(jscode))),
                            tagList(
                              tagAppendAttributes(textInput("user_text", label = " ", placeholder = "Entrer un mot :"),`data-proxy-click` = "submit"
                              ),
                               tags$head(tags$style(HTML("#submit{ background-color: rgb(124,6,1); color: white}"))),
                               actionButton("submit", label = "Envoyer"),
                            ),
                            # tags$head(tags$style(HTML("#submit{ background-color: rgb(124,6,1); color: white}"))),
                            # actionButton("submit", label = "Envoyer"),
                            tags$head(tags$style(HTML("#text2{ color: rgb(38,65,71); white-space : normal; hyphens: auto; font-size: 22px; font-weight: bold}"))),
                            htmlOutput("text2"),
                            tags$head(tags$style(HTML("#text{ color: rgb(38,65,71); white-space : normal; hyphens: auto; font-size: 17px}"))),
                            htmlOutput("text")
                          , width = 12)
                 ),
                 
                 tabPanel("Aide",
                          headerPanel(""),
                          useShinyjs(),
                          tags$style(HTML(".navbar-default .navbar-brand {color: white;}")),
                          tags$style(HTML('.navbar-static-top {background-color: rgb(174,8,1);}')),
                          tags$style(HTML(".navbar-default .navbar-nav > .active > a,
                                            .navbar-default .navbar-nav > .active > a:focus,
                                            .navbar-default .navbar-nav > .active > a:hover {color: white ;background-color: rgb(124,6,1);}")),
                          tags$style(HTML(".navbar-default .navbar-nav > li > a[data-value='Jouer'] {color: white;}
                                            .navbar-default .navbar-nav > li > a[data-value='Aide'] {color: white;}
                                            .navbar-default .navbar-nav > li > a[data-value='Informations'] {color: white;}")),
                          
                          fluidRow( 
                            
                            HTML('<center><img src="textatrou.png" width="270" height="130"></center>')
                            
                          ),
                          
                          fluidRow(
                            
                            column(1," "),
                            
                            column(2,
                                   tags$h5("Débloquer un indice"),
                                   tags$head(tags$style(HTML("#submit5{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit5", label = "Indice")),
                                   br()),
                            
                            column(3,
                                   tags$h5("Abandonner et afficher le texte"),
                                   tags$head(tags$style(HTML("#submit3{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit3", label = "J'abandonne")),
                                   br()),
                            
                            column(1," "),
                            
                            column(2,
                                   tags$h5("Afficher le texte"),
                                   tags$head(tags$style(HTML("#submit4{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit4", label = "Voir le texte")),
                                   br()),
                            
                            column(2,
                                   tags$h5("Lancer une nouvelle partie"),
                                   tags$head(tags$style(HTML("#submit2{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit2", label = "Nouvelle Partie")),
                                   br()),
                            
                            column(1," ")
                            
                            
                            
                          ),
                          
                          
                          mainPanel(
                            tags$head(tags$style(HTML("#text3{ color: rgb(38,65,71); white-space : normal; hyphens: auto; font-size: 17px}"))),
                            htmlOutput("text3")
                            , width = 12)
                 ),
                 
                 tabPanel("Informations",
                          headerPanel(""),
                          useShinyjs(),
                          tags$style(HTML(".navbar-default .navbar-brand {color: white;}")),
                          tags$style(HTML('.navbar-static-top {background-color: rgb(174,8,1);}')),
                          tags$style(HTML(".navbar-default .navbar-nav > .active > a,
                                            .navbar-default .navbar-nav > .active > a:focus,
                                            .navbar-default .navbar-nav > .active > a:hover {color: white ;background-color: rgb(124,6,1);}")),
                          tags$style(HTML(".navbar-default .navbar-nav > li > a[data-value='Jouer'] {color: white;}
                                            .navbar-default .navbar-nav > li > a[data-value='Aide'] {color: white;}
                                            .navbar-default .navbar-nav > li > a[data-value='Informations'] {color: white;}")),
                          
                          fluidRow( 
                            
                            HTML('<center><img src="textatrou.png" width="270" height="130"></center>')
                            
                          ),
                          
                          fluidRow(
                            
                            column(1," "),
                            
                            column(2,
                                   tags$h5("Débloquer un indice"),
                                   tags$head(tags$style(HTML("#submit5{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit5", label = "Indice")),
                                   br()),
                            
                            column(3,
                                   tags$h5("Abandonner et afficher le texte"),
                                   tags$head(tags$style(HTML("#submit3{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit3", label = "J'abandonne")),
                                   br()),
                            
                            column(1," "),
                            
                            column(2,
                                   tags$h5("Afficher le texte"),
                                   tags$head(tags$style(HTML("#submit4{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit4", label = "Voir le texte")),
                                   br()),
                            
                            column(2,
                                   tags$h5("Lancer une nouvelle partie"),
                                   tags$head(tags$style(HTML("#submit2{ background-color: rgb(124,6,1); color: white}"))),
                                   disabled(actionButton("submit2", label = "Nouvelle Partie")),
                                   br()),
                            
                            column(1," ")
                            
                            
                            
                          ),
                          
                          
                          mainPanel(
                            tags$head(tags$style(HTML("#text4{ color: rgb(38,65,71); white-space : normal; hyphens: auto; font-size: 17px}"))),
                            htmlOutput("text4")
                            , width = 12)
                 )
                 
                 
)

server <- function(input, output, session) {
  
  texty2 <- reactiveVal(fin_titre)
  texty <- reactiveVal(fin)
  
  Jouer <- eventReactive( input$submit, {
    x <- input$user_text
    if(b >= 10){
      enable("submit5")
    }
    Play(x)
  })
  
  Jouer_titre <- eventReactive( input$submit, {
    x <- input$user_text
    Play_titre(x)
  })
  
  NouvelleP <- eventReactive(input$submit2,{
    NouvellePartie()
  })
  
  Ind <- eventReactive(input$submit5, {
    disable("submit5")
    Indice()
  })
  
  AbandonTexte <- eventReactive(input$submit3,{
    disable("submit")
    disable("submit5")
    texte_abandon <- str_replace_all(texte,"\n"," <br/> <br/> ")
    HTML(paste('<br/>',texte_abandon,'<br/>','<br/>', collapse = " "))
  })
  
  AbandonTitre <- eventReactive(input$submit3,{
    HTML(paste('<br/>',titre,'<br/>',collapse= " "))
  })
  
  ResultatTexte <- eventReactive(input$submit4,{
    texte_resultat <- str_replace_all(texte,"\n"," <br/> <br/> ")
    HTML(paste('<br/>',texte_resultat,'<br/>','<br/>', collapse = " "))
  })
  
  ResultatTitre <- eventReactive(input$submit4,{
    HTML(paste('<br/>',titre,'<br/>',collapse= " "))
  })
  
  observeEvent(input$submit2, {
    enable("submit")
    texty(NouvellePartie())
  })
  
  observeEvent(input$submit2, {
    texty2(NouvellePtitre())
    texty(NouvellePtexte())
  })
  
  
  observeEvent(input$submit3, {
    texty(AbandonTexte())
    texty2(AbandonTitre())
  })
  
  observeEvent(input$submit4, {
    texty(ResultatTexte())
    texty2(ResultatTitre())
  })

  observeEvent(input$submit, {
    texty(Jouer())
    updateTextInput(session, "user_text",  label = " ", value = "")
  })
  
  observeEvent(input$submit, {
    texty2(Jouer_titre())
    updateTextInput(session, "user_text",  label = " ", value = "")
  })
  
  observeEvent(input$submit5, {
    texty2(Ind())
  })
  
  
  output$text2 <- renderUI({
    texty2()
  })
  
  output$text <- renderUI({
    texty()
  })
  
  output$text3 <- renderUI({
    HTML(paste("<br/>",
               "<br/>",
               "<br/>",
               "<br/>",
               paste0("<span style='font-weight: 600'>","But du jeu","</span>"),
               "<br/>",
               "<br/>",
               "Le but du jeu est de découvrir l'intitulé de la page Wikipédia en entrant les mots 
               qui composent son introduction par essais successifs. <br/> Les mots corrects apparaîtront 
               en clair au fur et à mesure que vous les saisissez et lorsque vous trouverez l'intitulé exact, vous aurez gagné. <br/>
               Essayez un maximum de mots pour pouvoir débloquer des bouts de phrases qui vous servirons à trouver le thème de la page.",
               "<br/>",
               "<br/>",
               paste0("<span style='font-weight: 600'>","Indices","</span>"),
               "<br/>",
               "<br/>",
               "Pour vous aider dans votre tâche, un système d'indices à été mis en place. <br/> Un indice se débloquera pour la 
               première fois après 10 mots entrés puis tous les 10 mots à partir du moment où le bouton Indice est actionné. <br/>
               Chaque indice est un mot du texte et il a une longueur minimum de 4 lettres.",
               "<br/>",
               "<br/>",
               paste0("<span style='font-weight: 600'>","Abandon et nouvelle partie","</span>"),
               "<br/>",
               "<br/>",
               "Vous avez la possibilité d'abandonner, dans ce cas l'intitulé ainsi que le texte apparaîtront. <br/>
               Vous avez aussi la possibilité de lancer une nouvelle partie, un nouveau 
               tirage sera alors effectuer et vous pourrez rejouer et deviner l'intitulé de la nouvelle page Wikipédia.",
               "<br/>",
               "<br/>"
               ))
  })
  
  output$text4 <- renderUI({
    HTML(paste("<br/>",
               "<br/>",
               "<br/>",
               "<br/>", 
               paste0("<span style='font-weight: 600'>","Informations complémentaires","</span>"),
               "<br/>", 
               "<br/>", 
               "À chaque partie, une page Wikipédia parmi la liste des 500 pages les plus consultées en France au mois 
               dernier est sélectionnée. <br/> Cette liste est mise à jour chaque mois par un bot directement sur Wikipédia
               et de ce fait, le jeu se met à jour lui même à chaque début de mois.",
               "<br/>",
               "<br/>",
               paste0("<span style='font-weight: 600'>","liens utiles :","</span>"),
               "<br/>",
               "<br/>",
               "Liste des 500 pages les plus consultées en France au mois dernier : 
               https://fr.wikipedia.org/wiki/Projet:Wikipédia_1.0/Les_plus_consultés/Pages_populaires <br/> Période actuelle :",
               str_to_title(months(as.Date(format(Sys.Date(), '%Y-%m-01')) - 1)),format(Sys.time(), "%Y"),
               "<br/>",
               "Mon GitHub : https://github.com/idirsadaoui",
               "<br/>", 
               "<br/>", 
               "<br/>", 
               "2022"
               ))
  })

}


shinyApp(ui = ui, server = server)