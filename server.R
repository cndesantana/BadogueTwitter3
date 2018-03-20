<<<<<<< HEAD
library(stringr)
library(quanteda)
library(readtext)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(lubridate)
library(dplyr)
library(stylo)
library(tidytext)
library(tm)
library(wordcloud)
library(xlsx)
library(gdata)
library(readxl)
library(httr)
library(twitteR)

function(input, output, session) {

   output$downloadComments<- downloadHandler(
     filename = function() {
        comments_timeseries_filename;
     },
     content = function(file) {
        api_key = "3I8JehozX8N4Bojg0qSdmDFLX"
        api_secret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
        access_token = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
        access_token_secret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"
        setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
        tweets = searchTwitter(input$term, n=input$count,lang=input$lang)
        tweets = twListToDF(tweets)
        allmessages <- tweets %>% select(created,id,screenName,text,location);
        names(allmessages) <- c("Data","Autor ID","Autor Nome","Conteúdo","Localização");
        wb<-createWorkbook(type="xlsx")
        TABLE_COLNAMES_STYLE <- CellStyle(wb) + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")
        sheet <- createSheet(wb, sheetName = "twitterdata")
        setColumnWidth(sheet, colIndex=1:length(allmessages[1,]), 25)
        addDataFrame(allmessages, sheet, colnamesStyle = TABLE_COLNAMES_STYLE, colStyle = TABLE_COLNAMES_STYLE, startColumn=1, row.names = FALSE)
        saveWorkbook(wb, file)   
     })
  

  ############## DASHBOARD - ATUALIZAÇÃO DOS DADOS VIA CLIQUE EM BOTÃO 'RUN'
  # When the dashboard button is clicked, save the form data
  observeEvent(input$twitterbtn, {
     api_key = "3I8JehozX8N4Bojg0qSdmDFLX"
     api_secret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
     access_token = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
     access_token_secret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"

     withProgress(message = 'Baixando...', value = 0, {
        incProgress(1/3, detail = paste("33%"))
        setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
        tweets = searchTwitter(input$term, n=input$count,lang=input$lang)
        tweets = twListToDF(tweets)
        tweets <- tweets %>% select(created,id,screenName,text,location);
        names(tweets) <- c("Data","Autor ID","Autor Nome","Conteúdo","Localização");
        incProgress(1/3, detail = paste("66%"))
        sufix <- paste(
           as.character(format(Sys.time(),"%d%m%Y")),
           digest(input$term),
           sep="");
        saveData(tweets,names(tweets),prefix="comments",sufix)
        incProgress(1/3, detail = paste("100%"))
     })
     
  })

 toPlot <- eventReactive(input$update, {
     runif(2);
  })
  
  ####### COMENTÁRIOS NO TEMPO
  
  output$commentsPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$term),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix))
     if(length(toPlot) == 2){
        cat("toPlot passou!",sep="\n")
        if(file.exists(comments_timeseries_filename)){
           cat("Comments_Timeseries passou!",sep="\n")
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           
           timeseries <- comments_dataframe %>% mutate(
              day = ymd_hms(Data) %>%
                 as.Date() %>%
                 format("%d"), 
              month = ymd_hms(Data) %>%
                 as.Date() %>%
                 format("%m"), 
              year = ymd_hms(Data) %>%
                 as.Date() %>%
                 format("%Y"), 
              hour = ymd_hms(Data) %>%
                 format("%H"),
              hour = as.numeric(hour) - 3,
              hour = as.character(hour), 
              min = ymd_hms(Data) %>%
                 format("%M")
           ) %>%
              mutate(date = as.POSIXct(paste(paste(day,month,year,sep="/"),paste(hour,min,sep=":")),format = "%d/%m/%Y %H:%M")) %>%
              group_by(date) %>%
              summarise(total = n())
           
           myx <- timeseries$date
           mydate <- format(as.POSIXct(myx), format="%d/%m/%Y %H:%M")
           myy <- timeseries$total
           ggplot(timeseries) + geom_line(stat = "identity", aes(x = date, y = total)) + ylab("Comentários por minuto") + xlab("Tempo")
        }
     }
  })
  
  plotComentariosTS = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$term),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        
        timeseries <- comments_dataframe %>% mutate(
           day = ymd_hms(Data) %>%
              as.Date() %>%
              format("%d"), 
           month = ymd_hms(Data) %>%
              as.Date() %>%
              format("%m"), 
           year = ymd_hms(Data) %>%
              as.Date() %>%
              format("%Y"), 
           hour = ymd_hms(Data) %>%
              format("%H"),
           hour = as.numeric(hour) - 3,
           hour = as.character(hour),
           min = ymd_hms(Data) %>%
              format("%M")
        ) %>%           
           mutate(date = as.POSIXct(paste(paste(day,month,year,sep="/"),paste(hour,min,sep=":")),format = "%d/%m/%Y %H:%M")) %>%
           group_by(date) %>%
           summarise(total = n())
        
        myx <- timeseries$date
        mydate <- format(as.POSIXct(myx), format="%d/%m/%Y %H:%M")
        myy <- timeseries$total
        ggplot(timeseries) + geom_line(stat = "identity", aes(x = date, y = total)) + ylab("Comentários por minuto") + xlab("Tempo")
        
     }
  }

  output$comentariosts = downloadHandler(
     filename = function() {
        paste("comentariosts.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = 16, height = 9,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotComentariosTS(), device = device)
     }     
  )
  
  
  
  ####### WORDCLOUDS
  
  plotWordcloudTS = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$term),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        
        text <- as.character(comments_dataframe$Conteúdo)
        mydfm <- getDFMatrix(text);
        set.seed(100)
        textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(8,"Dark2"))     
     }
  }  
  
  output$wordcloudPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$term),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(length(toPlot) > 0){
        if(file.exists(comments_timeseries_filename)){
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           
           text <- as.character(comments_dataframe$Conteúdo)
           mydfm <- getDFMatrix(text);
           set.seed(100)
           textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                              rot.per = .25, 
                              colors = RColorBrewer::brewer.pal(8,"Dark2"))
        }
     }
  })
  
  output$wordcloudts = downloadHandler(
     filename = function() {
        paste("wordcloudts.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotWordcloudTS(), device = device)
     }     
  )
  
  ###### UNIGRAM

  output$unigramaPlot <- renderPlot({
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$term),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(length(toPlot) > 0){
        if(file.exists(comments_timeseries_filename)){
           comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
           text <- as.character(comments_dataframe$Conteúdo)
           
           unigram <- getUnigram(text)
           unigram %>% 
              filter(!is.na(words)) %>% 
              select(words) %>% group_by(words) %>% 
              summarise(total = n()) %>% 
              arrange(total) %>% tail(20) %>% 
              ggplot(aes(reorder(words,total), total)) +
              geom_bar(stat = "identity") + 
              xlab("Palavras") + ylab("Frequência") +
              ggtitle("Palavras mais frequentes") +
              geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
              coord_flip()
        }
     }
  })
  
  plotUnigrama = function(){
     sufix <- paste(
        as.character(format(Sys.time(),"%d%m%Y")),
        digest(input$term),
        sep="");
     prefix <- "comments";
     comments_timeseries_filename <- file.path(outputDir,sprintf("%s_%s.csv", prefix, sufix)) 
     if(file.exists(comments_timeseries_filename)){
        comments_dataframe <- read.csv(comments_timeseries_filename,sep=",")
        text <- as.character(comments_dataframe$Conteúdo)
        
        unigram <- getUnigram(text)
        unigram %>% 
           filter(!is.na(words)) %>% 
           select(words) %>% group_by(words) %>% 
           summarise(total = n()) %>% 
           arrange(total) %>% tail(20) %>% 
           ggplot(aes(reorder(words,total), total)) +
           geom_bar(stat = "identity") + 
           xlab("Palavras") + ylab("Frequência") +
           ggtitle("Palavras mais frequentes") +
           geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
           coord_flip()
     }
  }
  
  output$unigrama = downloadHandler(
     filename = function() {
        paste("unigrama.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotUnigrama(), device = device)
     }     
  )

}


=======

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(quanteda)
library(tokenizers)
library(stringr)
badwords <- c("httpstconolejeofew","httpstcoe70pmemtDI","httpstcoB1LRIe9w9g","httpstcooVyWPTCypu","httpstco2BLB0m0v1U")
getDFMatrix <- function(text){
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   tokenInfo <- summary(myCorpus)
   myStemMat <- dfm(myCorpus, remove = stopwords("portuguese"), stem = TRUE, remove_punct = TRUE)
   mydfm <- dfm(myCorpus, remove = c(stopwords("portuguese"),badwords), remove_punct = TRUE, remove_numbers= TRUE)
   return(mydfm)
}

getUnigram <- function(text){
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- stringi::stri_trans_general(text, "latin-ascii")
   text <- removeWords(text,badwords)
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   unigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 2L, n_min = 1L, simplify = TRUE)))
   unigram$words[which(!is.na(str_extract(unigram$words,"httpstconolejeofew")))] <- NA

   return(unigram)
}

library(shiny)
library(twitteR)
library(wordcloud)
library(tm)
shinyServer(function (input, output) {
   rawData = reactive(function(){
#      tweets = searchTwitter(input$term, n=input$count, lang=input$lang)
      tweets = searchTwitter(input$term, n=input$count)
      tweets = twListToDF(tweets)
   })

   output$wordcl = renderPlot(function(){
      tw.text = enc2native(rawData()$text)
      tw.text = tolower(tw.text)
      tw.text = removeWords(tw.text,c(stopwords(input$lang),"rt"))
      tw.text = removePunctuation(tw.text,TRUE)
      tw.text = unlist(strsplit(tw.text," "))
      mydfm <- getDFMatrix(tw.text);
      set.seed(100)
      textplot_wordcloud(mydfm, min.freq = 2, random.order = FALSE,
                         rot.per = .25, 
                         colors = RColorBrewer::brewer.pal(8,"Dark2"))
   })
   
   output$plotlocal = renderPlot(function(){
      tweets %>% 
         group_by(location) %>% 
         summarise(audiencia = n()) %>% 
         arrange(audiencia) %>% 
         tail(30) %>% 
         ggplot(aes(x = reorder(location, as.numeric(audiencia)), 
                    y = as.numeric(audiencia))) + 
         geom_bar(stat="identity") + 
         geom_text(aes(x = reorder(location, as.numeric(audiencia)), 
                       y = as.numeric(audiencia), 
                       label = as.numeric(audiencia)), 
                   vjust = 0, hjust = 0, size = 2 ) + 
         xlab("Localização") + ylab("Audiência no Twitter") + 
         coord_flip()
   })
   
   output$plotautor = renderPlot(function(){
      tweets %>% 
         group_by(screenName) %>% 
         summarise(audiencia = n()) %>% 
         arrange(audiencia) %>% 
         tail(30) %>% 
         ggplot(aes(x = reorder(screenName, as.numeric(audiencia)), 
                    y = audiencia)) + 
         geom_bar(stat="identity") + 
         geom_text(aes(x = reorder(screenName, as.numeric(audiencia)), 
                       y = as.numeric(audiencia), 
                       label = as.numeric(audiencia)), 
                   vjust = 0, hjust = 0, size = 2 )
         xlab("Autor") + ylab("Audiência no Twitter") + 
         coord_flip()
   })
   
   output$plotunigram = renderPlot(function(){
      tw.text = enc2native(rawData()$text)
      unigram <- getUnigram(tw.text)
      unigram %>% 
         filter(!is.na(words)) %>% 
         select(words) %>% group_by(words) %>% 
         summarise(total = n()) %>% 
         arrange(total) %>% tail(20) %>% 
         ggplot(aes(reorder(words,total), total)) +
         geom_bar(stat = "identity") + 
         xlab("Palavras") + ylab("Frequência") +
         ggtitle("Palavras mais frequentes") +
         geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   })      
   
   
})
>>>>>>> f5abfe4798136b0e9e068b2dbd91443eb6bba457
