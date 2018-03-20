library(shiny)
library(shinySignals)
library(dplyr)
library(bubbles)
library(stringr)
library(quanteda)
library(readtext)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(lubridate)
library(stylo)
library(tidytext)
library(tm)
#library(wordcloud)
library(xlsx)
library(gdata)
library(readxl)
library(htmlwidgets)
library(httr)
library(devtools)
library(dplyr)
library(gdata)
library(ggplot2)
library(ggraph)
library(htmlwidgets)
library(httr)
library(igraph)
library(jpeg)
library(lubridate)
library(ngram)
library(plotrix)
library(quanteda)
library(readtext)
library(readxl)
library(reshape2)
library(Rfacebook)
library(RWeka)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinySignals)
library(stringr)
library(stylo)
library(tidyr)
library(tidytext)
library(tm)
library(tokenizers)
library(xlsx)
source("bloomfilter.R")

corpositivo <- "#20B2AA";
cornegativo <- "#c00000";
corneutro <- "#FFA500";

getFBID <- function(fburl){
   return(unlist(strsplit(httr::POST(url='https://findmyfbid.com',body=list(url = fburl), encode="json")$headers$`amp-redirect-to`,'/'))[5])
}

outputDir <- "/home/ubuntu/auxfiles/outputs"

reactions_timeseries_filename <- "reactions.xlsx"
comments_timeseries_filename <- "comments.xlsx"

saveData <- function(data,names,prefix,sufix) {
   #   data <- t(data)
   # Create a unique file name
   fileName <- sprintf("%s_%s.csv", prefix, sufix)
   # Write the file to the local system
   write.csv(
      x = data,
      file = file.path(outputDir, fileName), 
      row.names = FALSE, quote = TRUE
   )
}

loadData <- function() {
   # Read all the files into a list
   files <- list.files(outputDir, full.names = TRUE)
   data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
   # Concatenate all data together into one data.frame
   data <- do.call(rbind, data)
   data
}

badwords<-c("boa","scontent.xx.fbcdn.net","https","oh","oe","pra","v","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","mais","com","está","por","uma","tem","vai","pelo","meu","sobre","não","já","nos","sem","quando","xed","xbd","ser","xbe","xa0","x8f","xb9","xb2","xb0","xb1","xb8","x8c","xa3","xbc","xaa","www.youtube.com","scontent.xx.fbcdn.net","https","oh","oe","pra","v","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","é","sr","senhor","comentário","perfil","r","que","nao","sim","comentário","feito","comentario","imagem","secretaria","foi","photos","http","bit.ly","sou","mais","vídeo","timeline","video","er","enem","soumais","maisbahia","pmba","concurso","tres","mil","quinhentos","convocacao","convocação","convoca","500","habilitados","chama","convocados","PM","aprovados","Concurso","chamados","foram","serão","serao","pmba","etapas","restantes","elimintatórias","eliminatorias","convocou","apenas","essa","bata","tres","dois","conta","eliminatorias","eliminatórias","a0","b1","8f","governador","rui","costa")

getDFMatrix <- function(text){
   text <- removeWords(text,badwords)
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   text <- stringi::stri_trans_general(text, "latin-ascii")
   myCorpus <- corpus(text)
   mydfm <- dfm(myCorpus, remove = badwords, remove_punct = TRUE, remove_numbers= TRUE)
   return(mydfm)
}

getUnigram <- function(text){
   text <- removeWords(text,badwords)
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   text <- stringi::stri_trans_general(text, "latin-ascii")
   unigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 1L, n_min = 1L, simplify = TRUE)))
   
   unigram$words[which(!is.na(str_extract(unigram$words,"comentario")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"timeline")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"video")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"photos")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"conta")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"eliminatoria")))] <- NA
   unigram$words[which(!is.na(str_extract(unigram$words,"eliminatória")))] <- NA
   
   return(unigram)
}

getBigram <- function(text){
   text <- removeWords(text,badwords)
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   text <- stringi::stri_trans_general(text, "latin-ascii")
   bigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 2L, n_min = 2L, simplify = TRUE)))
   
   bigram$words[which(!is.na(str_extract(bigram$words,"conta")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"essa")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"bata")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"tres")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"dois")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"apenas")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"comentario")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"timeline")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"video")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"photos")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"sou")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"mais")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"2vx9vf4")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"hidrica")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"barragem")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"www.facebook")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"www.boatos.org")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"etapas")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"chamados")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"chamadas")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"mil")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"eliminatórias")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"eliminatorias")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"convocou")))] <- NA
   bigram$words[which(!is.na(str_extract(bigram$words,"chamou")))] <- NA
   
   
   return(bigram)
}


getTrigram <- function(text){
   text <- removeWords(text,badwords)
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   text <- stringi::stri_trans_general(text, "latin-ascii")
   trigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 3L, n_min = 3L, simplify = TRUE)))
   
   trigram$words[which(!is.na(str_extract(trigram$words,"conta")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"essa")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"bata")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"tres")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"dois")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"apenas")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"comentario")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"timeline")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"video")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"photos")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"sou")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"mais")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"2vx9vf4")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"hidrica")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"barragem")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"www.facebook")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"www.boatos.org")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"etapas")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"chamados")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"chamadas")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"mil")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatórias")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatorias")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatoria")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"eliminatória")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"convocou")))] <- NA
   trigram$words[which(!is.na(str_extract(trigram$words,"chamou")))] <- NA
   
   
   return(trigram)
}

