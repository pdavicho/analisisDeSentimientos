#Estadistica Básica
#06 - Graficos
#http://www.r-graph-gallery.com
#Definiendo la carpeta de trabajo
setwd("C:/Users/pablo/Desktop/Pablo David/ITSRumiñahui/AnalisisDeSentimientos/UntitledFolder/analisisV3.0")
getwd()

############## Instalacion de librerias
install.packages("lubridate")
install.packages("dplyr")
install.packages("readr")
install.packages("tidytext")
install.packages("stringr")

######### Librerias ###############
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
####################################
######### DATOS ####################
tweets_lasso <- read_csv("TweetLasso.csv")
tweets_yaku <- read_csv("TweetYaku.csv")

View(tweets_lasso)
str(tweets_lasso)

tweets <- bind_rows(tweets_lasso %>%
                      mutate(person = "Lasso"),
                    tweets_yaku %>%
                      mutate(person = "Yaku")) 
View(tweets)

figura <- ggplot(tweets, aes(x=tweets$FechaFormato, fill = person)) +
  geom_bar(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)

figura + ggtitle("Conteo de Twitters Lasso - Yaku") +
  xlab("Fecha") + ylab("Conteo")

################ Word Frequencys ###########################
library(tidytext)
library(stringr)



remove_reg <- "&amp;|&lt;|&gt;"
text = toString(tweets$Tweet)
tidy_tweets <- tweets %>%
  filter(!str_detect(tweets$Tweet, "^RT")) %>%
  mutate(text = str_remove_all(toString(tweets$Tweet), remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets")%>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))
View(tidy_tweets)
#######################NO FUNCIONO################################

tweet_text <- as.character(tweets$Tweet)
View(tweet_text)
install.packages("tm")
library(tm)
#TM contain corpus is a way to store a collection of documents in readable format
tweet_text_corpus <- Corpus(VectorSource(tweet_text))
#Remover puntuacion
tweet_text_corpus <- tm_map(tweet_text_corpus, removePunctuation)
#Transformar lower case
tweet_text_corpus <- tm_map(tweet_text_corpus, content_transformer(tolower))
# Remover stopwords spanish
tweet_text_corpus <- tm_map(tweet_text_corpus, removeWords, stopwords("spanish"))
#Eliminar espacio extra
tweet_text_corpus <- tm_map(tweet_text_corpus, stripWhitespace)
#Eliminar RT
tweet_text_corpus <- tm_map(tweet_text_corpus, removeWords, c("RT"))
#Remover URL
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
tweet_text_corpus <- tm_map(tweet_text_corpus, content_transformer(removeURL))

#Creando documento 
docTweet <- TermDocumentMatrix(tweet_text_corpus)
docTweet <- as.matrix(docTweet)
docTweet <- sort(rowSums(docTweet), decreasing = TRUE)

#Convirtiendo en df
docTweet <- data.frame(word = names(docTweet), freq=docTweet)
View(docTweet)

#Plot de Word frecuencia de las 10 mas usadas
word = docTweet$word[1:10]
conteo = docTweet$freq[1:10]
par(mar = rep(2, 4))
barplot(conteo, las=2, names.arg = word, col = "yellow",
        main = "Palabras mas frecuentes", 
        ylab = "Frecuencia de palabras")

dev.off()

#Nube de palabras
install.packages("wordcloud2")
library(wordcloud2)

wordCloud <- wordcloud2(data = docTweet, minRotation=0, maxRotation=0, ellipticity=0.6)
wordCloud






