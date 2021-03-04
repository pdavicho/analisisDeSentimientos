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





