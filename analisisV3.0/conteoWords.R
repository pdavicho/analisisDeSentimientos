#Definiendo la carpeta de trabajo
setwd("C:/Users/pablo/Desktop/Pablo David/ITSRumiñahui/AnalisisDeSentimientos/UntitledFolder/analisisV3.0")
getwd()

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
txtDF <- sapply(tweets, as.character)
str(txtDF)

library(tidytext)
library(stringr)

remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- tweets %>%
  filter(!str_detect(Tweet, "^RT")) %>%
  mutate(text = str_remove_all(Tweet, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

View(tidy_tweets)

frequency <- tidy_tweets %>%
  group_by(person) %>%
  count(word, sort = TRUE) %>%
  left_join(tidy_tweets %>%
              group_by(person) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

install.packages("tidyr")
library(tidyr)

frequency <- frequency %>%
  select(person, word, freq) %>%
  spread(person, freq) %>%
  arrange(Lasso, Yaku)

frequency

install.packages("scales")
library(scales)
ggplot(frequency, aes(Lasso, Yaku)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25)+
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  geom_abline(color = "red")

#################### COMPARANDO EL USO DE WORDS ########################
word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Lasso / Yaku)) %>%
  arrange(desc(logratio))

word_ratios %>%
  arrange(abs(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Log odds ratio (Lasso/Yaku)") +
  scale_fill_discrete(name = "", labels = c("Lasoo", "Yaku"))

##### Cambio en el uso de WORDS ##################
View(tidy_tweets)

words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(FechaFormato, person, word) %>%
  group_by(person, FechaFormato) %>%
  mutate(time_total = sum(n)) %>%
  group_by(person, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_by_time

nested_data <- words_by_time %>%
  nest(-word, -person)

words_by_time

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ FechaFormato,
                                  ., family = "binomial")))

nested_models












