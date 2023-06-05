# install packages #
install.packages("rio")
install.packages("janitor")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("stm")
install.packages("quanteda")
install.packages("wordcloud")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("geometry")
install.packages("Rtsne")
install.packages("rsvd")
install.packages("syuzhet")
install.packages("scales")
install.packages("wordcloud2")
install.packages("dplyr")
install.packages("textdata")
install.packages("tidyr")
install.packages("igraph")
install.packages("ggraph")

# load packages #
library(rio)
library(janitor)
library(tidytext)
library(tidyverse)
library(stm)
library(quanteda)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(geometry)
library(Rtsne)
library(rsvd)
library(syuzhet)
library(scales)
library(wordcloud2)
library(dplyr)
library(textdata)
library(tidyr)
library(igraph)
library(ggraph)

########################################################################################################################

# import data set #
raw_lyrics <- import(here::here("raw data", "taylorswiftlyrics.csv"))

# standardizes column names
clean_lyrics <- raw_lyrics %>% 
  clean_names()

# 187 obs and 4 vars
str(clean_lyrics)

########################################################################################################################

# data cleaning: remove stopwords #
# output column = word, input column = lyrics
raw_words <- clean_lyrics %>% 
  unnest_tokens(word, lyrics) %>%
  # call in stop_words data from tidyverse package
  anti_join(stop_words, by=c("word" = "word")) %>%
  # delete duplicates
  distinct()

# 10092 obs, 4 vars
str(raw_words)

########################################################################################################################

# put all lyrics in lower case: worked?? because exported list had TRUE and FLASE intact
clean_words <- raw_words %>% 
  mutate(word = tolower(word))

########################################################################################################################

# look at the most common words in the lyrics to see if you need to manually remove anymore stopwords
# Look at the most popular words
export_words <- clean_words %>%
  count(word) %>%
  as.data.frame()

# 3304 obs and 2 vars
str(export_words)
export(export_words, "export_words.csv")

########################################################################################################################

# manual exclusion of more stopwords
clean_words <- clean_words %>% 
  filter(word != "1") %>% 
  filter(word != "15") %>% 
  filter(word != "16") %>% 
  filter(word != "16th") %>% 
  filter(word != "17") %>%
  filter(word != "18") %>%
  filter(word != "1950s") %>%
  filter(word != "2") %>%
  filter(word != "2,190") %>%
  filter(word != "20") %>% 
  filter(word != "2003") %>% 
  filter(word != "22") %>% 
  filter(word != "25") %>%
  filter(word != "29th") %>%
  filter(word != "3") %>%
  filter(word != "32") %>%
  filter(word != "4") %>%
  filter(word != "45") %>%
  filter(word != "4am") %>%
  filter(word != "4th") %>%
  filter(word != "5") %>%
  filter(word != "58") %>%
  filter(word != "7") %>%
  filter(word != "90") %>%
  filter(word != "9th") %>%
  filter(word != "a.m") %>%
  filter(word != "ah") %>%
  filter(word != "ayy") %>%
  filter(word != "eh") %>%
  filter(word != "em") %>%
  filter(word != "g5") %>%
  filter(word != "ha") %>%
  filter(word != "hmm") %>%
  filter(word != "ho") %>%
  filter(word != "hoo") %>%
  filter(word != "huh") %>%
  filter(word != "ing") %>%
  filter(word != "la") %>%
  filter(word != "mhmmm") %>%
  filter(word != "mm") %>%
  filter(word != "mmh") %>%
  filter(word != "mmm") %>%
  filter(word != "mmmm") %>%
  filter(word != "na") %>%
  filter(word != "ooh") %>%
  filter(word != "oooh") %>%
  filter(word != "ooooh") %>%
  filter(word != "st") %>%
  filter(word != "ta") %>%
  filter(word != "tis") %>%
  filter(word != "uh") %>%
  filter(word != "ve") %>%
  filter(word != "whoa") %>%
  filter(word != "woo") %>%
  filter(word != "ya") %>%
  filter(word != "yeah") %>%
  filter(word != "baby") %>%
  filter(word != "wanna") %>% 
  filter(word != "gonna") %>% 
  filter(word != "til") %>%
  filter(word != "front")

########################################################################################################################

# 9699 obs and 4 vars
str(clean_words)

