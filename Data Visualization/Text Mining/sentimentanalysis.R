# sentiment analysis #
# make a copy 
senti_set <- words_set 
# 9699 obs. of  4 variables
str(senti_set)

########################################################################################################################
# tidytext package has sentiment lexicons built in
# nrc lexicon contains words representing the following wider range of sentiments
# 13,872 observations
emotions <- get_sentiments("nrc")
# bing lexicon contains words coded as positive/negative
# 6,786 observations
posvsnegs <- get_sentiments("bing")
# afinn lexicon contains words on a scale from -5 to 5
# 2,477 observations
sentiscale <- get_sentiments("afinn")

########################################################################################################################
# add emotion
# 3,097 obs and 3 vars
words_emotion <- inner_join(get_sentiments("nrc"), senti_set, by = "word") %>%
  count(word, sentiment, album_name, sort = TRUE) %>%
  # because i will be using "bing" for pos/neg
  filter(sentiment != "positive") %>%
  filter(sentiment != "negative") %>%
  group_by(album_name) %>%
  ungroup()

gg5 <- ggplot(words_emotion, aes(n, sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 2, scales = "free_x") +
  labs(
    x = "Count", 
    y = "Sentiment", 
    title = "Distribution of Emotions Across All Taylor Swift Albums"
  ) +
  theme(plot.title = element_text(hjust = 0.4, face = "bold"))

ggsave("gg5.png", plot = gg5, width = 8, height = 10)


########################################################################################################################
# make pos-neg cloud
# 1407 obs and 3 vars
words_posneg <- inner_join(get_sentiments("bing"), senti_set, by = "word") %>%
  count(word, sentiment, album_name, sort = TRUE) %>%
  distinct()

words_posneg %>%
acast(word ~ sentiment, value.var = "n", fill = 0) %>%
comparison.cloud(max.words = 300, scale=c(1.5,0.6),
                 colors = c("darkred","darkslateblue"), 
                 random.order = FALSE, rot.per = .1,
                 use.r.layout = FALSE, title.size = 3,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey70",
                 fun.aggregate = NULL) 

########################################################################################################################
########################################################################################################################
########################################################################################################################
# sentiment analysis, CHECKING: add pos/neg, emotion, value to each word

# 4,598 obs and 3 vars
emotioncheck1 <- right_join(get_sentiments("nrc"), words_setf, by = "word") %>%
  count(word, sentiment, sort = TRUE)
# 1,336 obs and 3 vars
emotioncheck2 <- emotioncheck1 %>%
  filter(!is.na(sentiment)) %>%
  # because i will be using "bing" for pos/neg
  filter(sentiment != "positive") %>%
  filter(sentiment != "negative")
# 8 distinct: anticipation, joy, sadness, surprise, anger, disgust, fear, trust
distinct(emotioncheck2, sentiment)