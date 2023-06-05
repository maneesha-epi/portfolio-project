# basic analysis #
# make a copy: 
words_set <- clean_words
# 9699 obs. of  4 variables
str(words_set)
head(words_set)

########################################################################################################################

# create a new table with "top 25 words"
freqtable1 <- words_set %>% 
  count(word) %>% 
  filter(n >= 25) %>%
  arrange(desc(n))

# 30 obs and 2 vars
str(freqtable1)

# make a plot
freqgraph <- ggplot(freqtable1, aes(word, n)) +
  geom_col(fill = "deeppink2") +
  coord_flip() +
  labs(
    x = "Word", 
    y = "Count", 
    title = "25 Most Commmon Words in Taylor Swift Songs"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# save plot 
ggsave("freqgraph.png", plot = freqgraph, width = 8, height = 10)

########################################################################################################################

# create a new table with top words starting at 13 count (cause it's tay tay's number)
wordcloud1 <- words_set %>% 
  count(word) %>% 
  arrange(desc(n))

# 3304 obs and 2 vars
str(wordcloud1)

# plotting word cloud: vfont=c("serif","bold")
wordcloud(
  # Assign the word column to words
  words = wordcloud1$word, freq = wordcloud1$n, 
  scale = c(4,.3),min.freq=13, max.words = 800, random.order = FALSE,
  random.color = FALSE, rot.per = .1,
  colors = c("darkorchid1", "darkorchid2", "darkorchid3", "darkorchid4"), 
  ordered.colors = FALSE, use.r.layout = FALSE, fixed.asp = TRUE)

########################################################################################################################
# how top 5 words changed over time
wordsby_year <- words_set %>%
  group_by(album_year) %>%
  count(word) %>%
  filter(word %in% c("time", "love", "night", "eyes", "home")) 

yeargraph <- wordsby_year %>%
  ggplot(aes(x = album_year, y = n, colour = word)) +
  geom_line() +
  guides(color = guide_legend(title = "Top 5 Words")) +
  labs(
    x = "Year of Album", 
    y = "Count", 
    title = "Use of the Top 5 Words Over Time") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size=10)) 
  
ggsave("yeargraph.png", width = 5, height = 4, plot = yeargraph)

########################################################################################################################

