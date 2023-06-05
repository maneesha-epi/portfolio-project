# bi-gram analysis #
# make a copy: 
bigrams_set <- clean_lyrics

########################################################################################################################

# create a bi-gram set
bigrams_set <- bigrams_set %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)
# 68537 obs. of  4 variables
str(bigrams_set)

# separate bi-grams into 2 columns 
bigrams_separate <- bigrams_set %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# get rid of stopwords & any other words in the bi-grams
bigrams_clean <- bigrams_separate %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% c("ah", "na", "ayy", "eh", "em", "ha", "hmm", "ho", "hoo", "huh", "ing", "la", "mhmmm",
                       "mm", "mmh", "mmm", "mmmm", "ooh", "oooh", "ooooh", "st", "ta", "uh", "tis", "ve", 
                       "whoa", "woo", "ya", "yeah", "til", "1", "15", "16", "16th", "17", "18", "1950s", "street",
                       "2", "2,190", "20", "2003", "22", "25", "29th", "3", "32", "4", "45", "4am", "4th", "cornelia",
                       "5", "58", "7", "90", "9th", "a.m", "tryna", "till", "hey", "christmas")) %>%
  filter(!word2 %in% c("ah", "na", "ayy", "eh", "em", "ha", "hmm", "ho", "hoo", "huh", "ing", "la", "mhmmm",
                       "mm", "mmh", "mmm", "mmmm", "ooh", "oooh", "ooooh", "st", "ta", "uh", "tis", "ve", 
                       "whoa", "woo", "ya", "yeah", "til", "1", "15", "16", "16th", "17", "18", "1950s", "street",
                       "2", "2,190", "20", "2003", "22", "25", "29th", "3", "32", "4", "45", "4am", "4th", "cornelia",
                       "5", "58", "7", "90", "9th", "a.m", "tryna", "till", "hey", "christmas")) %>%
  filter(word1 != word2)

bigrams_setf <- bigrams_clean %>%
  unite(bigram, word1, word2, sep = " ")

# 3634 obs and 5 vars
str(bigrams_setf)
########################################################################################################################

# create a new table with "top 10 bi-grams"
freqtable2 <- bigrams_setf %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2) %>%
  filter(n >= 10) %>%
  arrange(desc(n))

# simple network graph: without arrow (graphopt, fr)
bigram_graph <- freqtable2 %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'graphopt') + 
  geom_edge_link() + 
  geom_node_point(color = "blueviolet", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, vjust = 0.5, hjust = 0.5) +
  ggtitle("Top 10 Word Pairs in Taylor Swift Songs") +
  theme(plot.title = element_text(hjust = 0.4, face = "bold")) 

ggsave("bigram_graph.png", plot = bigram_graph, height = 4, width = 4)

########################################################################################################################

# check what is paired with "time" top word
freqtable3 <- bigrams_clean %>%
  filter(word1 == "time" | word2 == "time") %>%
  count(word1, word2) %>%
  filter(n > 1) %>%
  arrange(desc(n))
# simple network graph: with arrow, take out start_cap(10, 'inches') to get bidirectional arrows
timepair_graph <- ggraph(freqtable3, layout = "fr") +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)), 
                 arrow = arrow(type = "closed", length = unit(1, 'mm')), color="azure4") + 
  geom_node_point(color = "deepskyblue1") + 
  geom_node_text(aes(label = name), repel = TRUE, color = "black", hjust = 1, vjust = 2, size=3)+
  ggtitle("Network of Words Associated with Time") +
  theme(plot.title = element_text(hjust = 0.4, face = "bold")) 

ggsave("timepair_graph.png", plot = timepair_graph, height = 4.5, width = 6)

########################################################################################################################

# check what is paired with "love" top word
freqtable4 <- bigrams_clean %>%
  filter(word1 == "love" | word2 == "love") %>%
  count(word1, word2) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# simple network graph: with arrow, take out start_cap(10, 'inches') to get bidirectional arrows
lovepair_graph <- freqtable4 %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)), 
                 arrow = arrow(type = "closed", length = unit(1, 'mm')), color="azure4") + 
  geom_node_point(color = "darkred", size = 1) +
  geom_node_text(aes(label = name), repel = TRUE, hjust = 1, vjust = 1, size = 4) +
  ggtitle("Network Graph of Words Associated with Love") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave("lovepair_graph.png", plot = lovepair_graph, height = 5, width = 6)
