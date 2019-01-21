library(readxl)
library(tidytext)
library(tidyverse)

reviews = read_csv("./data/7282_1.csv")

reviews_ny = reviews %>% filter(city == 'New York') %>% 
  select(reviews.text) %>% rename(txt = reviews.text) %>% filter(txt != 'NA')

# Compose one-column variable, remove NAs, break into words, remove stop words
words_ny = reviews_ny %>% unnest_tokens(word, 'txt') %>% anti_join(stop_words)
words_ny %>% count(word, sort = T)

# Break into sentences
all_sentences = reviews %>% select(reviews.text) %>% rename(txt = reviews.text) %>% 
  filter(txt != 'NA') %>% unnest_tokens(sentence, txt, token = 'sentences')
all_sentences %>% count(sentence, sort = T)

# Count all positive words in NY hotel reviews, count all words included in sentiment analysis, 
# Find the proportion
n_positive = words_ny %>% inner_join(get_sentiments('bing')) %>% 
  filter(sentiment == 'positive') %>% nrow()
n_all = words_ny %>% inner_join(get_sentiments('bing')) %>% nrow()
positive_pct = n_positive / n_all