library(sparklyr)
library(readxl)
library(tidytext)
library(tidyverse)

# Create a local connection in Spark
sc = spark_connect(master = 'local')
class(sc)

# Load data locally from csv
reviews = read_csv("C:/Studies/big_data/7282_1.csv")

# Select only text and ratings from the dataframe, remove NAs
reviews_text = reviews %>% select(reviews.text, reviews.rating) %>% 
  rename(text = reviews.text, rating = reviews.rating) %>% 
  filter(text != 'NA' & rating != 'NA' & rating <= 5)

# Copy the edited table to Spark
reviews_text_spark = copy_to(sc, name = 'reviews_text', reviews_text)

# Create a dataframe of mean ratings for words
word_ratings = reviews_text %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  group_by(word) %>% summarize_all(funs(mean, n()))

# Copy another edited table to Spark
word_ratings_spark = copy_to(sc, name = 'word_ratings', word_ratings)

# Create strings for the reviews
a_review = 'I enjoyed my stay: friendly staff and the breakfast was great'
b_review = 'We were disappointed on the 2-bed room that was promised to us. Had the worst sleep ever.'
c_review = 'There was no toilet paper available'

# Define a function for calculating average rate for words
rate_review = function(my_review, word_ratings){
  my_review_rated = as_tibble(my_review) %>% unnest_tokens(word, value) %>% 
    merge(word_ratings)
  my_rating = mean(my_review_rated$mean)
  return(my_rating)}

# Use the function
a_rate = rate_review(a_review, word_ratings)
b_rate = rate_review(b_review, word_ratings)
c_rate = rate_review(c_review, word_ratings)

# Define another function with weighted counts
rate_review_w = function(my_review, word_ratings){
  my_review_rated = as_tibble(my_review) %>% unnest_tokens(word, value) %>% 
    merge(word_ratings)
  my_rating = sum(my_review_rated$mean * my_review_rated$n)/sum(my_review_rated$n)
  return(my_rating)}

# Improved rates
a_rate_w = rate_review_w(a_review, word_ratings)
b_rate_w = rate_review_w(b_review, word_ratings)
c_rate_w = rate_review_w(c_review, word_ratings)