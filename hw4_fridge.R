library(readxl)
library(tidytext)
library(tidyverse)

# Download the data, break Prescription columns into separate variables
fridge = read_excel("./data/Fridge.xlsx")
p1 = fridge %>% select('Person', 'Prescription 1') %>% rename(txt = 'Prescription 1')
p2 = fridge %>% select('Person', 'Prescription 2') %>% rename(txt = 'Prescription 2')
p3 = fridge %>% select('Person', 'Prescription 3') %>% rename(txt = 'Prescription 3')

#Load the dataset of stop words, remove them from our data
data('stop_words')

# Compose one-column variable, sort by person, remove NAs, break into words, remove stop words
fridge_presc = bind_rows(p1, p2, p3) %>% arrange(Person) %>% filter(txt != 'NA')
words_presc = fridge_presc %>% unnest_tokens(word, 'txt') %>% anti_join(stop_words)
words_presc %>% count(word, sort = T)