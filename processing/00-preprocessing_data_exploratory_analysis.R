# Exploratory data analysis

library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)

data_snt <- list()
directory_path <- "../data/raw_data"

csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

for (file in csv_files) {
  data <- read_csv(file, col_types = cols(`Tweet_Id` = col_character(), 
                                          `Reply_to` = col_character()))
  data_snt <- bind_rows(data_snt, data)
}

cercanias <- read_csv("../data/processed_data/cercanias_madrid_oficial.csv", 
                      col_types = cols(id = col_character(),
                                       conversation_id = col_character(),
                                       referenced_tweets.replied_to.id = col_character(), 
                                       in_reply_to_user_id = col_character(), 
                                       author.pinned_tweet_id = col_character()))

previous <- cercanias %>% 
  select(c(created_at, id, text, author.username, referenced_tweets.replied_to.id, entities.hashtags)) %>% 
  rename(c(Datetime = created_at, Tweet_Id = id, Text = text, Username = author.username,
           Reply_to = referenced_tweets.replied_to.id, Hashtags = entities.hashtags)) 

previous <- previous[order(previous$Datetime), ]

merged <- bind_rows(previous, data_snt)
write.csv(merged, "../data/processed_data/tweets_cercanias_madrid.csv")

## Data exploration

df <- merged %>% 
  mutate(Text = str_replace(Text, "(?<=^|[^\\w\\d])@\\w+", ""))

tokens <- df %>%
  mutate(Tweet = Text) %>% 
  unnest_tokens(word, Text)

tokens

word_counts <- tokens %>%
  count(word, sort = TRUE)

word_counts

bigrams <- tokens %>%
  mutate(next_word = lead(word)) %>%
  filter(!is.na(next_word)) %>%
  mutate(bigram = paste(word, next_word, sep = " "))

bigrams 

trigrams <- tokens %>%
  mutate(trigram = paste0(word, " ", lead(word, 1), " ", lead(word, 2))) %>%
  filter(!is.na(lead(word, 2)))

trigrams

trigrams_count <- trigrams %>%
  count(trigram, sort = TRUE)

trigrams_count

bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE)

bigram

trigrams %>% 
  filter(grepl("se están produciendo", trigram, ignore.case = T))

trigrams %>% 
  filter(grepl("alteraciones", trigram, ignore.case = T))

bigrams %>%
  filter(grepl("averías", bigram, ignore.case = TRUE))

trigrams %>%
  filter(grepl("reanudar", trigram, ignore.case = TRUE))

trigrams %>%
  filter(grepl(" parado", trigram, ignore.case = TRUE))

bigrams %>% 
  filter(grepl("sin", bigram, ignore.case = TRUE))