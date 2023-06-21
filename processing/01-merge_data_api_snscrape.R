library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(stringi)

setwd("/app/processing")

data_snt <- list()
directory_path <- "../data/raw_data"

# From 2023 to today, using snscrape
csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

for (file in csv_files) {
  data <- read_csv(file, col_types = cols(`Tweet_Id` = col_character(), 
                                          `Reply_to` = col_character())) # to avoid converting to sci.notation the ids
  data_snt <- bind_rows(data_snt, data)
}

data_snt <- data_snt %>% arrange(Datetime)

# Iñaki with API: only tweets that contains HASHTAGS
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

previous <- previous %>% arrange(Datetime)

merged <- bind_rows(previous, data_snt)
write.csv(merged, "../data/processed_data/tweets_cercanias_madrid.csv")






