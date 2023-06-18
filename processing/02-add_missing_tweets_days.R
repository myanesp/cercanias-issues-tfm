library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(stringi)

if (!file.exists("../data/processed_data/tweets_cercanias_madrid.csv")) {
  source("01-merge_data_api_snscrape.R")
} else {
  print("File exists. Proceeding with the next step")
}

df <- read_csv("../data/processed_data/tweets_cercanias_madrid.csv", col_types = cols(Tweet_Id = col_character())) %>% 
  select(c(2:7))

last <- as.Date(tail(df$Datetime, n = 1))
status <- ymd(Sys.Date()) - 1

if (status != last) {
  new_day <- read_csv(paste0("../data/raw_data/CercaniasMadrid_tweets_", status, ".csv"), col_types = cols(`Tweet_Id` = col_character(), 
                                             `Reply_to` = col_character())) %>% arrange(Datetime)
  df_updated <- bind_rows(df, new_day)
  write.csv(df_updated, "../data/processed_data/tweets_cercanias_madrid.csv")

} else if (status == last) {
  print("CSV is already updated")
} else {
  print("There may be a mistake")
}


