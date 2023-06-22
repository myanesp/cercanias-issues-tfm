library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(stringi)

setwd("/app/processing")

if (!file.exists("../data/processed_data/tweets_cercanias_madrid.csv")) {
  source("01-merge_data_api_snscrape.R")
} else {
  print("File exists. Proceeding with the next step")
}

df <- read_csv("../data/processed_data/tweets_cercanias_madrid.csv", col_types = cols(Tweet_Id = col_character())) %>% 
  select(c(!1))

last <- as.Date(tail(df$Datetime, n = 1))
status <- ymd(Sys.Date() - 1)

if (last == status){
  print("CSV is already updated")
} else if (last < status){
  date_sequence <- seq(from = last + 1, to = status, by = "day")
  print(date_sequence)
  for (current_date in date_sequence) {
    file_path <- paste0("../data/raw_data/CercaniasMadrid_tweets_", as.Date(current_date), ".csv")

    if (file.exists(file_path)) {
      new_day <- read_csv(file_path, col_types = cols(`Tweet_Id` = col_character(), `Reply_to` = col_character())) %>% arrange(Datetime)
      df_updated <- bind_rows(df, new_day)
      write.csv(df_updated, "../data/processed_data/tweets_cercanias_madrid.csv")
    } else {
      print(paste("File not found for date:", current_date))
    }
  }
}
    
    




