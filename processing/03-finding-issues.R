library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(lubridate)
library(stringr)
library(stringi)


setwd("/app/processing")

pattern_lines <- "(?i)(?<!\\S)C\\d+\\b"
pattern_hashtags <- "(?i)C\\d+\\S*"
pattern_destination <- "(?i)(destino|dirección|direccion)\\s+(\\w+)"
issues <- c("produciendo retenciones", "con demoras", "con demora", 
            "fuertes demoras", "están sufriendo demoras", "produciendo demoras",
            "produciendo retrasos", "fuertes retrasos", "con retraso",
            "fuertes alteraciones",
            "sufrir alteraciones", "posibles alteraciones", "produciendo alteraciones",
            "suceder alteraciones", "tren averiado", "tren detenido", "tren parado",
            "detenido por avería", "se encuentran parados", "se encuentra parado")


df <- read_csv("../data/processed_data/tweets_cercanias_madrid.csv", 
               col_types = cols(Tweet_Id = col_character())) %>% 
  select(c(2:8)) %>% 
  mutate(Text = str_replace(Text, "(?<=^|[^\\w\\d])@\\w+", "")) %>%   
  mutate(original = str_replace(original, "@CercaniasMadrid", "")) %>%
  mutate(Hashtags = str_extract_all(Hashtags, "(#\\w+|\\b\\w+\\b)")) %>% 
  mutate(Hashtags = lapply(Hashtags, function(x) str_replace_all(x, "#", ""))) %>% 
  unnest(Hashtags, keep_empty = T) %>%
  mutate(Lines = if_else(is.na(Hashtags),
                       str_extract(original, pattern_lines),
                       if_else(str_detect(Hashtags, pattern_hashtags),
                               str_extract(Hashtags, pattern_hashtags),
                               NA_character_))) %>%
  mutate(Lines = tolower(Lines)) %>%
  filter(!is.na(Lines) & grepl(paste0(issues, collapse = "|"), Text))

write.csv(df, "../data/final_data/cercanias_madrid_issues.csv")
file.copy("../data/final_data/cercanias_madrid_issues.csv", "../visualization/cercanias_madrid_issues.csv", overwrite = T)
