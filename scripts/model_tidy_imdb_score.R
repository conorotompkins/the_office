#tidy model
library(tidyverse)
library(lubridate)
library(tidytext)
library(janitor)
library(schrute)
library(tidyr)
library(glmnet)
library(tidymodels)

options(scipen = 999, digits = 2)

theme_set(theme_bw())

df <- schrute::theoffice %>% 
  as_tibble() %>% 
  mutate(air_date = ymd(air_date))

df

#imdb_rating
df_imdb <- df %>% 
  distinct(season, episode, imdb_rating)

#directors_new
df_directors <- df %>% 
  distinct(season, episode, director) %>% 
  separate_rows(director, sep = ";") %>% 
  mutate(director = str_c("director", director, sep = "_"))

df_directors <- df_directors %>%  
  mutate(director = str_remove_all(director, "\\."),
         director = str_replace_all(director, "\\-", "_"),
         director = str_replace_all(director, " ", "_"))

df_directors <- df_directors %>% 
  mutate(flag = 1) %>% 
  pivot_wider(id_cols = c(season, episode), names_from = director, values_from = flag, values_fill = list(flag = 0))
                                                   TRUE ~ 0))
df_directors %>% 
  count(season, episode, sort = TRUE)

#writers
df_writers <- df %>% 
  distinct(season, episode, writer) %>% 
  separate_rows(writer, sep = ";")

df_writers <- df_writers %>% 
  mutate(writer = str_remove_all(writer, "\\."),
         writer = str_replace_all(writer, "\\-", "_"),
         writer = str_replace_all(writer, " ", "_")) %>% 
  mutate(writer = str_c("writer", writer, sep = "_"))

df_writers <- df_writers %>% 
  mutate(flag = 1) %>% 
  pivot_wider(id_cols = c(season, episode), names_from = writer, values_from = flag, values_fill = list(flag = 0))

df_writers %>% 
  count(season, episode, sort = TRUE)

#cast
df_cast <- df %>%
  mutate(character = str_replace_all(character, " & ", " and "),
         character = str_replace_all(character, "/", " and "),
         character = str_replace_all(character, ",", " and "),
         character = str_trim(character),
         character = str_remove_all(character, "#"),
         character = str_remove_all(character, "-"),
         character = str_remove_all(character, "'"),
         character = str_remove_all(character, '"'),
         character = str_remove_all(character, "\\["),
         character = str_remove_all(character, "\\]"),
         character = str_remove_all(character, "\\("),
         character = str_remove_all(character, "\\)"),
         character = str_replace_all(character, " ", "_")
  ) %>%
  count(season, episode, character, name = "line_count")

df_top_cast <- df_cast %>% 
  count(character, sort = TRUE) %>% 
  filter(n >= 10) %>% 
  select(character)



df_cast_main <- df_cast %>% 
  semi_join(df_top_cast) %>% 
  pivot_wider(id_cols = c(season, episode), 
              names_from = character, 
              names_prefix = "cast_", 
              values_from = line_count, 
              values_fill = list(line_count = 0))


df_cast_main %>%
  count(season, episode, sort = TRUE)

df_cast_main %>% 
  pivot_longer(cols = contains("cast_")) %>% 
  filter(value > 0) %>% 
  count(name, sort = TRUE) %>% 
  View()
