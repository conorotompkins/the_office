library(tidyverse)
library(lubridate)
library(tidytext)
library(janitor)
library(schrute)
library(tidyr)

options(scipen = 999, digits = 2)

theme_set(theme_bw())

df <- schrute::theoffice %>% 
  as_tibble() %>% 
  mutate(air_date = ymd(air_date))

df

#directors
df_directors <- df %>% 
  distinct(season, episode, director, imdb_rating) %>% 
  separate(director, sep = ";", into = str_c("director", 1:2, sep = "_")) %>% 
  #pivot_longer(cols = director_1, names_to = "director_1", values_to = "director_1_name") %>% 
  #pivot_longer(cols = director_2, names_to = "director_2", values_to = "director_2_name") %>% 
  replace_na(list(director_2 = "none"))

df_directors

df_directors %>% 
  count(season, episode, sort = TRUE)

df_directors %>% 
  distinct(director_1) %>% 
  View()

df_directors %>% 
  distinct(director_2) %>% 
  View()

#writers
df_writers <- df %>% 
  distinct(season, episode, writer) %>% 
  separate(writer, sep = ";", into = str_c("writer", 1:3, sep = "_")) %>% 
  replace_na(list(writer_1 = "none",
                  writer_2 = "none",
                  writer_3 = "none"))

df_writers %>% 
  count(season, episode, sort = TRUE)

df_writers %>% 
  distinct(writer_1) %>% 
  View()

df_writers %>% 
  distinct(writer_2) %>% 
  View()

df_writers %>% 
  distinct(writer_3) %>% 
  View()

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

df_cast_main <- df_cast %>% 
  add_count(season, character) %>% 
  filter(n >= 2)

df_cast_main <- df_cast_main %>% 
  pivot_wider(id_cols = c(season, episode), names_from = character, names_prefix = "cast_", values_from = line_count) %>% 
  mutate(across(where(is.numeric), coalesce, 0))


df_cast_main %>%
  count(season, episode, sort = TRUE)

df_cast_main %>% 
  pivot_longer(cols = contains("cast_")) %>% 
  filter(value > 0) %>% 
  count(name, sort = TRUE) %>% 
  View()

df_full <- df_directors %>% 
  left_join(df_writers) %>% 
  left_join(df_cast_main) %>% 
  select(imdb_rating, everything()) %>% 
  select(-episode) %>% 
  select(imdb_rating, season, contains("director"), contains("writer"), -contains("cast"))

df_full_cast <- df_directors %>% 
  left_join(df_writers) %>% 
  left_join(df_cast_main) %>% 
  select(imdb_rating, everything()) %>% 
  select(-episode) %>% 
  select(-contains("director"), -contains("writer"))

df_full_cast %>% 
  glimpse()


model <- lm(imdb_rating ~ ., data = df_full_cast)

model

model %>% 
  tidy() %>%
  arrange(desc(estimate)) %>% 
  View()

model %>% 
  augment() %>% 
  ggplot(aes(imdb_rating, .fitted)) +
    geom_abline() +
    geom_point(alpha = .25) +
    geom_smooth()

model %>% 
  augment() %>% 
  ggplot(aes(.resid)) +
    geom_density()
