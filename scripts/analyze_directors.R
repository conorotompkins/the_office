library(tidyverse)
library(lubridate)
library(tidytext)
library(janitor)
library(schrute)

theme_set(theme_bw())

df <- schrute::theoffice %>% 
  as_tibble() %>% 
  mutate(air_date = ymd(air_date))

df

df_directors <- df %>% 
  distinct(season, episode, director) %>% 
  separate(director, sep = ";", into = str_c("director", 1:2, sep = "_")) %>% 
  pivot_longer(cols = contains("director"), names_to = "director", values_to = "director_name") %>% 
  select(-director) %>% 
  filter(!is.na(director_name))

df_directors %>% 
  count(season, episode, sort = TRUE)

df_directors %>% 
  group_by(season) %>% 
  mutate(number_of_episodes = max(episode)) %>% 
  ungroup() %>% 
  distinct(season, director_name, number_of_episodes) %>% 
  group_by(season) %>% 
  mutate(distinct_directors = n()) %>% 
  ungroup() %>% 
  mutate(distinct_directors_adj = distinct_directors / number_of_episodes) %>% 
  ggplot(aes(season, distinct_directors_adj)) +
    geom_line() +
    geom_point()

df_directors_long <- df_directors %>% 
  pivot_wider(id_cols = c(season, episode), names_from = director_name, names_prefix = "director_", values_from = director_name) %>% 
  pivot_longer(cols = contains("director"), names_to = "director_name", values_to = "director_flag") %>% 
  mutate(director_flag = !is.na(director_flag),
         director_name = str_remove(director_name, "director_")) %>% 
  group_by(season, director_name) %>% 
  mutate(season_director_count = sum(director_flag == TRUE)) %>% 
  ungroup() %>% 
  mutate(director_name = reorder_within(x = director_name, by = season_director_count, within = season))

df_directors_long %>% 
  filter(season_director_count > 0) %>% 
  ggplot(aes(episode, director_name, fill = director_flag)) +
    geom_tile() +
    scale_fill_viridis_d() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_reordered(expand = c(0,0)) +
    facet_wrap(~season, scales = "free")
