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

df_writers <- df %>% 
  distinct(season, episode, writer) %>% 
  separate(writer, sep = ";", into = str_c("writer", 1:3, sep = "_")) %>% 
  pivot_longer(cols = contains("writer"), names_to = "writer", values_to = "writer_name") %>% 
  select(-writer) %>% 
  filter(!is.na(writer_name))

df_writers %>% 
  group_by(season) %>% 
  mutate(number_of_episodes = max(episode)) %>% 
  ungroup() %>% 
  distinct(season, writer_name, number_of_episodes) %>% 
  group_by(season) %>% 
  mutate(distinct_writers = n()) %>% 
  ungroup() %>% 
  mutate(distinct_writers_adj = distinct_writers / number_of_episodes) %>% 
  ggplot(aes(season, distinct_writers_adj)) +
    geom_line() +
    geom_point()

df_writers_long <- df_writers %>% 
  pivot_wider(id_cols = c(season, episode), names_from = writer_name, names_prefix = "writer_", values_from = writer_name) %>% 
  pivot_longer(cols = contains("writer"), names_to = "writer_name", values_to = "writer_flag") %>% 
  mutate(writer_flag = !is.na(writer_flag),
         writer_name = str_remove(writer_name, "writer_")) %>% 
  group_by(season, writer_name) %>% 
  mutate(season_writer_count = sum(writer_flag == TRUE)) %>% 
  ungroup() %>% 
  mutate(writer_name = reorder_within(x = writer_name, by = season_writer_count, within = season))

df_writers_long %>% 
  filter(season_writer_count > 0) %>% 
  ggplot(aes(episode, writer_name, fill = writer_flag)) +
    geom_tile(color = "grey") +
    scale_fill_viridis_d() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_reordered(expand = c(0,0)) +
    facet_wrap(~season, scales = "free") +
    theme(panel.grid = element_blank())
