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

df_cast <- df %>% 
  count(season, episode, character) %>% 
  pivot_wider(id_cols = c(season, episode), names_from = character, names_prefix = "cast_", values_from = n)

df_cast

df_cast %>% 
  pivot_longer(cols = contains("cast"), names_to = "cast", values_to = "line_count") %>% 
  filter(line_count > 0) %>% 
  distinct(season, episode, cast) %>% 
  group_by(season) %>% 
  mutate(number_of_episodes = max(episode)) %>% 
  distinct() %>% 
  ungroup() %>%
  group_by(season) %>% 
  mutate(distinct_cast = n()) %>% 
  ungroup() %>% 
  mutate(distinct_cast_adj = distinct_cast / number_of_episodes) %>% 
  ggplot(aes(season, distinct_cast_adj)) +
    geom_line() +
    geom_point()



df_cast_long <- df_cast %>% 
  pivot_longer(cols = contains("cast"), names_to = "cast_name", values_to = "line_count") %>%
  mutate(cast_flag = !is.na(line_count),
         cast_name = str_remove(cast_name, "cast_")) %>%
  group_by(season, cast_name) %>% 
  mutate(season_cast_count = sum(cast_flag == TRUE),
         season_line_count = sum(line_count, na.rm = TRUE),
         lines_per_episode = season_line_count / season_cast_count) %>% 
  ungroup()

df_cast_long %>% 
  mutate(cast_name = reorder_within(x = cast_name, by = season_line_count, within = season)) %>% 
  filter(season_cast_count > 1) %>% 
  filter(season == 5) %>% 
  ggplot(aes(episode, cast_name, fill = line_count)) +
    geom_tile(color = "grey") +
    scale_fill_viridis_c() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_reordered(expand = c(0,0)) +
    facet_wrap(~season, scales = "free") +
    theme(panel.grid = element_blank())

df_cast_count <- df_cast_long %>% 
  filter(cast_flag == TRUE) %>% 
  count(cast_name, sort = TRUE) %>% 
  filter(n >= 10)

df_cast_long %>% 
  semi_join(df_cast_count) %>% 
  filter(cast_flag == TRUE) %>% 
  group_by(season, episode, cast_name) %>% 
  summarize(line_count = sum(line_count)) %>% 
  group_by(cast_name) %>% 
  summarize(line_avg = mean(line_count)) %>% 
  arrange(desc(line_avg)) %>% 
  View()

            