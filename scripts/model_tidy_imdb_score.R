#tidy model
library(tidyverse)
library(lubridate)
library(tidytext)
library(janitor)
library(schrute)
library(tidyr)
library(glmnet)
library(tidymodels)
library(vip)

options(scipen = 999, digits = 2)

theme_set(theme_bw())

set.seed(1234)

#prep data
df <- schrute::theoffice %>% 
  as_tibble() %>% 
  mutate(air_date = ymd(air_date))

df

#for IDing episodes
df_episode_list <- df %>% 
  distinct(season, episode) %>% 
  mutate(episode_id = str_c(season, episode, sep = "_"))

#imdb_ratings and flags for season premiers and finales
df_imdb <- df %>% 
  distinct(season, episode, imdb_rating) %>% 
  group_by(season) %>% 
  mutate(flag_premier = episode == first(episode),
         flag_finale = episode == last(episode)) %>% 
  ungroup() %>% 
  mutate(across(contains("flag"), as.numeric))

glimpse(df_imdb)

#directors
df_directors <- df %>% 
  distinct(season, episode, director) %>% 
  separate_rows(director, sep = ";")

#clean up
df_directors %>% 
  distinct(director) %>% 
  arrange(director)

df_director_fix <- tibble(director_good = c("Charles McDougall",
                                            "Claire Scanlon",
                                            "Greg Daniels",
                                            "Ken Whittingham",
                                            "Paul Lieberstein"),
                          director_bad = c("Charles McDougal",
                                           "Claire Scanlong",
                                           "Greg Daneils",
                                           "Ken Wittingham",
                                           "Paul Lieerstein"))

df_directors <- df_directors %>% 
  left_join(df_director_fix, by = c("director" = "director_bad")) %>% 
  mutate(director = case_when(!is.na(director_good) ~ director_good,
                              is.na(director_good) ~ director)) %>% 
  mutate(director = str_c("director", director, sep = "_")) %>% 
  select(-director_good)

df_directors <- df_directors %>%  
  mutate(director = str_remove_all(director, "\\."),
         director = str_replace_all(director, "\\-", "_"),
         director = str_replace_all(director, " ", "_")) %>% 
  add_count(director) %>% 
  filter(n > 2) %>% 
  select(-n)

df_directors <- df_directors %>% 
  mutate(flag = 1) %>% 
  pivot_wider(id_cols = c(season, episode), names_from = director, values_from = flag, values_fill = list(flag = 0))

df_directors %>% 
  select(1:20) %>% 
  glimpse()

#writers
df_writers <- df %>% 
  distinct(season, episode, writer) %>% 
  separate_rows(writer, sep = ";") %>% 
  add_count(writer) %>% 
  filter(n > 2)

df_writers <- df_writers %>% 
  mutate(writer = str_remove_all(writer, "\\."),
         writer = str_replace_all(writer, "\\-", "_"),
         writer = str_replace_all(writer, " ", "_")) %>% 
  mutate(writer = str_c("writer", writer, sep = "_"))

df_writers <- df_writers %>% 
  mutate(flag = 1) %>% 
  pivot_wider(id_cols = c(season, episode), names_from = writer, values_from = flag, values_fill = list(flag = 0))

df_writers %>% 
  select(1:20) %>% 
  glimpse()

#cast
df_characters <- df %>% 
  select(season, episode, character) %>% 
  mutate(character = case_when(season == 7 & episode == 18 & character == "Todd" ~ "Todd Packer",
                               TRUE ~ character)) %>% 
  mutate(character = case_when(season == 7 & episode == 14 & character == "David" ~ character,
                               character == "David" ~ "David Wallace",
                               TRUE ~ character)) %>% 
  mutate(character = case_when(character == "DeAngelo" ~ "Deangelo",
                               TRUE ~ character))

df_characters <- df_characters %>%
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
         character = str_replace_all(character, " ", "_")) %>%
  count(season, episode, character, name = "line_count")

df_top_characters <- df_characters %>% 
  count(character, sort = TRUE) %>% 
  filter(n >= 20) %>% 
  select(character)

df_characters_main <- df_characters %>% 
  semi_join(df_top_characters) %>% 
  pivot_wider(id_cols = c(season, episode), 
              names_from = character, 
              names_prefix = "cast_", 
              values_from = line_count, 
              values_fill = list(line_count = 0))

df_characters_main %>% 
  select(1:20) %>% 
  glimpse()

#model
##prepare

df_office <- df_imdb %>% 
  left_join(df_directors) %>% 
  left_join(df_writers) %>% 
  left_join(df_characters_main) %>% 
  mutate(episode_id = str_c(season, episode, sep = "_")) %>% 
  mutate(across(contains("director"), coalesce, 0),
         across(contains("writer"), coalesce, 0)) %>% 
  select(-episode)

df_office %>% 
  select(1:20) %>% 
  glimpse()

office_split <- initial_split(df_office, strata = season)
office_train <- training(office_split)
office_test <- testing(office_split)

office_rec <- recipe(imdb_rating ~ ., data = office_train) %>%
  update_role(episode_id, new_role = "ID") %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

office_prep <- office_rec %>%
  prep(strings_as_factors = FALSE)

#workflow
#tune lasso parameters

wf <- workflow() %>%
  add_recipe(office_rec)

office_boot <- bootstraps(office_train, strata = season)

tune_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 50)

ridge_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = office_boot,
  grid = lambda_grid)

#analyze metrics
lasso_grid %>%
  collect_metrics()


#select best model
owest_rmse <- ridge_grid %>%
  select_best("rmse")

#graph metrics
ridge_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric, fill = .metric)) +
  geom_ribbon(aes(ymin = mean - std_err,
                  ymax = mean + std_err),
              alpha = 0.5) +
  geom_line(size = 1.5) +
  geom_vline(xintercept = pull(lowest_rmse), linetype = 2) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  labs(title = "Ridge regression lambda values") +
  theme(legend.position = "none")


#finalize workflow
final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), lowest_rmse)


final_ridge %>% 
  fit(office_train) %>% 
  predict(office_train) %>% 
  bind_cols(office_train) %>% 
  ggplot(aes(imdb_rating, .pred)) +
  geom_abline(linetype = 2) +
  geom_point(alpha = .2) +
  geom_smooth() +
  coord_equal() +
  labs(x = "IMDB rating",
       y = "Predicted rating")

final_ridge %>% 
  fit(office_train) %>% 
  predict(office_train) %>% 
  bind_cols(office_train) %>% 
  separate(episode_id, into = c("season", "episode"), sep = "_") %>% 
  mutate(.resid = imdb_rating - .pred) %>% 
  select(season, episode, .resid) %>% 
  ggplot(aes(season, .resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(y = "Residual",
       title = "Actual minus predicted rating")

#analyze variables
df_vi <- final_ridge %>%
  fit(office_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(Variable = case_when(str_detect(Variable, "writer|director|cast") ~ Variable,
                              TRUE ~ str_c("other_", Variable))) %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  separate(Variable, sep = "_", into = c("role", "person"), extra = "merge") %>% 
  mutate(person = str_replace_all(person, "_", " "))

df_vi %>% 
  mutate(person = tidytext::reorder_within(x = person, by = Importance, within = role)) %>% 
  ggplot(aes(x = Importance, y = person, fill = Importance)) +
  geom_col(color = "black") +
  facet_wrap(~role, scales = "free_y") +
  scale_fill_viridis_c() +
  scale_y_reordered() +
  labs(y = NULL)

df_vi %>%  
  filter(person == "Greg Daniels")

df_vi %>% 
  filter(person == "Dwight" | person == "Rainn Wilson")

#analyze test data
last_fit(final_lasso, office_split) %>%
  collect_metrics()
