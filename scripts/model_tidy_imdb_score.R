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

set.seed(1234)

#prep data
df <- schrute::theoffice %>% 
  as_tibble() %>% 
  mutate(air_date = ymd(air_date))

df

df_episode_list <- df %>% 
  distinct(season, episode) %>% 
  mutate(episode_id = str_c(season, episode, sep = "_"))

#imdb_rating
df_imdb <- df %>% 
  distinct(season, episode, imdb_rating) %>% 
  group_by(season) %>% 
  mutate(flag_premier = episode == first(episode),
         flag_finale = episode == last(episode)) %>% 
  ungroup() %>% 
  mutate(across(contains("flag"), as.numeric))

#directors
df_directors <- df %>% 
  distinct(season, episode, director) %>% 
  separate_rows(director, sep = ";") %>% 
  mutate(director = str_c("director", director, sep = "_"))

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
  count(season, episode, sort = TRUE)

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
  filter(n >= 20) %>% 
  select(character)

df_top_cast

df_cast_main <- df_cast %>% 
  semi_join(df_top_cast) %>% 
  pivot_wider(id_cols = c(season, episode), 
              names_from = character, 
              names_prefix = "cast_", 
              values_from = line_count, 
              values_fill = list(line_count = 0))

df_cast_main %>%
  count(season, episode, sort = TRUE)

#model
##prepare

df_office <- df_imdb %>% 
  left_join(df_directors) %>% 
  left_join(df_writers) %>% 
  left_join(df_cast_main) %>% 
  mutate(episode_id = str_c(season, episode, sep = "_")) %>% 
  mutate(across(contains("director"), coalesce, 0),
         across(contains("writer"), coalesce, 0)) %>% 
  select(-episode)

glimpse(df_office)

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
lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(office_rec)

lasso_fit <- wf %>%
  add_model(lasso_spec) %>%
  fit(data = office_train)

lasso_fit %>%
  pull_workflow_fit() %>%
  tidy()

#tune lasso parameters

office_boot <- bootstraps(office_train, strata = season)

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 50)


lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = office_boot,
  grid = lambda_grid
)

#analyze metrics
lasso_grid %>%
  collect_metrics()


#select best model
lasso_grid %>% 
  unnest(.metrics) %>% 
  select(penalty, .metric, .estimate) %>% 
  filter(.metric == "rmse") %>% 
  filter(.estimate == min(.estimate))

lowest_rmse <- lasso_grid %>%
  select_best("rmse", maximize = FALSE)

#graph metrics
lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric, fill = .metric)) +
  geom_ribbon(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  geom_vline(xintercept = pull(lowest_rmse), linetype = 2) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  labs(title = "LASSO") +
  theme(legend.position = "none")


#finalize workflow
final_lasso <- finalize_workflow(
  wf %>% add_model(tune_spec),
  lowest_rmse
)


#analyze variables
library(vip)

final_lasso %>% 
  fit(office_train) %>% 
  predict(office_train) %>% 
  bind_cols(office_train) %>% 
  ggplot(aes(imdb_rating, .pred)) +
    geom_abline(linetype = 2) +
    geom_point(alpha = .2) +
    geom_smooth()

final_lasso %>%
  fit(office_train) %>%
  pull_workflow_fit() %>% 
  vi(lambda = lowest_rmse$penalty)


final_lasso %>%
  fit(office_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  #filter(str_detect(Variable, "writer|director|cast")) %>% 
  mutate(Variable = case_when(str_detect(Variable, "writer|director|cast") ~ Variable,
                              TRUE ~ str_c("other_", Variable))) %>% 
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  separate(Variable, sep = "_", into = c("role", "person"), extra = "merge") %>% 
  mutate(person = tidytext::reorder_within(x = person, by = Importance, within = role)) %>% 
  #filter(Importance > .05) %>% 
  ggplot(aes(x = Importance, y = person, fill = Sign)) +
  geom_col(color = "black") +
  facet_wrap(~role, scales = "free_y") +
  scale_fill_viridis_d() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(y = NULL)


#analyze test data
last_fit(
  final_lasso,
  office_split
) %>%
  collect_metrics()

# # A tibble: 2 x 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 rmse    standard       0.503
# 2 rsq     standard       0.147
