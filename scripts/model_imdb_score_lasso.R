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
  separate(director, sep = ";", into = str_c("director", 1:2, sep = "_")) %>% 
  pivot_longer(cols = contains("director"), names_to = "director", values_to = "director_name") %>%
  mutate(director_name = str_c("director", director_name, sep = "_")) %>% 
  select(-director) %>% 
  filter(!is.na(director_name))

df_directors <- df_directors %>%  
  mutate(director_name = str_remove_all(director_name, "\\."),
         director_name = str_replace_all(director_name, "\\-", "_"),
         director_name = str_replace_all(director_name, " ", "_"))

df_directors <- df_directors %>% 
  pivot_wider(id_cols = c(season, episode), names_from = director_name, values_from = director_name) %>% 
  mutate_at(vars(contains("director")), ~case_when(!is.na(.) ~ 1,
                                                        TRUE ~ 0))
df_directors %>% 
  count(season, episode, sort = TRUE)

#writers
df_writers <- df %>% 
  distinct(season, episode, writer) %>% 
  separate(writer, sep = ";", into = str_c("writer", 1:3, sep = "_")) %>% 
  pivot_longer(cols = contains("writer"), names_to = "writer", values_to = "writer_name") %>% 
  filter(!is.na(writer_name))

df_writers <- df_writers %>% 
  mutate(writer_name = str_remove_all(writer_name, "\\."),
         writer_name = str_replace_all(writer_name, "\\-", "_"),
         writer_name = str_replace_all(writer_name, " ", "_")) %>% 
  mutate(writer_name = str_c("writer", writer_name, sep = "_")) %>% 
  select(-writer)

df_writers <- df_writers %>% 
  pivot_wider(id_cols = c(season, episode), names_from = writer_name, values_from = writer_name) %>% 
  mutate_at(vars(contains("writer")), ~case_when(!is.na(.) ~ 1,
                                                   TRUE ~ 0))

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

df_cast_main <- df_cast %>% 
  add_count(season, character) %>% 
  filter(n >= 3)

df_cast_main <- df_cast_main %>% 
  pivot_wider(id_cols = c(season, episode), names_from = character, names_prefix = "cast_", values_from = line_count) %>% 
  #mutate(across(where(is.numeric), coalesce, 0))
  mutate_at(vars(contains("cast")), ~case_when(!is.na(.) ~ 1,
                                               TRUE ~ 0))


df_cast_main %>%
  count(season, episode, sort = TRUE)

df_cast_main %>% 
  pivot_longer(cols = contains("cast_")) %>% 
  filter(value > 0) %>% 
  count(name, sort = TRUE) %>% 
  View()

#model with directors and writers

df_directors_writers <- df_imdb %>% 
  left_join(df_directors) %>% 
  left_join(df_writers) %>% 
  select(imdb_rating, everything()) %>% 
  select(-episode)

model <- lm(imdb_rating ~ ., data = df_directors_writers)

model %>% 
  glance()

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


#model with directors and writer and cast


df_full <- df_imdb %>%
  left_join(df_directors) %>% 
  left_join(df_writers) %>% 
  left_join(df_cast_main) %>% 
  select(imdb_rating, everything()) %>% 
  select(-c(episode))

df_full %>% 
  glimpse()

model_full <- lm(imdb_rating ~ ., data = df_full)

model_full %>% 
  glance()

model_full %>% 
  tidy() %>%
  arrange(desc(estimate)) %>% 
  View()

model_full %>% 
  augment() %>% 
  ggplot(aes(imdb_rating, .fitted)) +
    geom_abline() +
    geom_point(alpha = .25) +
    geom_smooth()

model_full %>% 
  augment() %>% 
  ggplot(aes(.resid)) +
    geom_density()

#run glm model with ridge regression
cv.glmnet(x = RAPM_CF, y = CF60, weights = length_shift, alpha = 0, nfolds = 10, standardize = FALSE, parallel = TRUE)

df_full_predictor <- df_full %>% 
  select(-imdb_rating) %>% 
  as.matrix()

response_var <- df_full %>%
  pull(imdb_rating)

model_glm_cv <- cv.glmnet(x = df_full_predictor, y = response_var, alpha = 0, nfolds = 10)

model_glm_cv$glmnet.fit

model_glm_cv %>% 
  glance()

model_glm_cv %>% 
  tidy(return_zeroes = TRUE) %>% 
  mutate(log_lambda = log(lambda)) %>%
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()

model_glm_cv %>% 
  coef() %>% 
  tidy() %>% 
  arrange(desc(value))

#ridge
linear_reg(penalty = .10, mixture = 0) %>% # mixture = 0 meaning no L1 penalty 
  set_mode("regression") %>% 
  set_engine("glmnet") %>% 
  fit(imdb_rating ~ ., data = df_full)

