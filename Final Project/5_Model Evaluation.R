

# load package ------------------------------------------------------------

library(tidyverse)
library(glmnet)


# load data ---------------------------------------------------------------

stint <- read_csv("Final Project/data/eng1_2122_singleapm_stint_eg.csv")
prior <- read_csv("Final Project/data/eng1_2122_priors_adj.csv")
fifa22 <- read_csv("Final Project/data/priors_actualfifa.csv") 


# prepare data ------------------------------------------------------------

train_x <- stint %>%
  select("Ederson" : "Jonathan Tomkinson") %>%
  as.matrix()

train_y <- stint %>%
  select(c("xG_diff", "stint_length")) %>%
  mutate(value = (xG_diff / stint_length) * 95) %>%
  select(value) %>%
  as.matrix()

train_y[is.na(train_y)] <- 0
train_y[is.infinite(train_y)] <- 0

t <- stint %>%
  select(stint_length) %>%
  as.matrix()


  # prior

fifa22_adj <- fifa22 %>% 
  mutate(Min_overall = Min * overall) %>%
  select(Player, Min_overall, Min) %>%
  group_by(Player) %>%
  summarise(adj_overall = sum(Min_overall),
            total_min = sum(Min)) %>%
  ungroup() %>%
  mutate(adj_overall = adj_overall / total_min) %>%
  mutate(adj_scaled = adj_overall * coef[2] + coef[1])

prior <- prior %>% 
  left_join(fifa22_adj) %>%
  mutate(pred_scaled = weighted_pred * coef[2] + coef[1]) %>%
  mutate(pred_scaled_int = scaled_pred / 20)

prior$adj_scaled[is.na(prior$adj_scaled)] <- mean(prior$adj_scaled, na.rm = TRUE)



# validation --------------------------------------------------------------

set.seed(2058)

train_test_split <- stint %>%
  select(matchID) %>%
  unique() %>%
  mutate(test_fold = sample(rep(1:10, length.out = n()))) %>%
  as_data_frame()

model_data <- train_x %>%
  bind_cols(train_y) %>%
  rename(y = value) %>%
  bind_cols(t) %>%
  bind_cols(stint$matchID) %>%
  rename(matchID = ...681) %>%
  left_join(train_test_split) %>%
  as_tibble()
 


holdout_predictions <-
  map_dfr(unique(model_data$test_fold),
          function(holdout) {
            
            #separate test / training data
            test_data <- model_data %>% filter(test_fold == holdout)
            train_data <- model_data %>% filter(test_fold != holdout)
          
            
            #repeat for matrics
            test_x <- test_data %>% select("Ederson" : "Jonathan Tomkinson") %>% as.matrix() 
            test_t <- test_data$stint_length
            test_x_t <- as.matrix(test_x) * as.vector(test_t)
            
            train_x <- train_data %>% select("Ederson" : "Jonathan Tomkinson") %>% as.matrix()
            train_t <- train_data$stint_length
            train_x_t <- as.matrix(train_x) * as.vector(train_t)
            
            new_pred_prior <- prior$pred_scaled %>% as.matrix()
            pred_prior <- prior$pred_scaled_int %>% as.matrix()
            FIFA_prior <- prior$adj_scaled %>% as.matrix()
            
            test_y <- test_data$y
            test_y_ridge_pred_new <- (test_y - (test_x_t %*% new_pred_prior)) %>% 
              as.matrix()
            test_y_ridge_pred <- (test_y - (test_x_t %*% pred_prior)) %>% 
              as.matrix()
            test_y_ridge_FIFA <- (test_y - (test_x_t %*% FIFA_prior)) %>% 
              as.matrix()
            
            train_y <- train_data$y
            train_y_ridge_pred_new <- (train_y - (train_x_t %*% new_pred_prior)) %>% 
              as.matrix()
            train_y_ridge_pred <- (train_y - (train_x_t %*% pred_prior)) %>% 
              as.matrix()
            train_y_ridge_FIFA <- (train_y - (train_x_t %*% FIFA_prior)) %>% 
              as.matrix()
            
              
            #train models
            #zero_model <- # 
            intercept_model <- mean(train_y)
            ridge_new_pred_model <- glmnet(train_x, train_y_ridge_pred_new, 
                                           alpha = 0, weights = train_t,
                                           lambda = 800, standardize = FALSE,
                                           intercept = FALSE)
            ridge_pred_model <- glmnet(train_x, train_y_ridge_pred, 
                                       alpha = 0, weights = train_t,
                                       lambda = 800, standardize = FALSE,
                                       intercept = FALSE)
            ridge_FIFA_model <- glmnet(train_x, train_y_ridge_FIFA, 
                                       alpha = 0, weights = train_t,
                                       lambda = 800, standardize = FALSE,
                                       intercept = FALSE)
            ridge_APM_model <- glmnet(train_x, train_y, 
                                      alpha = 0, weights = train_t,
                                      lambda = 0.5, standardize = FALSE,
                                      intercept = FALSE)
            
            # test/predict models
            ridge_new_pred_preds <- predict(ridge_new_pred_model, newx = test_x) + (test_x_t %*% new_pred_prior)
            ridge_pred_preds <- predict(ridge_pred_model, newx = test_x) + (test_x_t %*% pred_prior)
            ridge_FIFA_preds <- predict(ridge_FIFA_model, newx = test_x) + (test_x_t %*% FIFA_prior)
            
            
            # return tibble of holdout results:
            tibble(zero_preds = as.numeric(matrix(0, length(test_y), 1)),
                   intercept_preds = as.numeric(matrix(intercept_model, length(test_y)), 1),
                   ridge_new_pred_preds = as.numeric(ridge_new_pred_preds),
                   ridge_pred_preds = as.numeric(ridge_pred_preds),
                   ridge_FIFA_preds = as.numeric(ridge_FIFA_preds),
                   ridge_APM_preds = as.numeric(predict(ridge_APM_model, newx = test_x_t)),
                   test_actual = test_y, 
                   test_fold = holdout)
          }
          )



# compare result ----------------------------------------------------------

holdout_predictions %>%
  rename(zero = zero_preds,
         intercept = intercept_preds,
         RAxGPM_link = ridge_new_pred_preds,
         RAxGPM_int = ridge_pred_preds,
         APM_FIFA = ridge_FIFA_preds,
         APM_only = ridge_APM_preds) %>%
  pivot_longer(zero : APM_only,
               names_to = "model", values_to = "test_preds") %>%
  group_by(model, test_fold) %>%
  summarise(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
            #r2 = 1 - (sum((test_actual - test_preds)**2) / sum((test_actual - mean(test_actual))**2))) %>%
  ggplot(aes(x = model, y = rmse)) +
  geom_point() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red")


# aggregate per game ------------------------------------------------------

holdout_predictions %>%
  bind_cols(model_data$matchID) %>%
  rename(matchID = ...9) %>%
  pivot_longer(zero_preds : ridge_APM_preds,
               names_to = "model", values_to = "test_preds") %>%
  group_by(matchID, model, test_fold) %>%
  summarise(total_actual = sum(test_actual),
            total_preds = sum(test_preds)) %>%
  ungroup() %>%
  group_by(model, test_fold) %>%
  summarise(rmse = sqrt(mean((total_actual - total_preds)^2))) %>%
  ggplot(aes(x = model, y = rmse)) +
  geom_point() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red")


# plot --------------------------------------------------------------------

holdout_predictions %>%
  ggplot(aes(x = test_actual)) +
  geom_histogram() +
  theme_bw()

holdout_predictions %>%
  rename(zero = zero_preds,
         intercept = intercept_preds,
         RAxGPM_link = ridge_new_pred_preds,
         RAxGPM_int = ridge_pred_preds,
         RAxGPM_FIFA = ridge_FIFA_preds,
         RAxGPM_only = ridge_APM_preds) %>%
  pivot_longer(zero : RAxGPM_only,
               names_to = "model", values_to = "test_preds") %>%
  ggplot(aes(x = test_preds)) +
  geom_histogram() +
  facet_wrap(~ model, ncol = 3) +
  theme_bw()

holdout_predictions %>%
  rename(zero = zero_preds,
         intercept = intercept_preds,
         RAxGPM_link = ridge_new_pred_preds,
         RAxGPM_int = ridge_pred_preds,
         RAxGPM_FIFA = ridge_FIFA_preds,
         RAxGPM_only = ridge_APM_preds) %>%
  pivot_longer(zero : RAxGPM_only,
               names_to = "model", values_to = "test_preds") %>%
  ggplot(aes(x = test_preds, y = test_actual)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed", color = "red") +
  facet_wrap(~ model, ncol = 3) +
  theme_bw()


# save data ---------------------------------------------------------------

holdout_predictions <- as.tibble(holdout_predictions)
write_csv(holdout_predictions, "Final Project/data/holdout_predictions.csv")

write_csv(prior, "Final Project/data/prior.csv")