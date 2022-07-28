
# load package ------------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

stint <- read_csv("Final Project/data/eng1_2122_singleapm_stint_eg.csv")
prior <- read_csv("Final Project/data/eng1_2122_priors_adj.csv")


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


train_x_t <- as.matrix(train_x) * as.vector(t)

new_scaled_prior <- as.matrix(prior$weighted_pred * coef[2] + coef[1])
new_train_y_prior_scaled <- (train_y - (train_x_t %*% new_scaled_prior)) %>% as.matrix()

scaled_prior <- prior %>% select(scaled_pred) %>% as.matrix()
scaled_prior <- scaled_prior / 20
train_y_prior_scaled <- (train_y - (train_x_t %*% scaled_prior)) %>% as.matrix()

unscaled_prior <- prior %>% select(weighted_pred) %>% as.matrix()
train_y_prior_unscaled <- (train_y - (train_x_t %*% unscaled_prior)) %>% as.matrix()

any(is.na(t))
any(is.na(train_y_prior))
any(is.na(train_x))
any(is.infinite(train_y_ptiot))

# fit ridge regression to tune lambda -------------------------------------

library(glmnet)

init_ridge_fit <- glmnet(train_x, train_y_prior, alpha = 0, weights = t, lambda = 1e6)
plot(init_ridge_fit, xvar = "lambda")

set.seed(2058)
fit_ridge_cv <- cv.glmnet(train_x, train_y, alpha = 0, weights = t, 
                          standardize = FALSE, parallel = TRUE)
plot(fit_ridge_cv)



tidy_ridge_coef <- tidy(fit_ridge_cv$glmnet.fit)
tidy_ridge_coef %>%
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fit_ridge_cv$lambda.min) +
  geom_vline(xintercept = fit_ridge_cv$lambda.1se,
             linetype = "dashed", color = "red") + 
  theme_bw()



ridge_coef <- tidy_ridge_coef %>%
  filter(lambda == fit_ridge_cv$lambda.min) %>%
  select(term, estimate)
  

rating <- prior %>%
  select(-weighted_pred) %>%
  left_join(ridge_coef, by = c("Player" = "term")) %>%
  mutate(rating = scaled_pred + estimate)



# fit ridge model with lambda 25000 picked --------------------------------

ridge_fit <- glmnet(train_x, train_y_prior_unscaled, alpha = 0, weights = t,
                    lambda = 100000, standardize = FALSE)

tidy_ridge_coef2 <- tidy(ridge_fit$beta)

ridge_coef <- tidy_ridge_coef2 %>%
  select(row, value)


unscaled_rating <- prior %>%
  #select(-weighted_pred) %>%
  select(-scaled_pred) %>%
  #mutate(scaled_pred = scaled_pred / 20) %>%
  left_join(ridge_coef, by = c("Player" = "row")) %>%
  #mutate(rating = scaled_pred + value) %>%
  mutate(rating = weighted_pred +value) %>%
  mutate(rating_scaled = as.numeric(scale(rating)))


scaled_ridge_fit <- glmnet(train_x, new_train_y_prior_scaled, alpha = 0, weights = t,
                    lambda = 500, standardize = FALSE)
y_predict <- predict(scaled_ridge_fit, newx = train_x)
y_hat <- y_predict + (train_x_t %*% new_scaled_prior)

sst <- sum((train_y - mean(train_y))**2)
sse <- sum((y_hat - train_y)**2)
mse <- sse / length(y_hat)
mse
rsq <- abs(1 - (sse / sst))
rsq
    


set.seed(2058)
fit_ridge_cv <- cv.glmnet(train_x, new_train_y_prior_scaled, alpha = 0, weights = t, 
                          standardize = FALSE, parallel = TRUE)
plot(fit_ridge_cv)



tidy_scaled_ridge_coef <- tidy(scaled_ridge_fit$beta)



scaled_ridge_coef <- tidy_scaled_ridge_coef %>%
  select(row, value)

scaled_rating <- prior %>%
  select(Player) %>%
  #select(-scaled_pred) %>%
  #mutate(scaled_pred = scaled_pred / 20) %>%
  bind_cols(new_scaled_prior) %>%
  rename(new_prior = ...2) %>%
  left_join(scaled_ridge_coef, by = c("Player" = "row")) %>%
  mutate(rating = new_prior + value) %>%
  #mutate(rating = weighted_pred +value) %>%
  mutate(rating_scaled = as.numeric(scale(rating)))


# save data ---------------------------------------------------------------

write_csv(rating, "Final Project/data/eng1_2122_apm_rating.csv")



# plot prior distribution -------------------------------------------------

rating %>%
  filter(scaled_pred != 0) %>%
  ggplot(aes(x = scaled_pred)) +
  geom_histogram() +
  theme_bw()

























# try to add redcard as variable ---------------------------------------

train_x_state <- stint %>%
  select(("Ederson" : "Jonathan Tomkinson")) 


library(fastDummies)

game_state <- stint %>%
  select(Score_Progression) %>%
  dummy_cols() %>%
  select(-Score_Progression) 

train_x_state <- game_state %>%
  bind_cols(redcard) %>%
  bind_cols(train_x) %>%
  as.matrix()


train_x_t_state <- as.matrix(train_x_state) * as.vector(t)

scaled_prior_state <- rbind(matrix(0, 35, 1), scaled_prior)
train_y_prior_state <- (train_y - (train_x_t_state %*% scaled_prior_state) 
                        %>% as.matrix())




redcard <- stint %>%
  select(c(redcard_home_cumu, redcard_away_cumu))
train_x_redcard <- redcard %>% bind_cols(train_x) %>% as.matrix()
train_x_t_redcard <- as.matrix(train_x_redcard) * as.vector(t)
scaled_prior_redcard <- rbind(matrix(0, 2, 1), scaled_prior)
train_y_prior_redcard <- (train_y - (train_x_t_redcard %*% scaled_prior_redcard) 
                        %>% as.matrix())


library(glmnet)

set.seed(2058)
fit_redcard_ridge_cv <- cv.glmnet(train_x_redcard, train_y_prior_redcard, 
                                alpha = 0, weights = t, 
                                standardize = FALSE, parallel = TRUE)
plot(fit_redcard_ridge_cv)


redcard_ridge_fit <- glmnet(train_x_redcard, train_y_prior_redcard, 
                          alpha = 0, weights = t,
                          lambda = 1000, standardize = FALSE)

library(broom)
tidy_redcard_ridge_coef <- tidy(redcard_ridge_fit$beta)
#tidy(state_ridge_fit)

redcard_ridge_coef <- tidy_redcard_ridge_coef %>%
  select(row, value)


prior_redcard <- c(colnames(redcard), prior$Player) %>% 
  as_tibble() %>%
  rename(name = value) %>%
  bind_cols(scaled_prior_redcard) 

redcard_rating <- prior_redcard %>%
  left_join(redcard_ridge_coef, by = c("name" = "row")) %>%
  mutate(rating = scaled_pred + value) %>%
  mutate(rating_scaled = as.numeric(scale(rating)))


write_csv(state_rating, "Final Project/data/eng1_2122_singleapm_rating_state.csv")
