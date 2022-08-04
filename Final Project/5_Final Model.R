

# load package ------------------------------------------------------------

stint <- read_csv("Final Project/data/eng1_2122_singleapm_stint_eg.csv")
prior <- read_csv("Final Project/data/priors.csv")

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

prior$adj_overall[is.na(prior$adj_overall)] <- mean(prior$adj_overall, na.rm = TRUE)


train_x_t <- as.matrix(train_x) * as.vector(t)

box_prior <- as.matrix(prior$weighted_pred * coef[2] + coef[1])
train_y_box_prior <- (train_y - (train_x_t %*% box_prior)) %>% as.matrix()

FIFA_prior <- as.matrix(prior$adj_overall * coef[2] + coef[1])
train_y_FIFA_prior <- (train_y - (train_x_t %*% FIFA_prior)) %>% as.matrix()


# fit model ---------------------------------------------------------------
library(glmnet)

box_ridge_fit <- glmnet(train_x, train_y_box_prior, alpha = 0, weights = t,
                           lambda = 800, standardize = FALSE, intercept = FALSE)

FIFA_ridge_fit <- glmnet(train_x, train_y_FIFA_prior, alpha = 0, weights = t,
                        lambda = 800, standardize = FALSE, intercept = FALSE)


box_ridge_coef <- tidy(box_ridge_fit$beta) %>%
  select(row, value)

FIFA_ridge_coef <- tidy(FIFA_ridge_fit$beta) %>%
  select(row, value)

scaled_rating <- prior %>%
  select(Player) %>%
  bind_cols(box_prior) %>%
  rename(box_prior = ...2) %>%
  bind_cols(FIFA_prior) %>%
  rename(FIFA_prior = ...3) %>%
  left_join(box_ridge_coef, by = c("Player" = "row")) %>%
  rename(box_coef = value) %>%
  mutate(box_rating = box_prior + box_coef) %>%
  left_join(FIFA_ridge_coef, by = c("Player" = "row")) %>%
  rename(FIFA_coef = value) %>%
  mutate(FIFA_rating = FIFA_prior + FIFA_coef)


# save data ---------------------------------------------------------------

write_csv(scaled_rating, "Final Project/data/eng1_2122_singleapm_rating.csv")
  