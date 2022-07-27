
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


scaled_prior <- prior %>% select(scaled_pred) %>% as.matrix()
train_y_prior <- (train_y - (train_x_t %*% scaled_prior)) %>% as.matrix()


any(is.na(t))
any(is.na(train_y_prior))
any(is.na(train_x))
any(is.infinite(train_y_ptiot))

# fit ridge regression to tune lambda -------------------------------------

library(glmnet)

init_ridge_fit <- glmnet(train_x, train_y_prior, alpha = 0, weights = t, lambda = 1e6)
plot(init_ridge_fit, xvar = "lambda")

set.seed(2058)
fit_ridge_cv <- cv.glmnet(train_x, train_y_prior, alpha = 0, weights = t, 
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
  filter(lambda == fit_ridge_cv$lambda.1se) %>%
  select(term, estimate)
  

rating <- prior %>%
  select(-weighted_pred) %>%
  left_join(ridge_coef, by = c("Player" = "term")) %>%
  mutate(rating = scaled_pred + estimate)



# fit ridge model with lambda 25000 picked --------------------------------

ridge_fit <- glmnet(train_x, train_y_prior, alpha = 0, weights = t,
                    lambda = 25000, standardize = FALSE)

tidy_ridge_coef2 <- tidy(ridge_fit$beta)

ridge_coef <- tidy_ridge_coef2 %>%
  select(row, value)


rating <- prior %>%
  select(-weighted_pred) %>%
  left_join(ridge_coef, by = c("Player" = "row")) %>%
  mutate(rating = scaled_pred + value) %>%
  mutate(rating_scaled = as.numeric(scale(rating)))


# save data ---------------------------------------------------------------

write_csv(rating, "Final Project/data/eng1_2122_apm_rating.csv")

