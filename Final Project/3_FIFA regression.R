
# load packages -----------------------------------------------------------

library(tidyverse)
library(caret)

# import data -------------------------------------------------------------

fifa22 <- read_csv("Final Project/FIFA22_official_data.csv")
pl_cb <- read_csv("Final Project/pl_cb.csv")


# clean data --------------------------------------------------------------

  # see python notebook

pl_cb_fifa22 <- read_csv("Final Project/data/pl_cb_fifa22.csv")

# clean new data ----------------------------------------------------------

pl_cb_fifa22 <- pl_cb_fifa22 %>%
  select(-Season_End_Year, -Squad, -Comp, -Player, -Nation, -TmPos, -ID)

which(colSums(is.na(pl_cb_fifa22)) > 0)
#SoT_percent_Standard     G_per_Sh_Standard    G_per_SoT_Standard         Dist_Standard 
#31                    34                    35                    36 
#npxG_per_Sh_Expected Succ_percent_Dribbles       Mn_per_Sub_Subs 
#40                   137                   161 

pl_cb_fifa22 <- pl_cb_fifa22 %>%
  select(-Dist_Standard, -Mn_per_Sub_Subs) %>%
  mutate_all(~replace(., is.na(.), 0))

which(colSums(is.na(pl_cb_fifa22)) > 0)

# Pcr ---------------------------------------------------------------------

  #pcr
set.seed(2022)
cv_model_pcr <- train(
  Overall ~ .,
  data = pl_cb_fifa22,
  method = 'pcr',
  trControl = 
    trainControl(method = 'cv', number = 10,
                 selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(pl_cb_fifa22) - 125)
ggplot(cv_model_pcr) + theme_bw()



summary(cv_model_pcr$finalModel)
cv_model_pcr$finalModel$coefficients


# pls ---------------------------------------------------------------------

set.seed(2022)
cv_model_pls <- train(
  Overall ~ .,
  data = pl_cb_fifa22,
  method = 'pls',
  trControl = 
    trainControl(method = 'cv', number = 10,
                 selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(pl_cb_fifa22) - 125)
ggplot(cv_model_pls) + theme_bw()

summary(cv_model_pls$finalModel)
summary(cv_model_pls)
cv_model_pls$results
cv_model_pls$


  ### Interpret PLS result

coef <- cv_model_pls$finalModel$coefficients
#coef <- as_tibble(coef)
view(coef)

weight <- cv_model_pls$finalModel$loading.weights
view(weight)

predictions <- predict(cv_model_pls$finalModel)
view(predictions)



# variable importance -----------------------------------------------------

library(vip)

vip(cv_model_pls, num_features = 20, method = "model") +
  theme_bw()



# try w/ reduced variables ------------------------------------------------

pl_cb_reduced <- read_csv("Final Project/data/pl_cb_reduced.csv")

pl_cb_reduced <- pl_cb_reduced %>%
  select(-Season_End_Year, -Squad, -Comp, -Player, -Nation, -TmPos)

pl_cb_reduced$overall <- pl_cb_fifa22$Overall

which(colSums(is.na(pl_cb_reduced)) > 0)

pl_cb_reduced <- pl_cb_reduced %>%
  mutate_all(~replace(., is.na(.), 0))

set.seed(2022)
cv_reduced_pls <- train(
  overall ~ .,
  data = pl_cb_reduced,
  method = 'pls',
  trControl = 
    trainControl(method = 'cv', number = 10,
                 selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(pl_cb_reduced) - 1)
ggplot(cv_reduced_pls) + theme_bw()

summary(cv_reduced_pls$finalModel)

reduced_predictions <- predict(cv_reduced_pls$finalModel)
view(reduced_predictions)

view(cv_reduced_pls$finalModel$loading.weights)


# test weirdname ----------------------------------------------------------

wn <- read_csv('Final Project/data/weirdname.csv')

set.seed(2022)
cv_wn_pls <- train(
  Overall ~ .,
  data = wn,
  method = 'pls',
  trControl = 
    trainControl(method = 'cv', number = 10),
                 #selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(wn) - 1)
ggplot(cv_wn_pls) + theme_bw()

summary(cv_wn_pls$finalModel)
view(predict(cv_wn_pls$finalModel))
view(cv_wn_pls$finalModel$loading.weights)



# try lm w/ reduced df ----------------------------------------------------

init_reg_fit <- lm(overall ~ ., pl_cb_reduced)
library(broom)
tidy(init_reg_fit) %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  coord_flip() + theme_bw()

lm_preds <- predict(init_reg_fit, newdata = select(pl_cb_reduced, -overall))
view(lm_preds)


# try ridge w/ reduced df -------------------------------------------------

library(glmnet)

model_X = pl_cb_reduced %>% select(-overall) %>% as.matrix()
model_y = pl_cb_reduced$overall

fit_ridge_cv <- cv.glmnet(model_X, model_y, alpha = 0)
plot(fit_ridge_cv)

tidy_ridge_cv <- tidy(fit_ridge_cv)
tidy_ridge_cv %>%
  ggplot(aes(x = lambda, y = estimate)) +
  geom_line() + scale_x_log10() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .25) +
  geom_vline(xintercept = fit_ridge_cv$lambda.min) +
  geom_vline(xintercept = fit_ridge_cv$lambda.1se,
             linetype = "dashed", color = "red") +
  theme_bw()

tidy_ridge_coef <- tidy(fit_ridge_cv$glmnet.fit)
tidy_ridge_coef %>%
  filter(lambda == fit_ridge_cv$lambda.1se) %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  coord_flip() + theme_bw()

ridge_preds <- predict(fit_ridge_cv, s = fit_ridge_cv$lambda.min, newx = model_X)
view(ridge_preds)

