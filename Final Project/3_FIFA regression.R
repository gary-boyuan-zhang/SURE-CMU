
# load packages -----------------------------------------------------------

library(tidyverse)
library(caret)

# import data -------------------------------------------------------------

fifa22 <- read_csv("Final Project/FIFA22_official_data.csv")
pl_cb <- read_csv("Final Project/pl_cb.csv")


# clean data --------------------------------------------------------------

  # see python notebook

pl_cb_fifa22 <- read_csv("Final Project/pl_cb_fifa22.csv")

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

