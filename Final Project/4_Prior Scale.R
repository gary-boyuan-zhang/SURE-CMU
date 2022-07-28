
# load package ------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

team <- read_csv("Final Project/data/teamratings.csv")


# fit lm ------------------------------------------------------------------

response <- (team$`xGD/90`) / 11

sd(response)

lm_model <- lm(response ~ team$`FIFA OVR`)
summary(lm_model)


coef <- lm_model$coefficients
new <- prior$weighted_pred * coef[2] + coef[1]
view(new)

mean(new)
sd(new)

#
coef
(Intercept) team$`FIFA OVR` 
-1.14597620      0.01460831 
#

as_tibble(new) %>%
  ggplot(aes(x = value)) +
  geom_histogram()

