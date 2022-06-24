

# load package ------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)


# load data ---------------------------------------------------------------


playoff_shot_data <- read_csv("nhl_playoffs_shots_2022.csv")

summary(playoff_shot_data)

table(playoff_shot_data)



# shot type effiencency / rate --------------------------------------------

table(playoff_shot_data$shotType)

shot_type <- playoff_shot_data %>%
  group_by(shotType) %>%
  summarise(total_shot = n(),
            total_ongoal = sum(event != 'MISS'),
            total_score = sum(event == 'GOAL')) %>%
  mutate(ongoal_pct = total_ongoal / total_shot,
         score_pct = total_score / total_shot)


# visualizing shot type  ---------------------------------------------------------------

  # bar chart

shot_type_shot_bar <- shot_type %>%
  mutate(shotType = fct_reorder(shotType, total_shot, .desc = TRUE)) %>%
  ggplot(aes(x = shotType)) +
  geom_bar(aes(y = total_shot),
           stat = 'identity') +
  theme_bw()


  # pie chart

playoff_shot_data %>%
  ggplot(aes(x = '', fill = shotType)) +
  geom_bar() +
  coord_polar('y') +
  scale_color_colorblind()



# shot type w/ shot & score side by side ----------------------------------

  # pie chart

shot_type_shot_pie <- shot_type %>%
  ggplot(aes(x = '', fill = shotType)) +
  geom_bar(aes(y = total_shot),
           stat = 'identity') +
  coord_polar('y') +
  theme_bw()

shot_type_score_pie <- shot_type %>%
  ggplot(aes(x = '', fill = shotType)) +
  geom_bar(aes(y = total_score),
           stat = 'identity') +
  coord_polar('y') +
  theme_bw()

shot_type_shot_pie + shot_type_score_pie


  # bar 

shot_type_score_bar <- shot_type %>%
  mutate(shotType = fct_reorder(shotType, total_shot, .desc = TRUE)) %>%
  ggplot(aes(x = shotType)) +
  geom_bar(aes(y = total_score),
           stat = 'identity') +
  theme_bw()

shot_type_shot_bar + shot_type_score_bar


# shot ongoal -------------------------------------------------------------

  # pie

shot_type_ongoal_pie <- shot_type %>%
  ggplot(aes(x = '', fill = shotType)) +
  geom_bar(aes(y = total_ongoal),
           stat = 'identity') +
  coord_polar('y') +
  theme_bw()

shot_type_shot_pie + shot_type_ongoal_pie  + shot_type_score_pie

  # bar

shot_type_ongoal_bar <- shot_type %>%
  mutate(shotType = fct_reorder(shotType, total_shot, .desc = TRUE)) %>%
  ggplot(aes(x = shotType)) +
  geom_bar(aes(y = total_ongoal),
           stat = 'identity') +
  theme_bw()


# score rate bar ----------------------------------------------------------

shot_type_score_pct_bar <- shot_type %>%
  mutate(shotType = fct_reorder(shotType, total_shot, .desc = TRUE)) %>%
  ggplot(aes(x = shotType)) +
  geom_bar(aes(y = score_pct),
           stat = 'identity') +
  theme_bw()

shot_type_shot_bar + shot_type_score_bar  + shot_type_score_pct_bar


# ongoal percantage bar ---------------------------------------------------

shot_type_ongoal_pct_bar <- shot_type %>%
  mutate(shotType = fct_reorder(shotType, total_shot, .desc = TRUE)) %>%
  ggplot(aes(x = shotType)) +
  geom_bar(aes(y = ongoal_pct),
           stat = 'identity') +
  theme_bw()

shot_type_shot_bar + shot_type_score_bar + shot_type_ongoal_bar + 
  shot_type_score_pct_bar + shot_type_ongoal_pct_bar






