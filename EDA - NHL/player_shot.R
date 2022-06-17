
# load data ---------------------------------------------------------------

library(tidyverse)

playoff_shot_data <- read_csv("nhl_playoffs_shots_2022.csv")

summary(playoff_shot_data)

table(playoff_shot_data)

# shot effiency: shot vs. score -------------------------------------------

# player stats from aggregate

player_data <- playoff_shot_data %>%
  group_by(shooterPlayerId, shooterName) %>%
  summarize(total_shot = n(),
            total_score = sum(event == 'GOAL'),
            total_ongoal = sum(!event == 'MISS')) %>%
  mutate(score_eff = total_score / total_shot,
         ongoal_pct = total_ongoal / total_shot)

# ecdf of total shot

player_data %>%
  ggplot(aes(x = total_shot)) +
  stat_ecdf() +
  theme_bw()

# ecdf of total score

player_data %>%
  ggplot(aes(x = total_score)) +
  stat_ecdf() +
  theme_bw()

# plot shot vs. score

player_data %>%
  filter(total_score > 0) %>%
  ggplot(aes(x = total_shot, y = total_score,
             label = shooterName)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = ifelse((total_shot > 75 & total_score > 8), 
                               as.character(shooterName), '')),
            hjust = 1, vjust = 1) +
  geom_smooth() +
  theme_bw()

# score_eff

player_data %>%
  filter(total_score > 3, total_shot > 10) %>%
  ggplot(aes(x = score_eff,
             label = shooterName)) +
  #geom_histogram(bins = 10) +
  geom_density() +
  geom_rug(alpha = 0.3) +
  #geom_text() +
  theme_bw()

# score_eff: kde & ecdf

library(patchwork)

player_eff_dens <- player_data %>%
  filter(total_score > 3, total_shot > 10) %>%
  ggplot(aes(x = score_eff,
             label = shooterName)) +
  geom_density(adjust = 0.7) +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'score_eff',
       y = 'Number of Goals')

player_eff_ecdf <- player_data %>%
  filter(total_score > 3, total_shot > 10) %>%
  ggplot(aes(x = score_eff,
             label = shooterName)) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'score_eff',
       y = 'proportion of Goals')


player_eff_dens + player_eff_ecdf # + plot_layout(guides = 'collect')


# on goal %

player_ongoal_dens <- player_data %>%
  filter(total_shot > 10) %>%
  ggplot(aes(x = ongoal_pct,
             label = shooterName)) +
  geom_density(adjust = 0.8) +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'ongoal_pct',
       y = 'Number of Goals')

player_ongoal_ecdf <- player_data %>%
  filter(total_shot > 10) %>%
  ggplot(aes(x = ongoal_pct,
             label = shooterName)) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'ongoal_pct',
       y = 'proportion of Goals')


player_ongoal_dens + player_ongoal_ecdf # + plot_layout(guides = 'collect')

# shot vs. ongoal

player_data %>%
  filter(total_shot > 10) %>%
  ggplot(aes(x = total_shot, y = total_ongoal,
             label = shooterName)) +
  geom_point(alpha = 0.5) +
  geom_text(#aes(label = ifelse((total_shot > 75 & total_score > 8), 
    #                  as.character(shooterName), '')),
    hjust = 1, vjust = 1) +
  geom_smooth() +
  theme_bw()

# ongoal vs. score

player_data <- player_data %>%
  #filter(total_shot > 10) %>%
  mutate(std_total_score = as.numeric(scale(total_score, center = FALSE)),
         std_total_ongoal = as.numeric(scale(total_ongoal, center = FALSE)))

player_data <- player_data %>%
  #filter(total_shot > 10) %>%
  mutate(std_total_score = (total_score - mean(total_score, na.rm = TRUE)) 
         / sd(total_score, na.rm = TRUE),
         std_total_ongoal = (total_ongoal - mean(total_ongoal, na.rm = TRUE)) 
         / sd(total_ongoal, na.rm = TRUE))


player_data %>%
  filter(total_shot >10) %>%
  ggplot(aes(x = std_total_ongoal, y = std_total_score,
             label = shooterName)) +
  geom_point(alpha = 0.5) +
  geom_text(#aes(label = ifelse((total_shot > 75 & total_score > 8), 
    #                  as.character(shooterName), '')),
    hjust = 1, vjust = 1) +
  geom_smooth() +
  theme_bw()
