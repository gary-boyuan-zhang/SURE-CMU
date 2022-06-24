

# load package ------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(cowplot)


# load data ---------------------------------------------------------------

playoff_shot_data <- read_csv("nhl_playoffs_shots_2022.csv")

summary(playoff_shot_data)

table(playoff_shot_data)

# shot/score time distribution ---------------------------------------------------------

# shot in regular time, histogram, w/ every 60s

playoff_shot_data %>%
  filter(time < 3600) %>%
  ggplot(aes(x = time)) +
  geom_histogram(binwidth = 60,
                 center = 30, closed = "left") +
  theme_bw()

# score in regular time, histogram, w/ every 120s

table(playoff_shot_data$event)

playoff_shot_data %>%
  filter(time < 3600, event == "GOAL") %>%
  ggplot(aes(x = time)) +
  geom_histogram(binwidth = 120,
                 center = 60, closed = 'left') +
  theme_bw()

# ecdf

playoff_shot_data %>%
  filter(time < 3600) %>%
  ggplot(aes(x = time, color = event)) +
  stat_ecdf(size = 0.65) +
  theme_bw() +
  scale_color_colorblind() +
  theme(legend.position = 'bottom')

# kde

playoff_shot_data %>%
  filter(time < 3600) %>%
  ggplot(aes(x = time, color = event)) +
  geom_density(size = 1) +
  geom_rug(alpha = 0.05) +
  theme_bw() +
  scale_color_colorblind() +
  theme(legend.position = 'bottom')

playoff_shot_data %>%
  filter(time < 3600) %>%
  ggplot(aes(x = time)) +
  geom_density() +
  geom_rug(alpha = 0.2) +
  theme_bw()

playoff_shot_data %>%
  filter(time < 3600, event == 'GOAL') %>%
  ggplot(aes(x = time)) +
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw()

# kde with home&away

playoff_shot_data %>%
  filter(time < 3600, event == 'GOAL') %>%
  ggplot(aes(x = time, color = as.character(isHomeTeam))) +
  geom_density(adjust = 0.7) +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  theme(legend.position = 'bottom')


# goal's ecdf&kde together w/ home & away

library(patchwork)

goal_dens <- playoff_shot_data %>%
  filter(time < 3600, event == 'GOAL') %>%
  ggplot(aes(x = time, color = as.character(isHomeTeam))) +
  geom_density(adjust = 0.7, size = 1) +
  geom_rug(alpha = 0.3) +
  geom_vline(xintercept = c(1200, 2400), linetype = 'twodash') +
  theme_bw() +
  labs(x = 'Time into the game (in seconds)',
       y = 'Number of Goals',
       title = 'Density Curve of Goal Time') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = '', labels = c('Away', 'Home')) 

goal_ecdf <- playoff_shot_data %>%
  filter(time < 3600, event == 'GOAL') %>%
  ggplot(aes(x = time, color = as.character(isHomeTeam))) +
  stat_ecdf(size = 0.8) +
  geom_rug(alpha = 0.3) +
  geom_vline(xintercept = c(1200, 2400), linetype = 'twodash') +
  theme_bw() +
  labs(x = 'Time into the game (in seconds)',
       y = 'Proportion of Goals',
       title = 'Empirical Cumulative Distribution of Goal Time') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = '', labels = c('Away', 'Home')) 

goal_dens + goal_ecdf #+ 
  #plot_layout(guides = 'collect') & theme(legend.position = 'bottom')



# shot's ecdf&kde together w/ home & away

shot_dens <- playoff_shot_data %>%
  filter(time < 3600) %>%
  ggplot(aes(x = time, color = as.character(isHomeTeam))) +
  geom_density(adjust = 0.7, size = 1) +
  geom_rug(alpha = 0.3) +
  geom_vline(xintercept = c(1200, 2400), linetype = 'twodash') +
  theme_bw() +
  labs(x = 'Time into the game (in seconds)',
       y = 'Number of Shots',
       title = 'Density Curve of Shot Time') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = '', labels = c('Away', 'Home')) 

shot_ecdf <- playoff_shot_data %>%
  filter(time < 3600) %>%
  ggplot(aes(x = time, color = as.character(isHomeTeam))) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  geom_vline(xintercept = c(1200, 2400), linetype = 'twodash') +
  theme_bw() +
  labs(x = 'Time into the game (in seconds)',
       y = 'Proportion of Shots',
       title = 'Empirical Cumulative Distribution of Shot Time') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = '', labels = c('Away', 'Home')) 

shot_dens + shot_ecdf

plot_grid(goal_dens, goal_ecdf, 
          shot_dens, shot_ecdf,
          ncol = 2)

# ongoal's ecdf&kde together w/ home & away

ongoal_dens <- playoff_shot_data %>%
  filter(time < 3600, event != 'MISS') %>%
  ggplot(aes(x = time, color = as.character(isHomeTeam))) +
  geom_density(adjust = 0.7, size = 1) +
  geom_rug(alpha = 0.3) +
  geom_vline(xintercept = c(1200, 2400), linetype = 'twodash') +
  theme_bw() +
  labs(x = 'Time into the game (in seconds)',
       y = 'Number of Shot on Goals',
       title = 'Density Curve of Shot On Goal Time') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = '', labels = c('Away', 'Home')) 

ongoal_ecdf <- playoff_shot_data %>%
  filter(time < 3600, event != 'MISS') %>%
  ggplot(aes(x = time, color = as.character(isHomeTeam))) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  geom_vline(xintercept = c(1200, 2400), linetype = 'twodash') +
  theme_bw() +
  labs(x = 'Time into the game (in seconds)',
       y = 'Proportion of Shot on Goals',
       title = 'Empirical Cumulative Distribution of Shot on Goal Time') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = '', labels = c('Away', 'Home')) 

ongoal_dens + ongoal_ecdf


plot_grid(goal_dens, goal_ecdf, 
          shot_dens, shot_ecdf,
          ncol = 2)


# team level summary ------------------------------------------------------


  # team level summary
  
team_summary <- playoff_shot_data %>%
    filter(time < 3600) %>%
    group_by(isHomeTeam) %>%
    summarise(goal = sum(event == 'GOAL'),
              shot = n()) %>%
    mutate(isHomeTeam = as.character(isHomeTeam))

  # side by side bar

team_summary_goal_bar <- team_summary %>%
  ggplot(aes(x = isHomeTeam)) +
  geom_bar(aes(y = goal),
           stat = 'identity') +
  scale_color_colorblind() +
  theme_bw()

team_summary_shot_bar <- team_summary %>%
  ggplot(aes(x = isHomeTeam)) +
  geom_bar(aes(y = shot),
           stat = 'identity') +
  theme_bw()

team_summary_shot_bar + team_summary_goal_bar

  # side by side pie

team_summary_goal_pie <- team_summary %>%
  ggplot(aes(x = '', fill = isHomeTeam)) +
  geom_bar(aes(y = goal),
           stat = 'identity') +
  coord_polar('y') +
  scale_color_colorblind() +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title.y = element_blank())

team_summary_shot_pie <- team_summary %>%
  ggplot(aes(x = '', fill = isHomeTeam)) +
  geom_bar(aes(y = shot),
           stat = 'identity') +
  coord_polar('y') +
  theme_bw()

team_summary_shot_pie + team_summary_goal_pie + plot_layout(guides = 'collect')


plot_grid(team_summary_shot_bar, team_summary_goal_bar,
          team_summary_shot_pie, team_summary_goal_pie,
          ncol = 2)

# overtime ----------------------------------------------------------------

# shot in over time, histogram, w/ every 60s

playoff_shot_data %>%
  filter(time > 3600) %>%
  ggplot(aes(x = time)) +
  geom_histogram(binwidth = 60,
                 center = 30, closed = "left") +
  theme_bw()

# score in over time, histogram, w/ every 120s

table(playoff_shot_data$event)

playoff_shot_data %>%
  filter(time > 3600, event == "GOAL") %>%
  ggplot(aes(x = time)) +
  geom_histogram(binwidth = 120,
                 center = 60, closed = 'left') +
  theme_bw()

# ecdf

overtime_ecdf <- playoff_shot_data %>%
  filter(time > 3600) %>%
  ggplot(aes(x = time, color = event)) +
  stat_ecdf() +
  geom_rug(alpha = 0.5) +
  theme_bw() +
  theme(legend.position = 'bottom')

# kde

overtime_dens <- playoff_shot_data %>%
  filter(time > 3600) %>%
  ggplot(aes(x = time, color = event)) +
  geom_density() +
  geom_rug(alpha = 0.5) +
  theme_bw() +
  theme(legend.position = 'bottom')

# put together
overtime_dens + overtime_ecdf



# overtime shot & goal ----------------------------------------------------

overtime_summary <- playoff_shot_data %>%
  filter(time > 3600) %>%
  group_by(isHomeTeam) %>%
  summarise(goal = sum(event == 'GOAL'),
            shot = n()) %>%
  mutate(isHomeTeam = as.character(isHomeTeam))

  # bar

overtime_summary_goal_bar <- overtime_summary %>%
  ggplot(aes(x = isHomeTeam)) +
  geom_bar(aes(y = goal),
           stat = 'identity') +
  theme_bw()

overtime_summary_shot_bar <- overtime_summary %>%
  ggplot(aes(x = isHomeTeam)) +
  geom_bar(aes(y = shot),
           stat = 'identity') +
  theme_bw()

overtime_summary_shot_bar + overtime_summary_goal_bar

  # pie

overtime_summary_goal_pie <- overtime_summary %>%
  ggplot(aes(x = '', fill = isHomeTeam)) +
  geom_bar(aes(y = goal),
           stat = 'identity') +
  coord_polar('y') +
  theme_bw()

overtime_summary_shot_pie <- overtime_summary %>%
  ggplot(aes(x = '', fill = isHomeTeam)) +
  geom_bar(aes(y = shot),
           stat = 'identity') +
  coord_polar('y') +
  theme_bw()

overtime_summary_shot_pie + overtime_summary_goal_pie

plot_grid(overtime_summary_shot_bar, overtime_summary_goal_bar,
          overtime_summary_shot_pie, overtime_summary_goal_pie,
          ncol = 2)

