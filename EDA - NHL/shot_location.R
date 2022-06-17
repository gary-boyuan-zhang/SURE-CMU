
# load data ---------------------------------------------------------------

library(tidyverse)

playoff_shot_data <- read_csv("nhl_playoffs_shots_2022.csv")

summary(playoff_shot_data)

table(playoff_shot_data)

# shot location -----------------------------------------------------------

colnames(playoff_shot_data)
head(select(playoff_shot_data,arenaAdjustedXCord, arenaAdjustedYCord))

playoff_shot_data %>%
  ggplot(aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord)) +
  geom_point(alpha = 0.3) +
  geom_density2d() +
  theme_bw() +
  coord_fixed()

# convert into a half court by |X|

playoff_shot_data %>%
  #geom_hockey(league = "NHL") +
  ggplot(aes(x = abs(arenaAdjustedXCord), y = arenaAdjustedYCord)) +
  geom_point(alpha = 0.3) +
  geom_density2d(adjust = 1) +
  #geom_hockey(league = "NHL") +
  theme_bw() +
  coord_fixed()

# add hockey background

geom_hockey(league = 'NHL') +
  geom_point(data = playoff_shot_data,
             aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord),
             alpha = 0.3) +
  geom_density2d(adjust = 1) +
  theme_bw() +
  coord_fixed() 

geom_hockey(league = 'NHL', full_surf = FALSE) +
  geom_point(data = playoff_shot_data,
             aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord),
             alpha = 0.3,
             #size = 3,
  ) +
  geom_density2d(adjust = 1) +
  theme_bw() +
  coord_fixed() 


# shot location by left&right ---------------------------------------------


#heatmap

geom_hockey(league = 'NHL', full_surf = FALSE) +
  stat_density2d(data = playoff_shot_data,
                 adjust = 0.8,
                 #h = 1.2, bins = 40,
                 alpha = 0.3,
                 aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord, 
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() +
  coord_fixed() +
  facet_wrap(~ shooterLeftRight) +
  labs(title = 'Heat map of shots from L & R') +
  theme(legend.position = 'bottom',
        legend.text = element_blank(),
        plot.title = element_text(hjust = 0.5))

# relative x

relative_l <- geom_hockey(league = 'NHL', full_surf = FALSE) +
  stat_density2d(data = filter(playoff_shot_data, shooterLeftRight == 'L'),
                 adjust = 0.8,
                 #h = 1.2, bins = 40,
                 alpha = 0.3,
                 aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord, 
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() +
  coord_fixed() +
  theme(legend.position = 'bottom')

# relative y
relative_r <- geom_hockey(league = 'NHL', full_surf = FALSE) +
  stat_density2d(data = filter(playoff_shot_data, shooterLeftRight == 'R'),
                 adjust = 0.8,
                 #h = 1.2, bins = 40,
                 alpha = 0.3,
                 aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord, 
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() +
  coord_fixed()+
  theme(legend.position = 'bottom')

relative_l + relative_r

#cowplot plot_grid
get_legend()

table(playoff_shot_data$shooterLeftRight)


# shot location color by shot type

geom_hockey(league = 'NHL', full_surf = FALSE) +
  geom_point(data = playoff_shot_data,
             aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord,
                 color = shotType),
             alpha = 0.3
  ) +
  geom_density2d(adjust = 1) +
  scale_color_manual(name = "", values = c("WRIST" = "red", "WRAP" = "orange", "TIP" = "yellow","SNAP" = "green", 
                                           "SLAP" = "grey", "DEFL" = "blue", "BACK" = "purple")) +
  theme_bw() +
  coord_fixed() 

# shot location group by shot type

geom_hockey(league = 'NHL', full_surf = FALSE) +
  geom_point(data = playoff_shot_data,
             aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord),
             alpha = 0.3
  ) +
  geom_density2d(adjust = 1) +
  theme_bw() +
  coord_fixed() +
  facet_wrap(~ shotType, ncol = 4)

geom_hockey(league = 'NHL', full_surf = FALSE) +
  stat_density2d(data = playoff_shot_data,
                 adjust = 1,
                 #h = 1.2, bins = 40,
                 alpha = 0.3,
                 aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord, 
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() +
  coord_fixed() +
  facet_wrap(~ shotType, ncol = 4) +
  theme(legend.position = "bottom")

table(playoff_shot_data$shotType)


# goal location

geom_hockey(league = 'NHL', full_surf = FALSE) +
  stat_density2d(data = filter(playoff_shot_data, event == 'GOAL'),
                 adjust = 0.8,
                 #h = 1.2, bins = 40,
                 alpha = 0.3,
                 aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord, 
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() +
  coord_fixed() +
  facet_wrap(~ shooterLeftRight) +
  labs(title = 'Heat map of goals from L & R') +
  theme(legend.position = 'bottom',
        legend.text = element_blank(),
        plot.title = element_text(hjust = 0.5))

# shot & goal location: scatter

geom_hockey(league = 'NHL', full_surf = FALSE) +
  geom_point(data = playoff_shot_data,
             alpha = 0.3,
             aes(x = -abs(arenaAdjustedXCord), y = arenaAdjustedYCord)) +
  #color = event)) +
  theme_bw() +
  coord_fixed() +
  facet_grid(event ~ shooterLeftRight) +
  labs(title = 'Shots and Goals from L & R') +
  theme(legend.position = 'bottom',
        #legend.text = element_blank(),
        plot.title = element_text(hjust = 0.5))