
# load data ---------------------------------------------------------------

library(tidyverse)

playoff_shot_data <- read_csv("nhl_playoffs_shots_2022.csv")

summary(playoff_shot_data)

table(playoff_shot_data)

colnames(playoff_shot_data)

# team data summary -------------------------------------------------------

 # data aggregation

team_data <- playoff_shot_data %>%
  filter(time < 3600) %>%
  group_by(teamCode) %>% 
  summarise(total_shot = n(),
         total_score = sum(event == 'GOAL'),
         total_ongoal = sum(event != 'MISS'),
         ngames = n_distinct(game_id)) %>%
  mutate(shot_pg = total_shot / ngames,
         score_pg = total_score / ngames,
         ongoal_pg = total_ongoal / ngames)

concede_data <- playoff_shot_data %>%
  mutate(opponent = ifelse(teamCode == homeTeamCode, awayTeamCode, homeTeamCode)) %>%
  filter(time < 3600) %>%
  group_by(opponent) %>%
  summarise(total_shot_concede = n(),
            total_score_concede = sum(event == 'GOAL'),
            total_ongoal_concede = sum(event != 'MISS'))

team_data <- team_data %>%
  inner_join(concede_data, by = c('teamCode' = 'opponent')) %>%
  mutate(shot_con_pg = total_shot_concede / ngames,
         score_con_pg = total_score_concede / ngames,
         ongoal_con_pg = total_ongoal_concede / ngames)


 # plot shot_pg vs. shot_con_pg

team_shot <- team_data %>%
  ggplot(aes(x = shot_pg, y = shot_con_pg,
             label = teamCode,
             size = abs(shot_pg - shot_con_pg))) +
  geom_point() +
  geom_text(hjust = 0.8, vjust = 1.2) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  coord_fixed() +
  theme(legend.position = 'bottom')

# plot score_pg vs. score_con_pg

team_score <- team_data %>%
  ggplot(aes(x = score_pg, y = score_con_pg,
             label = teamCode,
             size = abs(score_pg - score_con_pg))) +
  geom_point() +
  geom_text(hjust = 0.8, vjust = 1.2) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  coord_fixed() +
  theme(legend.position = 'bottom')

# plot ongoal_pg vs. ongoal_con_pg

team_ongoal <- team_data %>%
  ggplot(aes(x = ongoal_pg, y = ongoal_con_pg,
             label = teamCode,
             size = abs(ongoal_pg - ongoal_con_pg))) +
  geom_point() +
  geom_text(hjust = 0.8, vjust = 1.2) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  coord_fixed() +
  theme(legend.position = 'bottom')

team_shot + team_ongoal + team_score
