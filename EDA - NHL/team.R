
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




# hcluster ----------------------------------------------------------------

  # shot_pg vs. shot_con_pg

    ## construct team_shot_dist & team_shot_dist_matrix

team_shot_dist <- team_data %>%
  select(shot_pg, shot_con_pg) %>%
  dist()

team_shot_dist_matrix <- team_shot_dist %>%
  as.matrix()

rownames(team_shot_dist_matrix) <- team_data$teamCode
colnames(team_shot_dist_matrix) <- team_data$teamCode

head(team_shot_dist_matrix)

    ## pivot longer to get team_shot_dist_table

team_shot_dist_table <- team_shot_dist_matrix %>%
  as_tibble() %>%
  mutate(team1 = rownames(team_shot_dist_matrix)) %>%
  pivot_longer(cols = -team1,
               names_to = 'team2',
               values_to = 'dist')

    ## plot unordered distance heatmap

team_shot_dist_table %>%
  ggplot(aes(x = team1, y = team2,
             fill = dist)) +
  geom_tile() +
  theme_bw() +
  theme(legend.position = 'bottom') +
  scale_fill_gradient(low = 'darkorange',
                      high = 'darkblue')

    ## order dist table with serialization
library(seriation)

order <- team_shot_dist %>%
  seriate() %>%
  get_order()

team_shot_order <- team_data$teamCode[order]

    ## plot ordered distance heatmap

team_shot_dist_table %>%
  mutate(team1 = fct_relevel(team1, team_shot_order),
         team2 = fct_relevel(team2, team_shot_order)) %>%
  ggplot(aes(x = team1, y = team2,
             fill = dist)) +
  geom_tile() +
  theme_bw() +
  theme(legend.position = 'bottom') +
  scale_fill_gradient(low = 'darkorange',
                      high = 'darkblue')

    ## h cluster with minimax linkage

library(protoclust)

team_shot_minimax <- protoclust(team_shot_dist)

    ## protoclust dendrogram

library(ggdendro)

team_shot_dendro <- team_shot_minimax %>%
  ggdendrogram(theme_dendro = FALSE,
               labels = FALSE,
               leaf_labels = FALSE) +
  geom_hline(yintercept = 6, linetype = 'twodash', color = 'red') +
  labs(y = 'Maximum dissimilarity from prototype') +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

    ## protocut

team_shot_minimax_cluster <- team_shot_minimax %>%
  protocut(h = 6)

    ## plot protoclust result

team_shot_clustered <- team_data %>%
  mutate(team_clusters = as.factor(team_shot_minimax_cluster$cl)) %>%
  ggplot(aes(x = shot_pg, y = shot_con_pg,
             color = team_clusters,
             label = teamCode,
             #size = abs(shot_pg - shot_con_pg)
             )) +
  geom_point() + 
  geom_text(hjust = 0.5, vjust = 1.2) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = 'bottom')

team_shot_dendro + team_shot_clustered

