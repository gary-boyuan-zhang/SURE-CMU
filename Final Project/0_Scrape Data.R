
# Load Package ------------------------------------------------------------

library(worldfootballR)
library(tidyverse)
library(janitor)
library(broom)

# start scrapping ---------------------------------------------------------

mapped_players <- player_dictionary_mapping()

standard <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                          stat_type= "standard",
                                          team_or_player= "player")
shooting <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                          stat_type= "shooting",
                                          team_or_player= "player")
passing <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                         stat_type= "passing",
                                         team_or_player= "player")
passing_types <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                         stat_type= "passing_types",
                                         team_or_player= "player")
gca <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                     stat_type= "gca",
                                     team_or_player= "player")
defense <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                         stat_type= "defense",
                                         team_or_player= "player")
possession <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                            stat_type= "possession",
                                            team_or_player= "player")
playing_time <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                              stat_type= "playing_time",
                                              team_or_player= "player")
misc <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                      stat_type= "misc",
                                      team_or_player= "player")
keepers <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                         stat_type= "keepers",
                                         team_or_player= "player")
keepers_adv <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                             stat_type= "keepers_adv",
                                             team_or_player= "player")


# Aggregate Premeir League Player Data ------------------------------------


pl_all <- standard %>%
  filter(Comp == "Premier League") %>%
  rename("UrlFBref" = Url) %>%
  left_join(mapped_players) %>%
  left_join(shooting) %>%
  left_join(passing) %>%
  left_join(passing_types) %>%
  left_join(gca) %>%
  left_join(defense) %>%
  left_join(possession) %>%
  left_join(playing_time) %>%
  left_join(misc) %>%
  left_join(keepers) %>%
  left_join(keepers_adv)

pl_nongk <- standard %>%
  filter(Comp == "Premier League", Pos != "GK") %>%
  rename("UrlFBref" = Url) %>%
  left_join(mapped_players) %>%
  left_join(shooting) %>%
  left_join(passing) %>%
  left_join(passing_types) %>%
  left_join(gca) %>%
  left_join(defense) %>%
  left_join(possession) %>%
  left_join(playing_time) %>%
  left_join(misc)

pl_gk <- pl_all %>%
  filter(Pos == "GK")

pl_cb <- pl_nongk %>%
  filter(TmPos == "Centre-Back", Min_Playing >= 600) %>%
  select(-UrlFBref, -UrlTmarkt, -Url, -PlayerFBref, -Pos, -Born)
  

# Exploratory PCA ---------------------------------------------------------

model_cb <- pl_cb %>%
  select_if(is.numeric) %>%
  select(-Season_End_Year) %>%
  remove_constant(na.rm = TRUE) %>%
  as.matrix()
model_cb <- t(na.omit(t(model_cb))) %>% as_tibble()

pca_cb <- prcomp(model_cb, center = TRUE, scale = TRUE)
summary(pca_cb)

pca_cb %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1 / ncol(model_cb),
             color = "darkred",
             linetype = "dashed") +
  theme_bw()

loadings_cb <- pca_cb %>%
  tidy(matrix = "loadings") %>%
  filter(PC <= 7) %>%
  group_by(column) %>%
  mutate(tot_abs_value = sum(abs(value))) %>%
  ungroup() %>%
  #arrange(by = desc(tot_abs_value)) %>%
  filter(PC == 1) %>%
  select(column, tot_abs_value)
  
weights_cb <- t(loadings_cb) %>%
  row_to_names(row_number = 1) %>%
  as_tibble()

ratings_cb <- list()
for (i in seq(1, nrow(pl_cb))) {
  ratings_cb[i] <- rowSums(slice(model_cb, i) * as.numeric(weights_cb))
}
ratings_cb <- ratings_cb %>%
  as_tibble_col(column_name = "Rating") %>%
  mutate(Player = pl_cb$Player, Rating = as.numeric(Rating)) %>%
  select(Player, Rating) %>%
  arrange(by = desc(Rating))







summary(pl_cb$Min_Playing)
pl_cb %>%
  ggplot(aes(x = Min_Playing)) +
  stat_ecdf() +
  geom_rug(alpha = 0.5) +
  geom_vline(xintercept = 600, color = 'red') +
  geom_vline(xintercept = 1000, color = 'red') +
  theme_bw()








# save data ---------------------------------------------------------------

write_csv(pl_all, "Final Project/pl_all.csv")
write_csv(pl_nongk, "Final Project/pl_nongk.csv")
write_csv(pl_cb, "Final Project/pl_cb.csv")
write_csv(ratings_cb, "Final Project/ratings_cb.csv")
