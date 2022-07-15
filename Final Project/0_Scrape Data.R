
# Load Package ------------------------------------------------------------

library(worldfootballR)
library(tidyverse)


# start scrapping ---------------------------------------------------------

standard <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                          stat_type= "standard",
                                          team_or_player= "player")
shooting <- fb_big5_advanced_season_stats(season_end_year= 2022,
                                          stat_type= "shooting",
                                          team_or_player= "player")
passing <- fb_big5_advanced_season_stats(season_end_year= 2022,
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
  left_join(shooting) %>%
  left_join(passing) %>%
  left_join(gca) %>%
  left_join(defense) %>%
  left_join(possession) %>%
  left_join(playing_time) %>%
  left_join(misc) %>%
  left_join(keepers) %>%
  left_join(keepers_adv)

pl_gk <- pl_all %>%
  filter(Pos == "GK")

pl_df <- standard %>%
  filter(Comp == "Premier League", Pos == "DF") %>%
  left_join(shooting) %>%
  left_join(passing) %>%
  left_join(gca) %>%
  left_join(defense) %>%
  left_join(possession) %>%
  left_join(playing_time) %>%
  left_join(misc)



# DF DF,FW DF,MF    FW FW,DF FW,MF    GK    MF MF,DF MF,FW 
#185     4     5    83     2    59    42   116    11    39 


pl_all %>% 
  filter(Pos == 'DF,FW') %>%
  select(Player, Min_Playing)
#Player Min_Playing
#1   Ashley Young        1250
#2  Tariq Lamptey        1561
#3    Solly March        1746
#4 Arthur Masuaku         650

pl_all %>% 
  filter(Pos == 'FW,DF') %>%
  select(Player, Min_Playing)
#Player Min_Playing
#1 Marc Albrighton        1132
#2    Jacob Murphy        1473


pl_all %>% 
  filter(Pos == 'DF,MF') %>%
  select(Player, Min_Playing)
#Player Min_Playing
#1    Saman Ghoddos         536
#2    Stuart Dallas        2919
#3 Jamie Shackleton         709
#4  Hamza Choudhury         285
#5        Chiquinho         208

pl_all %>% 
  filter(Pos == 'MF,DF') %>%
  select(Player, Min_Playing)
#Player Min_Playing
#1           Granit Xhaka        2327
#2     Pascal Gro<U+00DF>        2038
#3     Ruben Loftus-Cheek        1394
#4     Saúl <U+00D1>íguez         480
#5             Alex Iwobi        2035
#6             Robin Koch        1574
#7          Wilfred Ndidi        1618
#8           James Milner         853
#9            Fernandinho         964
#10          Isaac Hayden        1002
#11 Jakob S<U+00F8>rensen         604

pl_all %>% 
  filter(Pos == 'MF,FW') %>%
  select(Player, Min_Playing)

pl_all %>% 
  filter(Pos == 'FW,MF') %>%
  select(Player, Min_Playing)


# save data ---------------------------------------------------------------

write_csv(pl_all, "Final Project/pl_all.csv")
write_csv(pl_gk, "Final Project/pl_gk.csv")
