
# load package ------------------------------------------------------------

library(tidyverse)
library(worldfootballR)


# scrape data -------------------------------------------------------------

team_urls <- understat_team_meta(team_name = c("Liverpool", "Manchester City"))

wba_liv_shots <- understat_match_shots(match_url = "https://understat.com/match/14789")
