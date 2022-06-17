# Questions:
1. Shot location: compare b/w L&R; heatmap
2. Shot type: relationship w/ shot distance, shot angle, shot speed; histogram, boxplot, cluster
3. Home team advantage: team level comparison, which team has the greatest diff b/w home and away?
4. Score/shot time distribution: compare b/w home & away 


# load data ---------------------------------------------------------------

library(tidyverse)

playoff_shot_data <- read_csv("nhl_playoffs_shots_2022.csv")

summary(playoff_shot_data)

table(playoff_shot_data)


# missing data ------------------------------------------------------------


any(is.na(playoff_shot_data))

na_shot_data <- playoff_shot_data[!complete.cases(playoff_shot_data),]

#missing data are all in goalieNameForShot column w/ 69 obs. 
 
unique(playoff_shot_data$shotType)

table(playoff_shot_data$shotType)



# shot type: shot distance & shot angle -----------------------------------

  



