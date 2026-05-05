library(nflreadr)
library(dplyr)

# Load ALL available seasons
games <- load_schedules(seasons = TRUE)

# Keep only what you need
simple_table <- games %>%
  select(game_id, home_team, away_team)

#View it
View(simple_table)