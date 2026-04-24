#Step 1: Load packages
library(nflreadr)
library(dplyr)

#Step 2: Load schedule data (example: multiple seasons)
games <- load_schedules(2022:2024)

#Step 3: Create the table with desired columns
games_table <- games %>%
  select(season, game_id, home_team, away_team, home_score, away_score) %>%
  mutate(
    game_outcome = case_when(
      home_score > away_score ~ paste(home_team, "win"),
      home_score < away_score ~ paste(away_team, "win"),
      TRUE ~ "Tie"
    )
  ) %>%
  select(season, game_id, home_team, away_team, game_outcome)

#Step 4: View the table
print(games_table)



#loads schedules
games <- load_schedules(2022:2024)

#calculates away win percentage
away_win_pct <- games %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%  #removes games without scores
  mutate(away_win = away_score > home_score) %>%
  summarise(
    total_games = n(),
    away_wins = sum(away_win),
    away_win_percentage = away_wins / total_games
  )