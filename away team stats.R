library(nflreadr)
library(dplyr)

# Load data
games <- load_schedules(2023)
pbp <- load_pbp(2023)

# Step 1: Game outcome
games_clean <- games %>%
  select(game_id, home_team, away_team, home_score, away_score) %>%
  mutate(
    game_outcome = case_when(
      away_score > home_score ~ "Away Win",
      away_score < home_score ~ "Away Loss",
      TRUE ~ "Tie"
    )
  )

# Step 2: Aggregate stats (by team per game)
pbp_summary <- pbp %>%
  filter(!is.na(posteam)) %>%
  group_by(game_id, posteam) %>%
  summarise(
    completion_pct = mean(complete_pass, na.rm = TRUE),
    total_yards = sum(yards_gained, na.rm = TRUE),
    avg_yards = mean(yards_gained, na.rm = TRUE),
    total_epa = sum(epa, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Join ONLY away team stats
away_table <- games_clean %>%
  left_join(pbp_summary, by = c("game_id", "away_team" = "posteam")) %>%
  select(
    game_id, home_team, away_team, game_outcome,
    completion_pct, total_yards, avg_yards, total_epa, interceptions
  )

# View result
print(away_table)