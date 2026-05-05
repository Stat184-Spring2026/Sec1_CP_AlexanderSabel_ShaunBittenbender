library(nflreadr)
library(dplyr)

games <- load_schedules(2023)
pbp <- load_pbp(2023)

#Step 1: Aggregate stats at team-game level
team_game_stats <- pbp %>%
  filter(!is.na(posteam)) %>%
  group_by(game_id, team = posteam) %>%
  summarise(
    completion_pct = mean(complete_pass, na.rm = TRUE),
    total_yards = sum(yards_gained, na.rm = TRUE),
    avg_yards = mean(yards_gained, na.rm = TRUE),
    total_epa = sum(epa, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    .groups = "drop"
  )

#Step 2: Create home stats
home_stats <- team_game_stats %>%
  rename(
    home_team = team,
    home_completion_pct = completion_pct,
    home_total_yards = total_yards,
    home_avg_yards = avg_yards,
    home_total_epa = total_epa,
    home_interceptions = interceptions
  )

#Step 3: Create away stats
away_stats <- team_game_stats %>%
  rename(
    away_team = team,
    away_completion_pct = completion_pct,
    away_total_yards = total_yards,
    away_avg_yards = avg_yards,
    away_total_epa = total_epa,
    away_interceptions = interceptions
  )

#Step 4: Join everything per game
game_comparison <- games %>%
  select(game_id, home_team, away_team) %>%
  left_join(home_stats, by = c("game_id", "home_team")) %>%
  left_join(away_stats, by = c("game_id", "away_team")) %>%
  mutate(
    epa_diff = home_total_epa - away_total_epa,
    yards_diff = home_total_yards - away_total_yards,
    completion_diff = home_completion_pct - away_completion_pct
  )

View(game_comparison)

game_comparison %>%
  summarise(avg_epa_diff = mean(epa_diff, na.rm = TRUE))


library(ggplot2)

ggplot(game_comparison, aes(x = epa_diff)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of EPA Difference (Home - Away)")