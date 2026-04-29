library(nflreadr)
library(dplyr)
library(tidyr)

# Load data
games <- load_schedules(2023)
pbp <- load_pbp(2023)

# Step 1: Aggregate stats at team-game level
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

# Step 2: Label games as home or away
team_games <- games %>%
  select(game_id, home_team, away_team) %>%
  pivot_longer(cols = c(home_team, away_team),
               names_to = "location",
               values_to = "team") %>%
  mutate(location = ifelse(location == "home_team", "home", "away"))

# Step 3: Join stats with location
team_stats_with_location <- team_games %>%
  left_join(team_game_stats, by = c("game_id", "team"))

# Step 4: Compute averages for each team (home vs away)
team_home_away_split <- team_stats_with_location %>%
  group_by(team, location) %>%
  summarise(
    avg_completion_pct = mean(completion_pct, na.rm = TRUE),
    avg_total_yards = mean(total_yards, na.rm = TRUE),
    avg_epa = mean(total_epa, na.rm = TRUE),
    avg_interceptions = mean(interceptions, na.rm = TRUE),
    games = n(),
    .groups = "drop"
  )

# Step 5: Put home vs away side-by-side
comparison_table <- team_home_away_split %>%
  pivot_wider(
    names_from = location,
    values_from = c(avg_completion_pct, avg_total_yards, avg_epa, avg_interceptions, games)
  )

View(comparison_table)


library(ggplot2)

ggplot(comparison_table, aes(x = avg_epa_home, y = avg_epa_away)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Team EPA: Home vs Away",
    x = "Average EPA (Home)",
    y = "Average EPA (Away)"
  )



comparison_table <- comparison_table %>%
  mutate(epa_diff = avg_epa_home - avg_epa_away)

ggplot(comparison_table, aes(x = reorder(team, epa_diff), y = epa_diff)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "EPA Difference (Home - Away) by Team",
    x = "Team",
    y = "EPA Difference"
  )



comparison_long <- comparison_table %>%
  select(team, avg_epa_home, avg_epa_away) %>%
  pivot_longer(cols = -team,
               names_to = "location",
               values_to = "epa")

ggplot(comparison_long, aes(x = location, y = epa)) +
  geom_boxplot() +
  labs(
    title = "Distribution of EPA: Home vs Away",
    x = "Location",
    y = "EPA"
  )
