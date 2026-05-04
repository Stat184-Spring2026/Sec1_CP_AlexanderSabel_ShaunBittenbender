library(nflreadr)
library(dplyr)
library(geosphere)

games <- load_schedules(seasons = TRUE)

simple_table <- games |>
  select(game_id, home_team, away_team)

# 3. Create a coordinate mapping
team_coords <- data.frame(
  team = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
           "DET", "GB", "HOU", "IND", "JAX", "KC", "OAK", "SD", "STL", "MIA", 
           "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS"),
  lat = c(33.527, 33.757, 39.285, 42.773, 35.225, 41.862, 39.095, 41.506, 32.747, 39.743, 
          42.340, 44.501, 29.684, 39.760, 30.323, 39.048, 36.090, 33.953, 33.953, 25.957, 
          44.973, 42.090, 29.951, 40.812, 40.812, 39.900, 40.446, 37.403, 47.595, 27.975, 36.166, 38.907),
  long = c(-112.262, -84.400, -76.620, -78.786, -80.852, -87.616, -84.516, -81.699, -97.092, -105.020, 
           -83.045, -88.062, -95.408, -86.163, -81.637, -94.483, -115.183, -118.338, -118.338, -80.238, 
           -93.257, -71.264, -90.081, -74.074, -74.074, -75.167, -80.015, -121.969, -122.331, -82.503, -86.771, -77.036)
)

# 4. Join and Calculate Distance
simple_table <- simple_table |>
  left_join(team_coords, by = c("home_team" = "team")) |>
  rename(home_lat = lat, home_long = long) |>
  left_join(team_coords, by = c("away_team" = "team")) |>
  rename(away_lat = lat, away_long = long) |>
  rowwise() |>
  mutate(
    distance_miles = distHaversine(
      c(home_long, home_lat), 
      c(away_long, away_lat)
    ) * 0.000621371
  ) |>
  ungroup() |>
  select(game_id, home_team, away_team, distance_miles)

View(simple_table)