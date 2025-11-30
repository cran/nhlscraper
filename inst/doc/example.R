## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = '#>',
  fig.align = 'center',
  out.width = '90%'
)

## ----library, eval = FALSE----------------------------------------------------
# # Load library.
# library(nhlscraper)
# 
# # Access glossary.
# glossary <- nhlscraper::glossary()

## ----load, eval = FALSE-------------------------------------------------------
# # Load data.
# gc_pbps_20222023 <- nhlscraper::gc_pbps(20222023)
# gc_pbps_20232024 <- nhlscraper::gc_pbps(20232024)
# gc_pbps_20242025 <- nhlscraper::gc_pbps(20242025)
# 
# # Aggregate data.
# common_cols <- Reduce(
#   intersect,
#   list(
#     names(gc_pbps_20222023),
#     names(gc_pbps_20232024),
#     names(gc_pbps_20242025)
#   )
# )
# gc_pbps_20222025 <- rbind(
#   gc_pbps_20222023[common_cols],
#   gc_pbps_20232024[common_cols],
#   gc_pbps_20242025[common_cols]
# )

## ----clean1, eval = FALSE-----------------------------------------------------
# # Flag home/away.
# gc_pbps_20222025_is_home_flagged         <-
#   nhlscraper::flag_is_home(gc_pbps_20222025)
# 
# # Strip game ID.
# gc_pbps_20222025_game_id_stripped        <-
#   nhlscraper::strip_game_id(gc_pbps_20222025_is_home_flagged)
# 
# # Strip time and period.
# gc_pbps_20222025_time_period_stripped    <-
#   nhlscraper::strip_time_period(gc_pbps_20222025_game_id_stripped)

## ----clean2, eval = FALSE-----------------------------------------------------
# # Strip situation code.
# gc_pbps_20222025_situation_code_stripped <-
#   nhlscraper::strip_situation_code(gc_pbps_20222025_time_period_stripped)
# 
# # Flag rebound shot attempts.
# gc_pbps_20222025_is_rebound_flagged      <-
#   nhlscraper::flag_is_rebound(gc_pbps_20222025_situation_code_stripped)
# 
# # Flag rush shot attempts.
# gc_pbps_20222025_is_rush_flagged         <-
#   nhlscraper::flag_is_rush(gc_pbps_20222025_is_rebound_flagged)
# 
# # Count goals, SOG, Fenwick, and Corsi.
# gc_pbps_20222025_goals_shots_counted     <-
#   nhlscraper::count_goals_shots(gc_pbps_20222025_is_rush_flagged)

## ----clean3, eval = FALSE-----------------------------------------------------
# # Normalize coordinates to +x.
# gc_pbps_20222025_coordinates_normalized  <-
#   nhlscraper::normalize_coordinates(gc_pbps_20222025_goals_shots_counted)
# 
# # Calculate distance.
# gc_pbps_20222025_distance_calculated     <-
#   nhlscraper::calculate_distance(gc_pbps_20222025_coordinates_normalized)
# 
# # Calculate angle.
# gc_pbps_20222025_angle_calculated        <-
#   nhlscraper::calculate_angle(gc_pbps_20222025_distance_calculated)
# 
# # Keep only shots.
# gc_shots_20222025 <- gc_pbps_20222025_angle_calculated[
#   gc_pbps_20222025_angle_calculated$typeDescKey %in%
#     c('goal', 'shot-on-goal', 'missed-shot', 'blocked-shot'),
# ]
# 
# # Remove shootouts and penalty shots.
# gc_shots_20222025_final <- gc_shots_20222025[
#   !(gc_shots_20222025$situationCode %in% c('0101', '1010')),
# ]
# 
# # Indicate goal or not.
# gc_shots_20222025_final$isGoal <- as.integer(
#   gc_shots_20222025_final$typeDescKey == 'goal'
# )

## ----model1, eval = FALSE-----------------------------------------------------
# # Build xG model version 1.
# xG_v1 <- glm(
#   isGoal ~
#     distance +
#     angle +
#     isEmptyNetAgainst +
#     strengthState,
#   family = binomial,
#   data   = gc_shots_20222025_final
# )
# 
# # Summarize model 1.
# summary(xG_v1)

## ----model2, eval = FALSE-----------------------------------------------------
# # Build xG model version 2.
# xG_v2 <- glm(
#   isGoal ~
#     distance +
#     angle +
#     isEmptyNetAgainst +
#     strengthState +
#     isRebound +
#     isRush,
#   family = binomial,
#   data   = gc_shots_20222025_final
# )
# 
# # Summarize model 2.
# summary(xG_v2)

## ----model3, eval = FALSE-----------------------------------------------------
# # Build xG model version 3.
# xG_v3 <- glm(
#   isGoal ~
#     distance +
#     angle +
#     isEmptyNetAgainst +
#     strengthState +
#     isRebound +
#     isRush +
#     goalDifferential,
#   family = binomial,
#   data   = gc_shots_20222025_final
# )
# 
# # Summarize model 3.
# summary(xG_v3)

## ----visual1, eval = FALSE----------------------------------------------------
# # Plot shot locations for Game 7 Stanley Cup Finals 2025.
# ig_game_shot_locations(
#   game  = 2023030417,
#   model = 1,
#   team  = 'H'
# )
# ig_game_shot_locations(
#   game  = 2023030417,
#   model = 1,
#   team  = 'A'
# )

## ----pic1, fig.alt = "Shot Locations for Game 7 SCF 2025", echo = FALSE-------
knitr::include_graphics(c(
  'ig_shot_locs_2023030417_home_xG_v1.png',
  'ig_shot_locs_2023030417_away_xG_v1.png'
))

## ----visual2, eval = FALSE----------------------------------------------------
# # Plot cumulative xG for Game 7 Stanley Cup Finals 2025.
# ig_game_cumulative_expected_goals(
#   game  = 2023030417,
#   model = 1
# )

## ----pic2, fig.alt = "Cumulative xG over Time for Game 7 SCF 2025", echo = FALSE----
knitr::include_graphics('ig_cum_xG_2023030417_xG_v1.png')

