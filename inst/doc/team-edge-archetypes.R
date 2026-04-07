## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>',
  fig.align = 'center',
  out.width = '90%',
  fig.width = 7,
  fig.height = 4.5
)

make_table <- function(x, caption, digits = 3) {
  knitr::kable(x, caption = caption, digits = digits)
}

## ----leaders------------------------------------------------------------------
# Pull 2024-25 team EDGE leaders.
edge_leaders <- nhlscraper::team_edge_leaders(
  season = 20242025,
  game_type = 2
)

# Build compact leader table.
leader_table <- data.frame(
  metric = c(
    'Shots over 90 mph',
    'Bursts over 22 mph',
    'Distance per 60',
    'High-danger shots on goal',
    'Offensive-zone time',
    'Neutral-zone time',
    'Defensive-zone time'
  ),
  team = c(
    edge_leaders[['shotAttemptsOver90']][['team']][['abbrev']],
    edge_leaders[['burstsOver22']][['team']][['abbrev']],
    edge_leaders[['distancePer60']][['team']][['abbrev']],
    edge_leaders[['highDangerSOG']][['team']][['abbrev']],
    edge_leaders[['offensiveZoneTime']][['team']][['abbrev']],
    edge_leaders[['neutralZoneTime']][['team']][['abbrev']],
    edge_leaders[['defensiveZoneTime']][['team']][['abbrev']]
  ),
  value = c(
    as.character(edge_leaders[['shotAttemptsOver90']][['attempts']]),
    as.character(edge_leaders[['burstsOver22']][['bursts']]),
    sprintf('%.2f miles', edge_leaders[['distancePer60']][['distanceSkated']][['imperial']]),
    as.character(edge_leaders[['highDangerSOG']][['sog']]),
    sprintf('%.3f', edge_leaders[['offensiveZoneTime']][['zoneTime']]),
    sprintf('%.3f', edge_leaders[['neutralZoneTime']][['zoneTime']]),
    sprintf('%.3f', edge_leaders[['defensiveZoneTime']][['zoneTime']])
  ),
  stringsAsFactors = FALSE
)
make_table(
  leader_table,
  caption = 'League-leading 2024-25 team EDGE categories.'
)

## ----profiles-----------------------------------------------------------------
# Define selected teams and robust fetch helpers.
team_ids <- c(CAR = 12, COL = 21, EDM = 22, FLA = 13, WSH = 15)
fetch_with_retry <- function(fetch_fun, validator, tries = 3) {
  for (i in seq_len(tries)) {
    value <- try(fetch_fun(), silent = TRUE)
    if (!inherits(value, 'try-error') && validator(value)) {
      return(value)
    }
    Sys.sleep(i / 4)
  }
  NULL
}
valid_df <- function(x, required_cols) {
  is.data.frame(x) && nrow(x) > 0 && all(required_cols %in% names(x))
}
extract_name <- function(first_name, last_name) {
  if (is.na(first_name) || is.na(last_name) || first_name == '' || last_name == '') {
    return(NA_character_)
  }
  paste(first_name, last_name)
}
build_team_profile <- function(team_code, team_id) {
  team_summary <- fetch_with_retry(
    function() nhlscraper::team_edge_summary(
      team = team_id,
      season = 20242025,
      game_type = 2
    ),
    function(x) is.list(x) && 'team' %in% names(x)
  )
  zone_rows <- fetch_with_retry(
    function() nhlscraper::team_edge_zone_time(
      team = team_id,
      season = 20242025,
      game_type = 2,
      category = 'details'
    ),
    function(x) valid_df(x, c('strengthCode', 'offensiveZonePctg'))
  )
  skating_rows <- fetch_with_retry(
    function() nhlscraper::team_edge_skating_speed(
      team = team_id,
      season = 20242025,
      game_type = 2,
      category = 'details'
    ),
    function(x) {
      valid_df(x, c(
        'positionCode',
        'maxSkatingSpeed.imperial',
        'burstsOver22.value'
      ))
    }
  )
  shot_speed_rows <- fetch_with_retry(
    function() nhlscraper::team_edge_shot_speed(
      team = team_id,
      season = 20242025,
      game_type = 2,
      category = 'details'
    ),
    function(x) {
      valid_df(x, c(
        'position',
        'topShotSpeed.imperial',
        'shotAttempts90To100.value'
      ))
    }
  )
  shot_location_rows <- fetch_with_retry(
    function() nhlscraper::team_edge_shot_location(
      team = team_id,
      season = 20242025,
      game_type = 2,
      category = 'details'
    ),
    function(x) valid_df(x, c('area', 'sog'))
  )
  if (
    is.null(zone_rows) ||
      is.null(skating_rows) ||
      is.null(shot_speed_rows) ||
      is.null(shot_location_rows)
  ) {
    return(data.frame(
      team = team_code,
      points = if (is.null(team_summary)) NA_real_ else as.numeric(team_summary[['team']][['points']]),
      wins = if (is.null(team_summary)) NA_real_ else as.numeric(team_summary[['team']][['wins']]),
      offensiveZonePctg = NA_real_,
      maxSkatingSpeed = NA_real_,
      burstsOver22 = NA_real_,
      shotAttemptsOver90 = NA_real_,
      hardestShot = NA_real_,
      interiorShare = NA_real_,
      circleShare = NA_real_,
      pointShare = NA_real_,
      otherShare = NA_real_,
      fastestSkater = NA_character_,
      hardestShooter = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  zone_row <- zone_rows[zone_rows[['strengthCode']] == 'all', , drop = FALSE]
  skating_row <- skating_rows[skating_rows[['positionCode']] == 'all', , drop = FALSE]
  shot_speed_row <- shot_speed_rows[shot_speed_rows[['position']] == 'all', , drop = FALSE]
  if (!nrow(zone_row)) zone_row <- zone_rows[1, , drop = FALSE]
  if (!nrow(skating_row)) skating_row <- skating_rows[1, , drop = FALSE]
  if (!nrow(shot_speed_row)) shot_speed_row <- shot_speed_rows[1, , drop = FALSE]
  interior_mask <- shot_location_rows[['area']] %in% c(
    'Crease',
    'Low Slot',
    'L Net Side',
    'R Net Side'
  )
  circle_mask <- shot_location_rows[['area']] %in% c(
    'High Slot',
    'L Circle',
    'R Circle'
  )
  point_mask <- shot_location_rows[['area']] %in% c(
    'Center Point',
    'L Point',
    'R Point',
    'Outside L',
    'Outside R',
    'Beyond Red Line'
  )
  total_shots <- sum(shot_location_rows[['sog']])
  data.frame(
    team = team_code,
    points = if (is.null(team_summary)) NA_real_ else as.numeric(team_summary[['team']][['points']]),
    wins = if (is.null(team_summary)) NA_real_ else as.numeric(team_summary[['team']][['wins']]),
    offensiveZonePctg = as.numeric(zone_row[['offensiveZonePctg']][1]),
    maxSkatingSpeed = as.numeric(skating_row[['maxSkatingSpeed.imperial']][1]),
    burstsOver22 = as.numeric(skating_row[['burstsOver22.value']][1]),
    shotAttemptsOver90 = as.numeric(
      shot_speed_row[['shotAttemptsOver100.value']][1] +
        shot_speed_row[['shotAttempts90To100.value']][1]
    ),
    hardestShot = as.numeric(shot_speed_row[['topShotSpeed.imperial']][1]),
    interiorShare = sum(shot_location_rows[['sog']][interior_mask]) / total_shots,
    circleShare = sum(shot_location_rows[['sog']][circle_mask]) / total_shots,
    pointShare = sum(shot_location_rows[['sog']][point_mask]) / total_shots,
    otherShare = sum(shot_location_rows[['sog']][!(interior_mask | circle_mask | point_mask)]) / total_shots,
    fastestSkater = extract_name(
      skating_row[['maxSkatingSpeed.overlay.player.firstName.default']][1],
      skating_row[['maxSkatingSpeed.overlay.player.lastName.default']][1]
    ),
    hardestShooter = extract_name(
      shot_speed_row[['topShotSpeed.overlay.player.firstName.default']][1],
      shot_speed_row[['topShotSpeed.overlay.player.lastName.default']][1]
    ),
    stringsAsFactors = FALSE
  )
}
team_profiles <- Map(
  build_team_profile,
  team_code = names(team_ids),
  team_id = unname(team_ids)
)
team_profiles <- do.call(rbind, team_profiles)
rownames(team_profiles) <- NULL
profile_table <- team_profiles[, c(
  'team',
  'points',
  'wins',
  'offensiveZonePctg',
  'maxSkatingSpeed',
  'burstsOver22',
  'shotAttemptsOver90',
  'hardestShot',
  'interiorShare'
)]
make_table(
  profile_table,
  caption = 'Five-team 2024-25 EDGE profile comparison.'
)

## ----pace-plot, fig.cap = 'Territorial control and pace look different across elite 2024-25 teams.'----
# Plot territorial control and burst volume.
old_par <- graphics::par(no.readonly = TRUE)
graphics::par(mfrow = c(1, 2), mar = c(5, 7, 3, 1))
ordered_zone <- team_profiles[order(team_profiles[['offensiveZonePctg']]), ]
graphics::barplot(
  ordered_zone[['offensiveZonePctg']],
  names.arg = ordered_zone[['team']],
  horiz = TRUE,
  las = 1,
  col = '#2a9d8f',
  border = NA,
  xlab = 'Offensive-Zone Share'
)
ordered_bursts <- team_profiles[order(team_profiles[['burstsOver22']]), ]
graphics::barplot(
  ordered_bursts[['burstsOver22']],
  names.arg = ordered_bursts[['team']],
  horiz = TRUE,
  las = 1,
  col = '#e76f51',
  border = NA,
  xlab = 'Bursts Over 22 MPH'
)
graphics::par(old_par)

## ----shot-mix-plot, fig.cap = 'Shot-geography mix for five elite 2024-25 teams.'----
# Plot shot mix shares.
shot_mix <- t(as.matrix(team_profiles[, c(
  'interiorShare',
  'circleShare',
  'pointShare',
  'otherShare'
)]))
colnames(shot_mix) <- team_profiles[['team']]
rownames(shot_mix) <- c(
  'Interior',
  'Circles and slot',
  'Points and perimeter',
  'Other'
)
graphics::barplot(
  shot_mix,
  beside = FALSE,
  col = c('#1b4332', '#40916c', '#74c69d', '#d8f3dc'),
  ylim = c(0, 1),
  ylab = 'Share of Tracked Shots',
  xlab = 'Team'
)
graphics::legend(
  'topright',
  legend = rownames(shot_mix),
  fill = c('#1b4332', '#40916c', '#74c69d', '#d8f3dc'),
  bty = 'n'
)

## ----player-table-------------------------------------------------------------
# Show players behind each team's most extreme speed and shot events.
player_table <- team_profiles[, c(
  'team',
  'fastestSkater',
  'maxSkatingSpeed',
  'hardestShooter',
  'hardestShot'
)]
make_table(
  player_table,
  caption = "Players behind each team's fastest burst and hardest shot."
)

