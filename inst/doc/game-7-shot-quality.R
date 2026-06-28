## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>',
  fig.align = 'center',
  out.width = '92%',
  fig.width = 7,
  fig.height = 4.6
)

make_table <- function(x, caption, digits = 3) {
  knitr::kable(x, caption = caption, digits = digits)
}

## ----data---------------------------------------------------------------------
# Pull summary, play-by-play, and roster context.
game_id <- 2023030417
game_summary <- nhlscraper::gc_summary(game_id)
pbp_xg <- nhlscraper::calculate_expected_goals(
  nhlscraper::gc_play_by_play(game_id)
)
xg_model_available <- 'xG' %in% names(pbp_xg) &&
  any(is.finite(pbp_xg[['xG']]) & pbp_xg[['xG']] > 0)
if (!xg_model_available) {
  shot_mask <- pbp_xg[['eventTypeDescKey']] %in% c(
    'goal',
    'shot-on-goal',
    'missed-shot'
  )
  distance <- rep(NA_real_, nrow(pbp_xg))
  if ('distance' %in% names(pbp_xg)) {
    distance <- suppressWarnings(as.numeric(pbp_xg[['distance']]))
  } else if (all(c('xCoordNorm', 'yCoordNorm') %in% names(pbp_xg))) {
    x <- suppressWarnings(as.numeric(pbp_xg[['xCoordNorm']]))
    y <- suppressWarnings(as.numeric(pbp_xg[['yCoordNorm']]))
    distance <- sqrt((89 - x) ^ 2 + y ^ 2)
  }
  distance[!is.finite(distance)] <- stats::median(distance[shot_mask], na.rm = TRUE)
  distance[!is.finite(distance)] <- 35

  fallback_xg <- 0.02 + 0.30 * exp(-distance / 22)
  fallback_xg[pbp_xg[['eventTypeDescKey']] == 'goal'] <- pmax(
    fallback_xg[pbp_xg[['eventTypeDescKey']] == 'goal'],
    0.08
  )
  pbp_xg[['xG']] <- NA_real_
  pbp_xg[['xG']][shot_mask] <- pmin(pmax(fallback_xg[shot_mask], 0.005), 0.65)
}
rosters <- nhlscraper::game_rosters(game_id)

# Build team labels.
home_id <- game_summary[['homeTeam']][['id']]
away_id <- game_summary[['awayTeam']][['id']]
home_abbrev <- game_summary[['homeTeam']][['abbrev']]
away_abbrev <- game_summary[['awayTeam']][['abbrev']]

# Build player lookup.
rosters[['playerFullName']] <- paste(
  rosters[['playerFirstName']],
  rosters[['playerLastName']]
)
rosters[['teamTriCode']] <- ifelse(
  rosters[['teamId']] == home_id,
  home_abbrev,
  away_abbrev
)

# Keep shot attempts with scored xG.
shots <- pbp_xg[
  !is.na(pbp_xg[['xG']]) &
    pbp_xg[['xG']] > 0,
  ,
  drop = FALSE
]
roster_match <- match(shots[['shootingPlayerId']], rosters[['playerId']])
shots[['playerFullName']] <- rosters[['playerFullName']][roster_match]
shots[['teamTriCode']] <- rosters[['teamTriCode']][roster_match]
shots[['timeInPeriod']] <- sprintf(
  '%02d:%02d',
  shots[['secondsElapsedInPeriod']] %/% 60,
  shots[['secondsElapsedInPeriod']] %% 60
)

## ----xg-source-note, echo = FALSE, results = 'asis'---------------------------
if (!xg_model_available) {
  cat(
    paste(
      '> Note: this rendered article uses a deterministic fallback xG estimate',
      'because the external NHLxG booster store was unavailable during vignette',
      'build. In normal package use, `calculate_expected_goals()` supplies the',
      'model-scored values after downloading and caching the needed booster.'
    ),
    '\n\n'
  )
}

## ----team-table---------------------------------------------------------------
# Summarize team-level chance quality.
team_table <- data.frame(
  team = c(home_abbrev, away_abbrev),
  goals = c(
    game_summary[['homeTeam']][['score']],
    game_summary[['awayTeam']][['score']]
  ),
  shotsOnGoal = c(
    game_summary[['homeTeam']][['sog']],
    game_summary[['awayTeam']][['sog']]
  ),
  attempts = c(
    sum(shots[['eventOwnerTeamId']] == home_id),
    sum(shots[['eventOwnerTeamId']] == away_id)
  ),
  xG = c(
    sum(shots[['xG']][shots[['eventOwnerTeamId']] == home_id], na.rm = TRUE),
    sum(shots[['xG']][shots[['eventOwnerTeamId']] == away_id], na.rm = TRUE)
  )
)
team_table[['xGPerAttempt']] <- team_table[['xG']] / team_table[['attempts']]
make_table(
  team_table,
  caption = 'Game 7 scoreboard and shot-quality summary.',
  digits = 3
)

## ----goal-table---------------------------------------------------------------
# Build goal timeline.
goals <- pbp_xg[pbp_xg[['eventTypeDescKey']] == 'goal', , drop = FALSE]
goal_match <- match(goals[['scoringPlayerId']], rosters[['playerId']])
goal_table <- data.frame(
  period = goals[['periodNumber']],
  time = sprintf(
    '%02d:%02d',
    goals[['secondsElapsedInPeriod']] %/% 60,
    goals[['secondsElapsedInPeriod']] %% 60
  ),
  team = ifelse(
    goals[['eventOwnerTeamId']] == home_id,
    home_abbrev,
    away_abbrev
  ),
  scorer = rosters[['playerFullName']][goal_match],
  xG = goals[['xG']],
  stringsAsFactors = FALSE
)
make_table(
  goal_table,
  caption = 'Goal timeline with shot-quality estimate.',
  digits = 3
)

## ----period-table-------------------------------------------------------------
# Summarize xG by period and team.
period_summary <- aggregate(
  xG ~ periodNumber + eventOwnerTeamId,
  data = shots,
  FUN = sum
)
period_ids <- sort(unique(shots[['periodNumber']]))
period_table <- data.frame(period = period_ids)
for (team_id in c(home_id, away_id)) {
  team_label <- ifelse(team_id == home_id, home_abbrev, away_abbrev)
  team_rows <- period_summary[
    period_summary[['eventOwnerTeamId']] == team_id,
    ,
    drop = FALSE
  ]
  period_table[[paste0(team_label, '_xG')]] <- team_rows[['xG']][match(
    period_ids,
    team_rows[['periodNumber']]
  )]
}
period_table[is.na(period_table)] <- 0
make_table(
  period_table,
  caption = 'Expected goals by period.',
  digits = 3
)

## ----period-plot, fig.cap = 'Period-level xG in Game 7.'----------------------
# Plot period xG by team.
period_matrix <- rbind(
  period_table[[paste0(home_abbrev, '_xG')]],
  period_table[[paste0(away_abbrev, '_xG')]]
)
graphics::barplot(
  period_matrix,
  beside = TRUE,
  col = c('#c1121f', '#003049'),
  border = NA,
  ylim = c(0, max(period_matrix, na.rm = TRUE) * 1.28),
  names.arg = paste('P', period_table[['period']]),
  xlab = 'Period',
  ylab = 'Expected Goals'
)
graphics::legend(
  'topright',
  legend = c(home_abbrev, away_abbrev),
  fill = c('#c1121f', '#003049'),
  bty = 'n',
  cex = 0.85
)

## ----chance-table-------------------------------------------------------------
# Show largest individual chances.
chance_idx <- order(-shots[['xG']])
chance_table <- data.frame(
  player = shots[['playerFullName']][chance_idx],
  team = shots[['teamTriCode']][chance_idx],
  period = shots[['periodNumber']][chance_idx],
  time = shots[['timeInPeriod']][chance_idx],
  event = shots[['eventTypeDescKey']][chance_idx],
  xCoordNorm = shots[['xCoordNorm']][chance_idx],
  yCoordNorm = shots[['yCoordNorm']][chance_idx],
  xG = shots[['xG']][chance_idx],
  stringsAsFactors = FALSE
)
chance_table <- utils::head(chance_table, 12)
make_table(
  chance_table,
  caption = 'Highest-xG attempts in Game 7.',
  digits = 3
)

## ----cumulative-data----------------------------------------------------------
# Build cumulative xG paths.
build_cum_path <- function(team_id) {
  team_shots <- shots[
    shots[['eventOwnerTeamId']] == team_id,
    c('eventId', 'secondsElapsedInGame', 'xG')
  ]
  team_shots <- team_shots[order(
    team_shots[['secondsElapsedInGame']],
    team_shots[['eventId']]
  ), ]
  data.frame(
    minutes = c(0, team_shots[['secondsElapsedInGame']] / 60),
    cumXG = c(0, cumsum(team_shots[['xG']]))
  )
}
home_path <- build_cum_path(home_id)
away_path <- build_cum_path(away_id)

## ----cumulative-plot, fig.cap = 'Cumulative expected goals in Game 7.'--------
graphics::plot(
  home_path[['minutes']],
  home_path[['cumXG']],
  type = 's',
  lwd = 2.5,
  col = '#c1121f',
  xlim = c(0, 60),
  ylim = c(0, max(c(home_path[['cumXG']], away_path[['cumXG']])) * 1.08),
  xlab = 'Minutes Elapsed',
  ylab = 'Cumulative Expected Goals'
)
graphics::lines(
  away_path[['minutes']],
  away_path[['cumXG']],
  type = 's',
  lwd = 2.5,
  col = '#003049'
)
graphics::abline(v = c(20, 40), lty = 3, col = '#adb5bd')
graphics::legend(
  'topleft',
  legend = c(home_abbrev, away_abbrev),
  col = c('#c1121f', '#003049'),
  lwd = 2.5,
  bty = 'n'
)

## ----rink-plot, fig.cap = 'Shot-quality map for Game 7. Point size scales with xG.'----
# Plot shot map.
home_shots <- shots[shots[['eventOwnerTeamId']] == home_id, ]
away_shots <- shots[shots[['eventOwnerTeamId']] == away_id, ]
nhlscraper::draw_NHL_rink()
graphics::points(
  home_shots[['xCoordNorm']],
  home_shots[['yCoordNorm']],
  pch = 19,
  col = grDevices::rgb(0.76, 0.07, 0.12, 0.55),
  cex = 0.6 + 7 * sqrt(home_shots[['xG']])
)
graphics::points(
  away_shots[['xCoordNorm']],
  away_shots[['yCoordNorm']],
  pch = 19,
  col = grDevices::rgb(0.00, 0.19, 0.29, 0.55),
  cex = 0.6 + 7 * sqrt(away_shots[['xG']])
)
graphics::legend(
  'topright',
  legend = c(home_abbrev, away_abbrev),
  pch = 19,
  col = c(
    grDevices::rgb(0.76, 0.07, 0.12, 0.75),
    grDevices::rgb(0.00, 0.19, 0.29, 0.75)
  ),
  bty = 'n'
)

