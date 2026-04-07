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

## ----data---------------------------------------------------------------------
# Pull summary, play-by-play, and roster context.
game_summary <- nhlscraper::gc_summary(2023030417)
pbp_xg <- nhlscraper::calculate_expected_goals(
  nhlscraper::gc_pbp(2023030417)
)
if (!('xG' %in% names(pbp_xg))) {
  stop(
    paste(
      "calculate_expected_goals() did not return an xG column during article build.",
      "This usually means the loaded nhlscraper namespace is stale or the scorer hit",
      "a runtime error and returned the input unchanged."
    )
  )
}
rosters <- nhlscraper::game_rosters(2023030417)

# Build team and player labels.
home_id <- game_summary[['homeTeam']][['id']]
away_id <- game_summary[['awayTeam']][['id']]
home_abbrev <- game_summary[['homeTeam']][['abbrev']]
away_abbrev <- game_summary[['awayTeam']][['abbrev']]

rosters[['playerFullName']] <- paste(
  rosters[['playerFirstName']],
  rosters[['playerLastName']]
)
rosters[['teamTriCode']] <- ifelse(
  rosters[['teamId']] == home_id,
  home_abbrev,
  away_abbrev
)

# Keep shot events with positive xG.
shots <- pbp_xg[!is.na(pbp_xg[['xG']]) & pbp_xg[['xG']] > 0, , drop = FALSE]
roster_match <- match(shots[['shootingPlayerId']], rosters[['playerId']])
shots[['playerFullName']] <- rosters[['playerFullName']][roster_match]
shots[['teamTriCode']] <- rosters[['teamTriCode']][roster_match]
shots[['timeInPeriod']] <- sprintf(
  '%02d:%02d',
  shots[['secondsElapsedInPeriod']] %/% 60,
  shots[['secondsElapsedInPeriod']] %% 60
)

## ----team-table---------------------------------------------------------------
# Summarize team-level scoreboard and xG results.
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
  xG = c(
    sum(shots[['xG']][shots[['eventOwnerTeamId']] == home_id], na.rm = TRUE),
    sum(shots[['xG']][shots[['eventOwnerTeamId']] == away_id], na.rm = TRUE)
  )
)
team_table[['xGPerShot']] <-
  team_table[['xG']] / team_table[['shotsOnGoal']]
make_table(
  team_table,
  caption = 'Game 7 team context: score, shots on goal, and expected goals.'
)

## ----period-table-------------------------------------------------------------
# Summarize xG by period and team.
period_data <- data.frame(
  periodNumber = shots[['periodNumber']],
  eventOwnerTeamId = shots[['eventOwnerTeamId']],
  xG = shots[['xG']]
)
period_summary <- aggregate(
  xG ~ periodNumber + eventOwnerTeamId,
  data = period_data,
  FUN = sum
)
period_ids <- sort(unique(shots[['periodNumber']]))
period_table <- data.frame(period = period_ids)
home_match <- match(
  period_ids,
  period_summary[['periodNumber']][period_summary[['eventOwnerTeamId']] == home_id]
)
away_match <- match(
  period_ids,
  period_summary[['periodNumber']][period_summary[['eventOwnerTeamId']] == away_id]
)
period_table[[paste0(home_abbrev, '_xG')]] <-
  period_summary[['xG']][period_summary[['eventOwnerTeamId']] == home_id][home_match]
period_table[[paste0(away_abbrev, '_xG')]] <-
  period_summary[['xG']][period_summary[['eventOwnerTeamId']] == away_id][away_match]
period_table[is.na(period_table)] <- 0
make_table(
  period_table,
  caption = 'Expected goals by period in Game 7.'
)

## ----chance-table-------------------------------------------------------------
# Show largest individual shot-quality events.
chance_idx <- order(-shots[['xG']])
chance_table <- data.frame(
  player = shots[['playerFullName']][chance_idx],
  team = shots[['teamTriCode']][chance_idx],
  period = shots[['periodNumber']][chance_idx],
  timeInPeriod = shots[['timeInPeriod']][chance_idx],
  xCoordNorm = shots[['xCoordNorm']][chance_idx],
  yCoordNorm = shots[['yCoordNorm']][chance_idx],
  xG = shots[['xG']][chance_idx]
)
chance_table <- utils::head(chance_table, 10)
make_table(
  chance_table,
  caption = 'Highest-xG individual chances in Game 7.',
  digits = 3
)

## ----cum-data-----------------------------------------------------------------
# Build cumulative xG paths for both teams.
build_cum_path <- function(team_id) {
  team_idx <- shots[['eventOwnerTeamId']] == team_id
  team_shots <- data.frame(
    eventId = shots[['eventId']][team_idx],
    secondsElapsedInGame = shots[['secondsElapsedInGame']][team_idx],
    xG = shots[['xG']][team_idx]
  )
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

## ----cum-plot, fig.cap = 'Cumulative expected goals in Game 7 of the 2024 Stanley Cup Final.'----
graphics::plot(
  home_path[['minutes']],
  home_path[['cumXG']],
  type = 's',
  lwd = 2,
  col = '#c1121f',
  xlim = c(0, 60),
  ylim = c(0, max(c(home_path[['cumXG']], away_path[['cumXG']])) * 1.05),
  xlab = 'Minutes Elapsed',
  ylab = 'Cumulative Expected Goals'
)
graphics::lines(
  away_path[['minutes']],
  away_path[['cumXG']],
  type = 's',
  lwd = 2,
  col = '#003049'
)
graphics::abline(v = c(20, 40), lty = 3, col = '#8d99ae')
graphics::legend(
  'topleft',
  legend = c(home_abbrev, away_abbrev),
  col = c('#c1121f', '#003049'),
  lwd = 2,
  bty = 'n'
)

## ----rink-plot, fig.cap = 'Shot-quality map for Game 7. Point size scales with expected goals.'----
# Split shot map inputs by team.
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

