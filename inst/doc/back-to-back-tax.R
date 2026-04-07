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
# Pull regular-season game and team records.
games_tbl <- nhlscraper::games()
teams_tbl <- nhlscraper::teams()

# Keep salary-cap regular-season games.
games_tbl <- games_tbl[
  games_tbl[['seasonId']] >= 20052006 &
    games_tbl[['gameTypeId']] == 2,
  c(
    'gameId',
    'seasonId',
    'gameDate',
    'homeTeamId',
    'visitingTeamId',
    'homeScore',
    'visitingScore'
  )
]

# Expand games into team-game rows.
home_games <- data.frame(
  gameId = games_tbl[['gameId']],
  seasonId = games_tbl[['seasonId']],
  gameDate = as.Date(games_tbl[['gameDate']]),
  teamId = games_tbl[['homeTeamId']],
  isHome = TRUE,
  goalsFor = games_tbl[['homeScore']],
  goalsAgainst = games_tbl[['visitingScore']]
)
away_games <- data.frame(
  gameId = games_tbl[['gameId']],
  seasonId = games_tbl[['seasonId']],
  gameDate = as.Date(games_tbl[['gameDate']]),
  teamId = games_tbl[['visitingTeamId']],
  isHome = FALSE,
  goalsFor = games_tbl[['visitingScore']],
  goalsAgainst = games_tbl[['homeScore']]
)
team_games <- rbind(home_games, away_games)

# Sort within team and compute off-days since previous game.
team_games <- team_games[order(
  team_games[['teamId']],
  team_games[['gameDate']],
  team_games[['gameId']]
), ]
team_games[['previousGameDate']] <- ave(
  team_games[['gameDate']],
  team_games[['teamId']],
  FUN = function(x) c(as.Date(NA), utils::head(x, -1))
)
team_games[['restDays']] <-
  as.integer(team_games[['gameDate']] - team_games[['previousGameDate']]) - 1L
team_games <- team_games[!is.na(team_games[['restDays']]), ]

# Bucket rest and compute result metrics.
team_games[['restBucket']] <- ifelse(
  team_games[['restDays']] >= 3,
  '3+',
  as.character(team_games[['restDays']])
)
team_games[['restBucket']] <- factor(
  team_games[['restBucket']],
  levels = c('0', '1', '2', '3+')
)
team_games[['win']] <- team_games[['goalsFor']] > team_games[['goalsAgainst']]
team_games[['goalDiff']] <-
  team_games[['goalsFor']] - team_games[['goalsAgainst']]

## ----rest-table---------------------------------------------------------------
# Summarize results by rest bucket.
rest_summary <- aggregate(
  cbind(win, goalDiff) ~ restBucket,
  data = team_games,
  FUN = mean
)
rest_counts <- as.data.frame(table(team_games[['restBucket']]))
names(rest_counts) <- c('restBucket', 'games')
rest_summary <- merge(rest_summary, rest_counts, by = 'restBucket')
rest_summary <- rest_summary[
  match(levels(team_games[['restBucket']]), rest_summary[['restBucket']]),
  c('restBucket', 'games', 'win', 'goalDiff')
]

make_table(
  rest_summary,
  caption = 'Win rate and average goal differential by rest bucket.'
)

## ----rest-plot, fig.cap = 'Win rate across rest buckets in the salary-cap era.'----
graphics::barplot(
  rest_summary[['win']],
  names.arg = rest_summary[['restBucket']],
  col = c('#d62828', '#f77f00', '#fcbf49', '#90be6d'),
  border = NA,
  ylim = c(0, 0.6),
  xlab = 'Days of Rest',
  ylab = 'Win Rate'
)
graphics::abline(
  h = mean(team_games[['win']]),
  lty = 2,
  col = '#4d4d4d'
)

## ----home-road-table----------------------------------------------------------
# Summarize rest effect by venue.
home_road_summary <- aggregate(
  win ~ restBucket + isHome,
  data = team_games,
  FUN = mean
)
home_wins <- home_road_summary[
  home_road_summary[['isHome']],
  c('restBucket', 'win')
]
away_wins <- home_road_summary[
  !home_road_summary[['isHome']],
  c('restBucket', 'win')
]
names(home_wins)[names(home_wins) == 'win'] <- 'homeWinRate'
names(away_wins)[names(away_wins) == 'win'] <- 'awayWinRate'
home_road_table <- merge(home_wins, away_wins, by = 'restBucket')
home_road_table <- home_road_table[
  match(levels(team_games[['restBucket']]), home_road_table[['restBucket']]),
]
make_table(
  home_road_table,
  caption = 'Home and road win rate by rest bucket.'
)

## ----home-road-plot, fig.cap = 'Home and road win rate across rest buckets.'----
graphics::plot(
  seq_len(nrow(home_road_table)),
  home_road_table[['homeWinRate']],
  type = 'b',
  pch = 19,
  lwd = 2,
  col = '#1d3557',
  xaxt = 'n',
  ylim = c(0.35, 0.60),
  xlab = 'Days of Rest',
  ylab = 'Win Rate'
)
graphics::lines(
  seq_len(nrow(home_road_table)),
  home_road_table[['awayWinRate']],
  type = 'b',
  pch = 19,
  lwd = 2,
  col = '#e63946'
)
graphics::axis(
  side = 1,
  at = seq_len(nrow(home_road_table)),
  labels = home_road_table[['restBucket']]
)
graphics::legend(
  'bottomright',
  legend = c('Home', 'Away'),
  col = c('#1d3557', '#e63946'),
  pch = 19,
  lwd = 2,
  bty = 'n'
)

## ----model--------------------------------------------------------------------
# Fit simple win model with rest and venue.
rest_fit <- stats::glm(
  as.integer(win) ~ restBucket + isHome,
  data = team_games,
  family = stats::binomial()
)
rest_fit_tbl <- as.data.frame(summary(rest_fit)$coefficients)
rest_fit_tbl[['term']] <- rownames(rest_fit_tbl)
rownames(rest_fit_tbl) <- NULL
rest_fit_tbl[['term']] <- c(
  'Intercept',
  'One day versus zero',
  'Two days versus zero',
  'Three-plus days versus zero',
  'Home indicator'
)
rest_fit_tbl <- rest_fit_tbl[, c(
  'term',
  'Estimate',
  'Std. Error',
  'z value',
  'Pr(>|z|)'
)]
make_table(
  rest_fit_tbl,
  caption = 'Logistic model of wins on rest and venue.',
  digits = 4
)

## ----team-table---------------------------------------------------------------
# Rank teams by back-to-back win rate.
zero_rest_tbl <- team_games[team_games[['restDays']] == 0, c('teamId', 'win')]
zero_rest_tbl <- aggregate(
  win ~ teamId,
  data = zero_rest_tbl,
  FUN = function(x) c(winRate = mean(x), games = length(x))
)
zero_rest_tbl <- data.frame(
  teamId = zero_rest_tbl[['teamId']],
  winRate = zero_rest_tbl[['win']][, 'winRate'],
  games = zero_rest_tbl[['win']][, 'games']
)
zero_rest_tbl <- zero_rest_tbl[zero_rest_tbl[['games']] >= 50, ]
zero_rest_tbl <- merge(
  zero_rest_tbl,
  teams_tbl[, c('teamId', 'teamTriCode')],
  by = 'teamId',
  all.x = TRUE
)
zero_rest_tbl <- zero_rest_tbl[
  order(-zero_rest_tbl[['winRate']]),
  c('teamTriCode', 'games', 'winRate')
]
zero_rest_tbl <- utils::head(zero_rest_tbl, 10)
make_table(
  zero_rest_tbl,
  caption = 'Best back-to-back win rates among teams with at least 50 zero-rest games.'
)

