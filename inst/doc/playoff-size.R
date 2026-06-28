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
# Pull scoring and bio tables.
playoff_stats <- nhlscraper::skater_playoff_statistics()
career_stats <- nhlscraper::skater_statistics()[, c(
  'playerId',
  'rsGamesPlayed',
  'rsPoints',
  'positionCode'
)]
player_bios <- nhlscraper::players()[, c(
  'playerId',
  'playerFullName',
  'height',
  'weight'
)]

# Join player-level sources.
analysis_tbl <- merge(
  playoff_stats,
  career_stats,
  by = c('playerId', 'positionCode'),
  all.x = TRUE
)
analysis_tbl <- merge(
  analysis_tbl,
  player_bios,
  by = 'playerId',
  all.x = TRUE
)

# Keep modern skaters with stable samples.
analysis_tbl <- analysis_tbl[
  !is.na(analysis_tbl[['height']]) &
    !is.na(analysis_tbl[['weight']]) &
    analysis_tbl[['firstSeasonForGameType']] >= 20052006 &
    analysis_tbl[['gamesPlayed']] >= 20 &
    analysis_tbl[['rsGamesPlayed']] >= 200,
  ,
  drop = FALSE
]

# Fill names and compute rates.
analysis_tbl[['playerFullName']] <- ifelse(
  is.na(analysis_tbl[['playerFullName']]) |
    analysis_tbl[['playerFullName']] == '',
  paste(
    analysis_tbl[['skaterFirstName']],
    analysis_tbl[['skaterLastName']]
  ),
  analysis_tbl[['playerFullName']]
)
analysis_tbl[['regularPPG']] <-
  analysis_tbl[['rsPoints']] / analysis_tbl[['rsGamesPlayed']]
analysis_tbl[['playoffPPG']] <-
  analysis_tbl[['points']] / analysis_tbl[['gamesPlayed']]
analysis_tbl[['playoffLift']] <-
  analysis_tbl[['playoffPPG']] - analysis_tbl[['regularPPG']]
analysis_tbl[['positionBucket']] <- ifelse(
  analysis_tbl[['positionCode']] == 'D',
  'Defense',
  'Forward'
)

# Assign equal-count weight quartiles.
weight_rank <- rank(
  analysis_tbl[['weight']],
  ties.method = 'first'
) / nrow(analysis_tbl)
analysis_tbl[['weightQuartile']] <- cut(
  weight_rank,
  breaks = c(0, 0.25, 0.50, 0.75, 1),
  include.lowest = TRUE,
  labels = c('Lightest', 'Second', 'Third', 'Heaviest')
)
nrow(analysis_tbl)

## ----quartile-table-----------------------------------------------------------
# Summarize scoring by weight quartile.
quartile_summary <- aggregate(
  cbind(regularPPG, playoffPPG, playoffLift) ~ weightQuartile,
  data = analysis_tbl,
  FUN = mean
)
quartile_counts <- as.data.frame(table(analysis_tbl[['weightQuartile']]))
names(quartile_counts) <- c('weightQuartile', 'n')
quartile_summary <- merge(
  quartile_summary,
  quartile_counts,
  by = 'weightQuartile'
)
quartile_summary <- quartile_summary[
  match(levels(analysis_tbl[['weightQuartile']]), quartile_summary[['weightQuartile']]),
  c('weightQuartile', 'n', 'regularPPG', 'playoffPPG', 'playoffLift')
]
make_table(
  quartile_summary,
  caption = 'Regular-season scoring, playoff scoring, and playoff lift by weight quartile.',
  digits = 3
)

## ----quartile-plot, fig.cap = 'Playoff scoring level and playoff lift by weight quartile.'----
# Plot playoff scoring and playoff lift.
old_par <- graphics::par(no.readonly = TRUE)
graphics::par(mfrow = c(1, 2), mar = c(8, 4, 3, 1))
graphics::boxplot(
  playoffPPG ~ weightQuartile,
  data = analysis_tbl,
  col = c('#d8f3dc', '#b7e4c7', '#74c69d', '#2d6a4f'),
  border = '#1b4332',
  las = 2,
  xlab = '',
  ylab = 'Playoff Points Per Game'
)
graphics::barplot(
  quartile_summary[['playoffLift']],
  names.arg = quartile_summary[['weightQuartile']],
  col = c('#fcbf49', '#f77f00', '#d62828', '#6a4c93'),
  border = NA,
  las = 2,
  xlab = '',
  ylab = 'Playoff Lift'
)
graphics::abline(h = 0, lty = 2, col = '#495057')
graphics::par(old_par)

## ----position-summary---------------------------------------------------------
# Summarize rates by position and quartile.
position_summary <- aggregate(
  cbind(regularPPG, playoffPPG, playoffLift) ~ positionBucket + weightQuartile,
  data = analysis_tbl,
  FUN = mean
)
position_counts <- aggregate(
  playerId ~ positionBucket + weightQuartile,
  data = analysis_tbl,
  FUN = length
)
names(position_counts)[names(position_counts) == 'playerId'] <- 'n'
position_summary <- merge(
  position_summary,
  position_counts,
  by = c('positionBucket', 'weightQuartile')
)
make_table(
  position_summary,
  caption = 'Scoring translation by position family and weight quartile.',
  digits = 3
)

## ----risers-------------------------------------------------------------------
# Show largest positive playoff lifts.
risers_tbl <- analysis_tbl[
  analysis_tbl[['gamesPlayed']] >= 40,
  c(
    'playerFullName',
    'positionBucket',
    'weight',
    'regularPPG',
    'playoffPPG',
    'playoffLift',
    'gamesPlayed'
  )
]
risers_tbl <- risers_tbl[order(-risers_tbl[['playoffLift']]), ]
risers_tbl <- utils::head(risers_tbl, 10)
make_table(
  risers_tbl,
  caption = 'Largest playoff scoring lifts among skaters with at least 40 playoff games.',
  digits = 3
)

## ----fallers------------------------------------------------------------------
# Show largest negative playoff lifts.
fallers_tbl <- analysis_tbl[
  analysis_tbl[['gamesPlayed']] >= 40,
  c(
    'playerFullName',
    'positionBucket',
    'weight',
    'regularPPG',
    'playoffPPG',
    'playoffLift',
    'gamesPlayed'
  )
]
fallers_tbl <- fallers_tbl[order(fallers_tbl[['playoffLift']]), ]
fallers_tbl <- utils::head(fallers_tbl, 10)
make_table(
  fallers_tbl,
  caption = 'Largest playoff scoring drops among skaters with at least 40 playoff games.',
  digits = 3
)

## ----model--------------------------------------------------------------------
# Fit playoff-lift model.
lift_fit <- stats::lm(
  playoffLift ~ height + weight + I(positionCode == 'D'),
  data = analysis_tbl
)
lift_fit_tbl <- as.data.frame(summary(lift_fit)$coefficients)
lift_fit_tbl[['term']] <- rownames(lift_fit_tbl)
rownames(lift_fit_tbl) <- NULL
lift_fit_tbl[['term']] <- c(
  'Intercept',
  'Height',
  'Weight',
  'Defense indicator'
)
lift_fit_tbl <- lift_fit_tbl[, c(
  'term',
  'Estimate',
  'Std. Error',
  't value',
  'Pr(>|t|)'
)]
make_table(
  lift_fit_tbl,
  caption = 'Linear model of playoff scoring lift.',
  digits = 4
)

