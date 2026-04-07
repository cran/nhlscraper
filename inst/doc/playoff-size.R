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
# Pull playoff, regular-season, and biometric records.
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

# Join player-level tables.
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

# Keep salary-cap skaters with meaningful samples.
analysis_tbl <- analysis_tbl[
  !is.na(analysis_tbl[['height']]) &
    !is.na(analysis_tbl[['weight']]) &
    analysis_tbl[['firstSeasonForGameType']] >= 20052006 &
    analysis_tbl[['gamesPlayed']] >= 20 &
    analysis_tbl[['rsGamesPlayed']] >= 200,
]

# Fill missing names and compute scoring rates.
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
weight_share <- rank(
  analysis_tbl[['weight']],
  ties.method = 'first'
) / nrow(analysis_tbl)
analysis_tbl[['weightQuartile']] <- cut(
  weight_share,
  breaks = c(0, 0.25, 0.50, 0.75, 1),
  include.lowest = TRUE,
  labels = c('Lightest', 'Second', 'Third', 'Heaviest')
)
nrow(analysis_tbl)

## ----sample-table-------------------------------------------------------------
# Show sample of strongest playoff scorers.
sample_tbl <- analysis_tbl[
  order(-analysis_tbl[['playoffPPG']], -analysis_tbl[['gamesPlayed']]),
  c(
    'playerFullName',
    'positionCode',
    'weight',
    'rsGamesPlayed',
    'gamesPlayed',
    'regularPPG',
    'playoffPPG',
    'playoffLift'
  )
]
sample_tbl <- utils::head(sample_tbl, 8)
make_table(
  sample_tbl,
  caption = 'Top playoff scoring rates among skaters in the working sample.'
)

## ----quartiles----------------------------------------------------------------
# Summarize scoring levels and playoff lift by quartile.
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
  caption = 'Regular-season scoring, playoff scoring, and playoff lift by weight quartile.'
)

## ----quartile-plot, fig.cap = 'Playoff scoring level and playoff lift by weight quartile.'----
# Plot playoff scoring distribution and average lift.
old_par <- graphics::par(no.readonly = TRUE)
graphics::par(mfrow = c(1, 2), mar = c(8, 4, 3, 1))
graphics::boxplot(
  playoffPPG ~ weightQuartile,
  data = analysis_tbl,
  col = c('#d9ed92', '#b5e48c', '#76c893', '#34a0a4'),
  border = '#3a5a40',
  las = 2,
  ylab = 'Playoff Points Per Game',
  xlab = ''
)
graphics::barplot(
  quartile_summary[['playoffLift']],
  names.arg = quartile_summary[['weightQuartile']],
  col = c('#f4d35e', '#ee964b', '#f95738', '#7b2cbf'),
  border = NA,
  las = 2,
  ylab = 'Playoff Lift',
  xlab = ''
)
graphics::abline(h = 0, lty = 2, col = '#4d4d4d')
graphics::par(old_par)

## ----risers-------------------------------------------------------------------
# Show largest positive playoff lifts among larger-sample skaters.
risers_tbl <- analysis_tbl[
  analysis_tbl[['gamesPlayed']] >= 40,
  c(
    'playerFullName',
    'positionBucket',
    'weight',
    'regularPPG',
    'playoffPPG',
    'playoffLift'
  )
]
risers_tbl <- risers_tbl[order(-risers_tbl[['playoffLift']]), ]
risers_tbl <- utils::head(risers_tbl, 10)
make_table(
  risers_tbl,
  caption = 'Largest playoff scoring lifts among skaters with at least 40 playoff games.'
)

## ----model--------------------------------------------------------------------
# Fit simple playoff-lift model.
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
  caption = 'Linear model of playoff scoring lift on height, weight, and position.',
  digits = 4
)

