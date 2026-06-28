## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>',
  fig.align = 'center',
  out.width = '92%',
  fig.width = 7,
  fig.height = 4.5
)

make_table <- function(x, caption, digits = 3) {
  knitr::kable(x, caption = caption, digits = digits)
}

## ----basic-use, eval = FALSE--------------------------------------------------
# pbp <- nhlscraper::gc_play_by_play(2023030417)
# pbp <- nhlscraper::add_shift_times(
#   play_by_play = pbp,
#   shift_chart  = nhlscraper::shift_chart(2023030417)
# )
# pbp <- nhlscraper::add_deltas(pbp)
# pbp <- nhlscraper::calculate_expected_goals(pbp)

## ----partition-table, echo = FALSE--------------------------------------------
partition_table <- data.frame(
  partition = c('sd', 'ev', 'pp', 'sh', 'en', 'ps'),
  name = c(
    'Standard 5v5',
    'Other even strength',
    'Power play',
    'Short-handed',
    'Empty net',
    'Penalty shot / shootout'
  ),
  rows_sent_there = c(
    'Regulation 5v5 shots with both goalies in net, plus safe fallbacks.',
    'Remaining even-strength shots such as 4v4 and 3v3.',
    'Shots where the shooting team has a skater advantage.',
    'Shots where the shooting team has fewer skaters.',
    'Shots at an empty opposing net.',
    'Penalty-shot and shootout-style one-on-one attempts.'
  ),
  stringsAsFactors = FALSE
)
make_table(
  partition_table,
  caption = 'Shot partitions used by calculate_expected_goals().'
)

## ----routing-plot, echo = FALSE, fig.width = 7.5, fig.height = 4.8, fig.cap = 'Runtime routing from play-by-play row to xG value.'----
steps <- c(
  'Shot row',
  'Target season',
  'Partition',
  'Frozen recipe',
  'XGBoost booster',
  'xG'
)
box_x <- c(1, 2, 3, 3, 2, 1)
box_y <- c(2, 2, 2, 1, 1, 1)
box_w <- 0.78
box_h <- 0.36
draw_box <- function(x, y, label, number) {
  wrapped_label <- paste(strwrap(paste0(number, '. ', label), width = 16), collapse = '\n')
  graphics::rect(
    xleft   = x - box_w / 2,
    ybottom = y - box_h / 2,
    xright  = x + box_w / 2,
    ytop    = y + box_h / 2,
    col     = '#fefae0',
    border  = '#bc6c25',
    lwd     = 1.5
  )
  graphics::text(
    x      = x,
    y      = y,
    labels = wrapped_label,
    cex    = 0.84,
    col    = '#1f2933'
  )
}
draw_arrow <- function(i, j) {
  x0 <- box_x[i]
  y0 <- box_y[i]
  x1 <- box_x[j]
  y1 <- box_y[j]
  graphics::arrows(
    x0     = x0 + sign(x1 - x0) * box_w / 2,
    y0     = y0 + sign(y1 - y0) * box_h / 2,
    x1     = x1 - sign(x1 - x0) * box_w / 2,
    y1     = y1 - sign(y1 - y0) * box_h / 2,
    length = 0.08,
    lwd    = 1.3,
    col    = '#495057'
  )
}
graphics::plot(
  NA_real_,
  NA_real_,
  type = 'n',
  axes = FALSE,
  xlab = '',
  ylab = '',
  xlim = c(0.45, 3.55),
  ylim = c(0.55, 2.45)
)
for (i in seq_len(length(steps) - 1L)) {
  draw_arrow(i, i + 1L)
}
for (i in seq_along(steps)) {
  draw_box(box_x[i], box_y[i], steps[i], i)
}

## ----feature-table, echo = FALSE----------------------------------------------
feature_table <- data.frame(
  family = c(
    'Shot geometry',
    'Shot location bins',
    'Previous-event movement',
    'Rush and rebound context',
    'Game state',
    'Strength state',
    'Shooter and goalie biometrics',
    'Shift timing',
    'Shootout counters'
  ),
  examples = c(
    'x/y, normalized x/y, distance, angle',
    'slot, net-front, point, flank, perimeter indicators',
    'delta seconds, delta x/y, delta distance, delta angle',
    'isRush, isRebound, createdRebound, previous event type',
    'score differential, cumulative shots/Fenwick/Corsi',
    'skater counts, manpower differential, empty-net flags',
    'height, weight, handedness where available',
    'seconds elapsed/remaining in shift for on-ice players',
    'attempt order for one-on-one partitions'
  ),
  stringsAsFactors = FALSE
)
make_table(
  feature_table,
  caption = 'Feature families used by the xG models.'
)

## ----training-table, echo = FALSE---------------------------------------------
training_table <- data.frame(
  target_vintage = c('2013-14', '2018-19', '2023-24', '2026-27 deployment'),
  training_window = c(
    'Earliest supported historical window',
    '2015-16, 2016-17, 2017-18',
    '2020-21, 2021-22, 2022-23',
    '2023-24, 2024-25, 2025-26'
  ),
  note = c(
    'Uses the earliest supported vintage behavior.',
    'Example completed rolling vintage.',
    'Example modern completed rolling vintage.',
    'Latest deployment model used for future/default scoring.'
  ),
  stringsAsFactors = FALSE
)
make_table(
  training_table,
  caption = 'Examples of rolling training windows.'
)

## ----deployment-training-table, echo = FALSE----------------------------------
deployment_training <- data.frame(
  partition = c('sd', 'ev', 'pp', 'sh', 'en', 'ps'),
  train_seasons = rep('2023-24, 2024-25, 2025-26', 6),
  rows = c(283688, 7654, 59254, 8186, 2891, 2027),
  goals = c(16881, 813, 5678, 595, 1596, 645),
  goal_rate = c(0.0595, 0.1062, 0.0958, 0.0727, 0.5521, 0.3182),
  stringsAsFactors = FALSE
)
make_table(
  deployment_training,
  caption = 'Training volume for the shipped 2026-27 deployment vintage.',
  digits = 4
)

## ----season-results, echo = FALSE---------------------------------------------
season_results <- data.frame(
  season = c(
    '2013-14', '2014-15', '2015-16', '2016-17', '2017-18',
    '2018-19', '2019-20', '2020-21', '2021-22', '2022-23',
    '2023-24', '2024-25', '2025-26'
  ),
  rows = c(
    112051, 110922, 110263, 111708, 120543, 118438, 105028,
    79111, 122341, 122701, 123126, 120445, 120129
  ),
  goal_rate = c(
    0.0670, 0.0665, 0.0660, 0.0660, 0.0679, 0.0697, 0.0701,
    0.0712, 0.0730, 0.0736, 0.0712, 0.0714, 0.0736
  ),
  xg_rate = c(
    0.0665, 0.0664, 0.0669, 0.0666, 0.0664, 0.0674, 0.0694,
    0.0690, 0.0730, 0.0764, 0.0720, 0.0693, 0.0761
  ),
  roc_auc = c(
    0.7868, 0.7807, 0.7814, 0.7767, 0.7793, 0.7790, 0.7791,
    0.7843, 0.7756, 0.7685, 0.7737, 0.7812, 0.7945
  ),
  calibration_ratio = c(
    1.0065, 1.0011, 0.9876, 0.9918, 1.0224, 1.0328, 1.0093,
    1.0332, 1.0012, 0.9626, 0.9899, 1.0309, 0.9669
  ),
  stringsAsFactors = FALSE
)
make_table(
  season_results,
  caption = 'Completed-season xG evaluation by target season.',
  digits = 4
)

## ----evaluation-plot, echo = FALSE, fig.cap = 'Observed goal rate and xG rate by completed target season.'----
old_par <- graphics::par(no.readonly = TRUE)
graphics::par(mar = c(7, 4, 3, 1))
graphics::plot(
  seq_len(nrow(season_results)),
  season_results[['goal_rate']],
  type = 'b',
  pch = 19,
  lwd = 2,
  col = '#1d3557',
  xaxt = 'n',
  ylim = range(c(season_results[['goal_rate']], season_results[['xg_rate']])),
  xlab = '',
  ylab = 'Rate'
)
graphics::lines(
  seq_len(nrow(season_results)),
  season_results[['xg_rate']],
  type = 'b',
  pch = 17,
  lwd = 2,
  col = '#e63946'
)
graphics::axis(
  side = 1,
  at = seq_len(nrow(season_results)),
  labels = season_results[['season']],
  las = 2,
  cex.axis = 0.75
)
graphics::mtext('Target Season', side = 1, line = 5)
graphics::legend(
  'topleft',
  legend = c('Observed goal rate', 'Average xG'),
  col = c('#1d3557', '#e63946'),
  pch = c(19, 17),
  lwd = 2,
  bty = 'n'
)
graphics::par(old_par)

