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

## ----source-table, echo = FALSE-----------------------------------------------
source_table <- data.frame(
  source = c(
    'GameCenter API',
    'World Showcase API',
    'HTML play-by-play report',
    'Stats shift-chart API',
    'HTML shift reports'
  ),
  used_by = c(
    'gc_play_by_play(), game metadata for WSC',
    'wsc_play_by_play()',
    'gc_play_by_play(), wsc_play_by_play()',
    'shift_chart()',
    'shift_chart() fallback'
  ),
  role = c(
    'Authoritative event stream for GameCenter rows and common metadata.',
    'Alternate event stream with UTC timestamps and fewer clip fields.',
    'Primary source for event-level on-ice goalie and skater IDs.',
    'Preferred source for player shift start/end clocks.',
    'Fallback source for shift start/end clocks when stats API rows are missing.'
  ),
  stringsAsFactors = FALSE
)
make_table(
  source_table,
  caption = 'Public sources used by the play-by-play and shift-chart pipeline.'
)

## ----basic-use, eval = FALSE--------------------------------------------------
# pbp <- nhlscraper::gc_play_by_play(2023030417)
# shifts <- nhlscraper::shift_chart(2023030417)
# pbp <- nhlscraper::add_shift_times(pbp, shifts)

## ----pipeline-plot, echo = FALSE, fig.width = 7.5, fig.height = 5.6, fig.cap = 'Conceptual order of the play-by-play cleanup pipeline.'----
steps <- c(
  'Fetch API and HTML',
  'Normalize source names',
  'Repair clear order defects',
  'Parse situationCode',
  'Normalize coordinates',
  'Add shot context',
  'Parse HTML on-ice rows',
  'Match HTML to API rows',
  'Finalize public schema'
)
box_x <- c(1, 2, 3, 3, 2, 1, 1, 2, 3)
box_y <- c(3, 3, 3, 2, 2, 2, 1, 1, 1)
box_w <- 0.78
box_h <- 0.34
draw_box <- function(x, y, label, number) {
  wrapped_label <- paste(strwrap(paste0(number, '. ', label), width = 18), collapse = '\n')
  graphics::rect(
    xleft   = x - box_w / 2,
    ybottom = y - box_h / 2,
    xright  = x + box_w / 2,
    ytop    = y + box_h / 2,
    col     = '#edf6f9',
    border  = '#006d77',
    lwd     = 1.5
  )
  graphics::text(
    x      = x,
    y      = y,
    labels = wrapped_label,
    cex    = 0.74,
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
    col    = '#6c757d'
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
  ylim = c(0.55, 3.45)
)
for (i in seq_len(length(steps) - 1L)) {
  draw_arrow(i, i + 1L)
}
for (i in seq_along(steps)) {
  draw_box(box_x[i], box_y[i], steps[i], i)
}

## ----repair-table, echo = FALSE-----------------------------------------------
repair_table <- data.frame(
  issue = c(
    'Source-specific column names',
    'Goal row without shootingPlayerId',
    'Blocked-shot zone perspective',
    'Impossible period-boundary order',
    'Missing public output columns'
  ),
  action = c(
    'Rename to stable public names such as periodNumber and eventTypeDescKey.',
    'Use scoringPlayerId as shootingPlayerId when the scorer is known.',
    'Normalize blocked shots to the shooting-team perspective.',
    'Drop or repair only the small set of rows that violate clock boundaries.',
    'Allocate typed NA columns so GC and WSC outputs keep stable schemas.'
  ),
  stringsAsFactors = FALSE
)
make_table(
  repair_table,
  caption = 'Conservative repairs made before public output is finalized.'
)

## ----match-table, echo = FALSE------------------------------------------------
match_table <- data.frame(
  feature = c(
    'Period',
    'Elapsed seconds',
    'Event type',
    'Event-owning team',
    'Primary actor',
    'Secondary and tertiary actors',
    'Local event order'
  ),
  why_it_matters = c(
    'Prevents same-clock matches across periods.',
    'Keeps the match near the official event time.',
    'Separates faceoffs, shots, hits, penalties, goals, and stoppages.',
    'Prevents same-time events by opposite teams from swapping.',
    'Anchors the event to the shooter, scorer, hitter, winner, or penalized player.',
    'Disambiguates goals, blocks, faceoffs, hits, and penalties.',
    'Avoids backwards matches in duplicate clusters.'
  ),
  stringsAsFactors = FALSE
)
make_table(
  match_table,
  caption = 'Signals used when matching HTML rows back to API events.'
)

## ----workflow, eval = FALSE---------------------------------------------------
# # Load cleaned event stream.
# pbp <- nhlscraper::gc_play_by_play(2023030417)
# 
# # Add shift timing when fatigue or shift length matters.
# shifts <- nhlscraper::shift_chart(2023030417)
# pbp <- nhlscraper::add_shift_times(pbp, shifts)
# 
# # Add movement context when previous-event geometry matters.
# pbp <- nhlscraper::add_deltas(pbp)
# 
# # Score shots when chance quality matters.
# pbp <- nhlscraper::calculate_expected_goals(pbp)

