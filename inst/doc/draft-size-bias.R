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
# Pull draft picks.
draft_tbl <- nhlscraper::draft_picks()

# Keep modern skater sample.
draft_tbl <- draft_tbl[
  draft_tbl[['draftYear']] >= 1979 &
    draft_tbl[['roundNumber']] <= 7 &
    draft_tbl[['positionCode']] != 'G' &
    !is.na(draft_tbl[['height']]) &
    !is.na(draft_tbl[['weight']]),
  ,
  drop = FALSE
]

# Create analysis buckets.
draft_tbl[['roundBucket']] <- ifelse(
  draft_tbl[['roundNumber']] == 1,
  'Round 1',
  'Rounds 2-7'
)
draft_tbl[['era']] <- cut(
  draft_tbl[['draftYear']],
  breaks = c(1978, 1989, 1999, 2009, 2019, Inf),
  labels = c(
    '1979-1989',
    '1990-1999',
    '2000-2009',
    '2010-2019',
    '2020-present'
  )
)
draft_tbl[['positionBucket']] <- ifelse(
  draft_tbl[['positionCode']] == 'D',
  'Defense',
  'Forward'
)
draft_tbl[['tallSkater']] <- draft_tbl[['height']] >= 74
draft_tbl[['bigSkater']] <- draft_tbl[['height']] >= 74 &
  draft_tbl[['weight']] >= 205
nrow(draft_tbl)

## ----era-table----------------------------------------------------------------
# Summarize size by era and round bucket.
era_summary <- aggregate(
  cbind(height, weight, tallSkater, bigSkater) ~ era + roundBucket,
  data = draft_tbl,
  FUN = mean
)
era_counts <- aggregate(
  height ~ era + roundBucket,
  data = draft_tbl,
  FUN = length
)
names(era_counts)[names(era_counts) == 'height'] <- 'n'
era_summary <- merge(
  era_summary,
  era_counts,
  by = c('era', 'roundBucket')
)
era_summary <- era_summary[, c(
  'era',
  'roundBucket',
  'n',
  'height',
  'weight',
  'tallSkater',
  'bigSkater'
)]
make_table(
  era_summary,
  caption = 'Drafted skater size by era and draft bucket.',
  digits = 3
)

## ----rolling-data-------------------------------------------------------------
# Compute annual first-round and later-round height.
annual_height <- aggregate(
  height ~ draftYear + roundBucket,
  data = draft_tbl,
  FUN = mean
)
annual_height <- annual_height[order(
  annual_height[['roundBucket']],
  annual_height[['draftYear']]
), ]
annual_height[['rollHeight']] <- ave(
  annual_height[['height']],
  annual_height[['roundBucket']],
  FUN = function(x) as.numeric(stats::filter(x, rep(1 / 5, 5), sides = 2))
)
round_one <- annual_height[annual_height[['roundBucket']] == 'Round 1', ]
later_rounds <- annual_height[annual_height[['roundBucket']] == 'Rounds 2-7', ]

## ----rolling-plot, fig.cap = 'Five-draft rolling average height by draft bucket.'----
graphics::plot(
  round_one[['draftYear']],
  round_one[['rollHeight']],
  type = 'l',
  lwd = 2.5,
  col = '#003049',
  ylim = range(annual_height[['rollHeight']], na.rm = TRUE),
  xlab = 'Draft Year',
  ylab = 'Average Height (Inches)'
)
graphics::lines(
  later_rounds[['draftYear']],
  later_rounds[['rollHeight']],
  lwd = 2.5,
  col = '#f77f00'
)
graphics::abline(v = c(1990, 2000, 2010, 2020), lty = 3, col = '#adb5bd')
graphics::legend(
  'topright',
  legend = c('Round 1', 'Rounds 2-7'),
  col = c('#003049', '#f77f00'),
  lwd = 2.5,
  bty = 'n'
)

## ----big-share-table----------------------------------------------------------
# Summarize big-skater share by era and round bucket.
big_share <- aggregate(
  bigSkater ~ era + roundBucket,
  data = draft_tbl,
  FUN = mean
)
big_share <- merge(
  big_share,
  era_counts,
  by = c('era', 'roundBucket')
)
big_share <- big_share[
  order(big_share[['era']], big_share[['roundBucket']]),
]
make_table(
  big_share,
  caption = 'Share of drafted skaters at least 6-foot-2 and 205 pounds.',
  digits = 3
)

## ----big-share-plot, fig.cap = 'Share of big skaters by era and round bucket.'----
# Plot big-skater share by era.
round_levels <- c('Round 1', 'Rounds 2-7')
big_matrix <- rbind(
  big_share[['bigSkater']][big_share[['roundBucket']] == round_levels[1]],
  big_share[['bigSkater']][big_share[['roundBucket']] == round_levels[2]]
)
graphics::barplot(
  big_matrix,
  beside = TRUE,
  col = c('#2d6a4f', '#95d5b2'),
  border = NA,
  ylim = c(0, max(big_matrix, na.rm = TRUE) * 1.25),
  names.arg = levels(draft_tbl[['era']]),
  las = 2,
  ylab = 'Share of Big Skaters'
)
graphics::legend(
  'topright',
  legend = round_levels,
  fill = c('#2d6a4f', '#95d5b2'),
  bty = 'n'
)

## ----position-table-----------------------------------------------------------
# Summarize size by era and position family.
position_summary <- aggregate(
  cbind(height, weight, bigSkater) ~ era + positionBucket,
  data = draft_tbl,
  FUN = mean
)
position_counts <- aggregate(
  height ~ era + positionBucket,
  data = draft_tbl,
  FUN = length
)
names(position_counts)[names(position_counts) == 'height'] <- 'n'
position_summary <- merge(
  position_summary,
  position_counts,
  by = c('era', 'positionBucket')
)
make_table(
  position_summary,
  caption = 'Drafted skater size by era and position family.',
  digits = 3
)

## ----model--------------------------------------------------------------------
# Fit height model.
height_fit <- stats::lm(
  height ~ era + I(roundNumber == 1) + positionBucket,
  data = draft_tbl
)
height_fit_tbl <- as.data.frame(summary(height_fit)$coefficients)
height_fit_tbl[['term']] <- rownames(height_fit_tbl)
rownames(height_fit_tbl) <- NULL
height_fit_tbl <- height_fit_tbl[, c(
  'term',
  'Estimate',
  'Std. Error',
  't value',
  'Pr(>|t|)'
)]
make_table(
  height_fit_tbl,
  caption = 'Linear model of drafted skater height.',
  digits = 4
)

