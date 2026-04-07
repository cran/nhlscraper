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
# Pull draft picks and keep skaters with measured size.
draft_tbl <- nhlscraper::draft_picks()
draft_tbl <- draft_tbl[
  draft_tbl[['draftYear']] >= 1979 &
    draft_tbl[['positionCode']] != 'G' &
    !is.na(draft_tbl[['height']]) &
    !is.na(draft_tbl[['weight']]),
]

# Create era, round, and position buckets.
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
    '2020-2025'
  )
)
draft_tbl[['positionBucket']] <- ifelse(
  draft_tbl[['positionCode']] == 'D',
  'Defense',
  'Forward'
)
draft_tbl[['tallSkater']] <- draft_tbl[['height']] >= 74
nrow(draft_tbl)

## ----era-table----------------------------------------------------------------
# Summarize size by era and round bucket.
era_summary <- aggregate(
  cbind(height, weight) ~ era + roundBucket,
  data = draft_tbl,
  FUN = mean
)
make_table(
  era_summary,
  caption = 'Average drafted skater size by era and draft bucket.'
)

## ----rolling-data-------------------------------------------------------------
# Compute annual mean height by round bucket.
round1_annual <- aggregate(
  height ~ draftYear,
  data = draft_tbl[draft_tbl[['roundBucket']] == 'Round 1', ],
  FUN = mean
)
later_annual <- aggregate(
  height ~ draftYear,
  data = draft_tbl[draft_tbl[['roundBucket']] == 'Rounds 2-7', ],
  FUN = mean
)

# Smooth annual means with five-draft rolling averages.
round1_annual[['rollHeight']] <- as.numeric(stats::filter(
  round1_annual[['height']],
  rep(1 / 5, 5),
  sides = 2
))
later_annual[['rollHeight']] <- as.numeric(stats::filter(
  later_annual[['height']],
  rep(1 / 5, 5),
  sides = 2
))

## ----rolling-plot, fig.cap = 'Five-draft rolling average height for first-round skaters and later-round skaters.'----
graphics::plot(
  round1_annual[['draftYear']],
  round1_annual[['rollHeight']],
  type = 'l',
  lwd = 2,
  col = '#0f4c5c',
  ylim = range(
    c(round1_annual[['rollHeight']], later_annual[['rollHeight']]),
    na.rm = TRUE
  ),
  xlab = 'Draft Year',
  ylab = 'Average Height (Inches)'
)
graphics::lines(
  later_annual[['draftYear']],
  later_annual[['rollHeight']],
  lwd = 2,
  col = '#e36414'
)
graphics::legend(
  'topright',
  legend = c('Round 1', 'Rounds 2-7'),
  col = c('#0f4c5c', '#e36414'),
  lwd = 2,
  bty = 'n'
)

## ----tall-share---------------------------------------------------------------
# Summarize share of taller skaters by era.
tall_share <- aggregate(
  tallSkater ~ era + roundBucket,
  data = draft_tbl,
  FUN = mean
)
tall_counts <- aggregate(
  height ~ era + roundBucket,
  data = draft_tbl,
  FUN = length
)
names(tall_counts)[names(tall_counts) == 'height'] <- 'n'
tall_share <- merge(tall_share, tall_counts, by = c('era', 'roundBucket'))

make_table(
  tall_share,
  caption = 'Share of drafted skaters measuring at least 6-foot-2.'
)

## ----tall-share-plot, fig.cap = 'Share of drafted skaters at least 6-foot-2 by era and round bucket.'----
# Plot tall-skater shares by era.
tall_matrix <- rbind(
  tall_share[['tallSkater']][tall_share[['roundBucket']] == 'Round 1'],
  tall_share[['tallSkater']][tall_share[['roundBucket']] == 'Rounds 2-7']
)
graphics::barplot(
  tall_matrix,
  beside = TRUE,
  col = c('#1b4332', '#95d5b2'),
  ylim = c(0, 0.7),
  names.arg = levels(draft_tbl[['era']]),
  ylab = 'Share At Least 6-Foot-2',
  xlab = 'Draft Era'
)
graphics::legend(
  'topright',
  legend = c('Round 1', 'Rounds 2-7'),
  fill = c('#1b4332', '#95d5b2'),
  bty = 'n'
)

## ----position-table-----------------------------------------------------------
# Summarize size by era and position family.
position_summary <- aggregate(
  cbind(height, weight) ~ era + positionBucket,
  data = draft_tbl,
  FUN = mean
)
make_table(
  position_summary,
  caption = 'Average drafted skater size by era and position family.'
)

## ----model--------------------------------------------------------------------
# Fit simple draft-height model.
draft_fit <- stats::lm(
  height ~ draftYear + I(roundNumber == 1) + I(positionCode == 'D'),
  data = draft_tbl
)
draft_fit_tbl <- as.data.frame(summary(draft_fit)$coefficients)
draft_fit_tbl[['term']] <- rownames(draft_fit_tbl)
rownames(draft_fit_tbl) <- NULL
draft_fit_tbl[['term']] <- c(
  'Intercept',
  'Draft year',
  'First-round indicator',
  'Defense indicator'
)
draft_fit_tbl <- draft_fit_tbl[, c(
  'term',
  'Estimate',
  'Std. Error',
  't value',
  'Pr(>|t|)'
)]
make_table(
  draft_fit_tbl,
  caption = 'Linear model of drafted skater height.',
  digits = 4
)

