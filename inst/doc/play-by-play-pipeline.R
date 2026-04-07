## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fetch-example, eval = FALSE----------------------------------------------
# gc  <- nhlscraper::gc_play_by_play(2023030417)
# wsc <- nhlscraper::wsc_play_by_play(2023030417)

## ----shift-example, eval = FALSE----------------------------------------------
# pbp    <- nhlscraper::gc_play_by_play(2023030417)
# shifts <- nhlscraper::shift_chart(2023030417)
# pbp_with_shift_times <- nhlscraper::add_shift_times(pbp, shifts)

