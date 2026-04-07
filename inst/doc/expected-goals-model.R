## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

make_table <- function(x, caption, digits = 3) {
  knitr::kable(x, caption = caption, digits = digits)
}

## ----partition-table----------------------------------------------------------
partition_table <- data.frame(
  partition = c("sd", "ev", "pp", "sh", "en", "ps"),
  meaning = c(
    "Regulation 5v5 without empty nets",
    "Other even-strength states outside standard 5v5",
    "Shooting team has a skater advantage",
    "Shooting team is short-handed",
    "Opponent net is empty",
    "Penalty-shot and shootout-style situations"
  ),
  stringsAsFactors = FALSE
)
make_table(
  partition_table,
  caption = "The six shot partitions used by nhlscraper's xG model."
)

## ----train-table--------------------------------------------------------------
train_summary <- data.frame(
  partition = c("sd", "ev", "pp", "sh", "en", "ps"),
  games = c(2798, 1280, 2793, 2241, 1245, 230),
  rows = c(188930, 4907, 38903, 5539, 1828, 1188),
  goal_rate = c(0.0593, 0.1113, 0.0973, 0.0738, 0.5739, 0.3157)
)
make_table(
  train_summary,
  caption = "Training sample size and goal rate by partition.",
  digits = 4
)

## ----cv-table-----------------------------------------------------------------
cv_summary <- data.frame(
  partition = c("sd", "ev", "pp", "sh", "en", "ps"),
  cv_log_loss = c(0.1986, 0.3314, 0.3036, 0.2211, 0.6191, 0.6241),
  cv_roc_auc = c(0.7718, 0.6728, 0.6693, 0.7960, 0.7002, 0.5264),
  cv_brier = c(0.0525, 0.0953, 0.0852, 0.0628, 0.2161, 0.2163)
)

make_table(
  cv_summary,
  caption = "Grouped cross-validation diagnostics at the selected ridge penalty.",
  digits = 4
)

## ----overall-table------------------------------------------------------------
overall_results <- data.frame(
  season = c("2021-22", "2023-24", "2025-26"),
  rows = c(122341, 122180, 74169),
  goal_rate = c(0.0730, 0.0718, 0.0744),
  xg_rate = c(0.0757, 0.0715, 0.0779),
  log_loss = c(0.2316, 0.2222, 0.2319),
  roc_auc = c(0.7463, 0.7775, 0.7617),
  calibration_ratio = c(1.0363, 0.9958, 1.0465)
)
make_table(
  overall_results,
  caption = "External evaluation summary by season.",
  digits = 4
)

## ----future-partition-table---------------------------------------------------
future_partition_results <- data.frame(
  partition = c("sd", "ev", "pp", "sh", "en", "ps"),
  rows = c(57157, 1750, 12489, 1610, 604, 559),
  log_loss = c(0.2056, 0.3109, 0.3045, 0.2198, 0.5959, 0.6336),
  roc_auc = c(0.7615, 0.7021, 0.6517, 0.7844, 0.7400, 0.5131),
  calibration_ratio = c(1.0324, 1.1482, 1.0818, 1.1837, 1.0115, 0.9623)
)
make_table(
  future_partition_results,
  caption = "Future-season (`2025-26`) external results by partition.",
  digits = 4
)

