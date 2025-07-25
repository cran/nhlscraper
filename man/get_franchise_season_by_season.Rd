% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Team.R
\name{get_franchise_season_by_season}
\alias{get_franchise_season_by_season}
\title{Get all franchises' season-by-season results}
\usage{
get_franchise_season_by_season()
}
\value{
tibble with one row per franchise's season
}
\description{
\code{get_franchise_season_by_season()} retrieves information on each franchise's season, including but not limited to their ID, decision, final playoff round, and statistics.
}
\examples{
all_franchise_sbs <- get_franchise_season_by_season()
}
