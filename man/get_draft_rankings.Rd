% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Draft.R
\name{get_draft_rankings}
\alias{get_draft_rankings}
\title{Get draft rankings by year and player-type}
\usage{
get_draft_rankings(year = get_season_now()$seasonId\%/\%10000, player_type = 1)
}
\arguments{
\item{year}{integer in YYYY}

\item{player_type}{integer where 1=North American Skaters,
2=International Skaters, 3=North American Goalies,
and 4=International Goalies}
}
\value{
tibble with one row per player
}
\description{
\code{get_draft_rankings()} retrieves information on each prospect for a given set of \code{year} and \code{player_type}, including but not limited to their name, midterm and final ranks, position, bio-metrics, and birth date and location.
}
\examples{
draft_rankings_2025_1 <- get_draft_rankings(year=2025, player_type=1)
}
