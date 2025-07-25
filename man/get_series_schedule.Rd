% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Playoff.R
\name{get_series_schedule}
\alias{get_series_schedule}
\title{Get schedule by season and series}
\usage{
get_series_schedule(season = get_season_now()$seasonId, series = "a")
}
\arguments{
\item{season}{integer in YYYYYYYY}

\item{series}{string 1-letter Code}
}
\value{
tibble with one row per game
}
\description{
\code{get_series_schedule()} retrieves information on each game for a given set of \code{season} and \code{series}, including but not limited to their ID; venue; start date and time; and home and away teams' IDs, names, and scores. Access \code{get_seasons()} for \code{season} and \code{get_bracket()} for \code{series} references.
}
\examples{
COL_DAL_schedule_20242025 <- get_series_schedule(season=20242025, series='f')
}
