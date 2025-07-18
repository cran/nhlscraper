% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Team.R
\name{get_franchises}
\alias{get_franchises}
\title{Get all franchises}
\usage{
get_franchises()
}
\value{
tibble with one row per franchise
}
\description{
\code{get_franchises()} retrieves information on each franchise, including but not limited to their ID; first and last seasons' IDs; captain, coach, and general manager histories; and retired numbers.
}
\examples{
all_franchises <- get_franchises()
}
