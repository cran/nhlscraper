% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Game.R
\name{get_gc_play_by_play}
\alias{get_gc_play_by_play}
\title{Get GameCenter (GC) play-by-play by game}
\usage{
get_gc_play_by_play(game = 2024020602)
}
\arguments{
\item{game}{integer Game ID}
}
\value{
tibble with one row per play
}
\description{
\code{get_gc_play_by_play()} retrieves GC-provided information on each play for a given \code{game}, including but not limited to their ID; type; time of occurrence; winning, losing, blocking, shooting, hitting, hit, scoring, assisting, committed-by, drawn-by, and/or served-by player IDs; and X and Y coordinates. Access \code{get_games()} for \code{game} reference.
}
\examples{
gc_pbp_2024030411 <- get_gc_play_by_play(game=2024030411)
}
