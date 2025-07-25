% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Other.R
\name{get_partner_odds}
\alias{get_partner_odds}
\title{Get partner odds as of now}
\usage{
get_partner_odds(country = "US")
}
\arguments{
\item{country}{string 2-letter Code}
}
\value{
tibble with one row per game
}
\description{
\code{get_partner_odds()} retrieves partner-provided information on each game for a given \code{country}, including but not limited to their ID and home and away team odds. Access \code{get_countries()} for \code{country} reference.
}
\examples{
partner_odds_now_CA <- get_partner_odds(country='CA')
}
