#' Get all seasons
#' 
#' `get_seasons()` retrieves information on each season, including but not limited to their ID; start and end dates; number of regular season and playoff games; Stanley Cup owner; Olympics participation; entry and supplemental draft, conference-division, win-tie-loss, and wildcard regulations.
#' 
#' @return tibble with one row per season
#' @examples
#' all_seasons <- get_seasons()
#' @export

get_seasons <- function() {
  out <- nhl_api(
    path='season',
    type=2
  )
  return(tibble::as_tibble(out$data))
}

#' Get standings information for all seasons
#' 
#' `get_standings_information()` retrieves information on each season, including but not limited to their ID; start and end dates for standings; and conference-division, win-tie-loss, and wildcard regulations. May soon be merged with `get_seasons()`.
#' 
#' @return tibble with one row per season
#' @examples
#' standings_info <- get_standings_information()
#' @export

get_standings_information <- function() {
  out <- nhl_api(
    path='standings-season',
    type=1
  )
  return(tibble::as_tibble(out$seasons))
}

#' Get standings by date
#' 
#' `get_standings()` retrieves information on each team for a given `date`, including but not limited to their ID; name; conference; division; season, recent, and home-away statistics; and waiver sequence. Access `get_seasons()` for `date` reference.
#' 
#' @param date string in 'YYYY-MM-DD'
#' @return tibble with one row per team
#' @examples
#' standings_2025_01_02 <- get_standings(date='2025-01-02')
#' @export

get_standings <- function(date='2025-01-01') {
  if (!grepl('^\\d{4}-\\d{2}-\\d{2}$', date)) {
    stop('`date` must be in \'YYYY-MM-DD\' format', call.=FALSE)
  }
  out <- nhl_api(
    path=sprintf('standings/%s', date),
    type=1
  )
  return(tibble::as_tibble(out$standings))
}

#' Get schedule by date
#' 
#' `get_schedule()` retrieves information on each game for a given `date`, including but not limited to their ID; type; venue; start time; tickets link; and home and away teams' IDs, names, and scores. Access `get_seasons()` for `date` reference. Unable to conclude any major difference versus `get_scores()`; may soon be deprecated.
#' 
#' @param date string in 'YYYY-MM-DD'
#' @return tibble with one row per game
#' @examples
#' schedule_2025_01_02 <- get_schedule(date='2025-01-02')
#' @export

get_schedule <- function(date='2025-01-01') {
  if (!grepl('^\\d{4}-\\d{2}-\\d{2}$', date)) {
    stop('`date` must be in \'YYYY-MM-DD\' format', call.=FALSE)
  }
  out <- nhl_api(
    path=sprintf('schedule/%s', date),
    type=1
  )
  if (is.null(out$gameWeek)) {
    return(tibble::tibble())
  }
  sub <- out$gameWeek[out$gameWeek$date==date, , drop=FALSE]
  if (nrow(sub)==0) {
    return(tibble::tibble())
  }
  tibble::as_tibble(sub$games[[1]])
}
