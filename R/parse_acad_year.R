#' Calculates the academic, fiscal, or application year given a date object
#'
#' @param date a date object
#' @param start a date object or string coercible into a date
#'
#' @export
#'
parse_acad_year <- function(date,
                            start = as.Date("1970-07-01")) {

  assertthat::is.date(date)

  start.month <- lubridate::month(start)
  start.day   <- lubridate::day(start)

  year <- ifelse(test = lubridate::month(date) >= start.month
                         &
                        lubridate::day(date)   >= start.day,
                 yes  = lubridate::year(date) + 1,
                 no   = lubridate::year(date))

  return(year)
}

