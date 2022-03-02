#' Calculates the academic, fiscal, or application year given a date object
#'
#' @param date a date object
#' @param start a date object or string coercible into a date
#'
#' @export
#'
parse_acad_year <- function(date,
                            start = as.Date("1970-07-01")) {


  start.month <- lubridate::month(start)
  start.day   <- lubridate::day(start)


  if (lubridate::month(date) >= start.month &
      lubridate::day(date)   >= start.day) {
    year <- lubridate::year(date) + 1
  } else {
    year <- lubridate::year(date)
  }

  return(year)
}
