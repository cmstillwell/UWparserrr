#' Calculates the academic, fiscal, or application year given a date object
#'
#' @param date a date object
#' @param acad_year a 4-digit integer giving the academic, fiscal, or admissions cycle year
#' @param anchor_date the date on which to normalize the input date(s)
#' @param format what the function should return, either a date or count of days from the anchor date

norm_date_on_year <- function(date,
                              acad_year,
                              anchor_date = as.Date("2020-01-01"),
                              format = "date") {

  is.date <- function(x) {inherits(x, "Date")}

  assertthat::assert_that(is.numeric.Date(date))
  assertthat::assert_that(assertthat::is.count(acad_year))
  assertthat::assert_that(is.numeric.Date(anchor_date))

  date <- list(
    full   = as.Date(date),
    yr     = strptime(date, format = "%m"),
    mon    = clock::get_month(date),
    day    = clock::get_day(date),
    yr_adj = yr - acad_year
  )

  date_norm <- NULL

  return(date_norm)
}


test <- c("2020-07-01", "2020-10-31", "2021-01-14", "2021-06-30", "2021-07-12") %>%
  as.Date()

norm_date_on_year(test)
