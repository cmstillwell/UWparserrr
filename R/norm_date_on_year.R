#' Calculates the academic, fiscal, or application year given a date object
#'
#' @param date a date object
#' @param acad_year a 4-digit integer giving the academic, fiscal, or admissions cycle year
#' @param anchor_date the date on which to normalize the input date(s), i.e., the start date
#' @param anchor_year a positive integer giving the year on which to normalize the dates
#' @param format what the function should return, either a date or count of days from the anchor date
#'
#' @exportd

norm_date_on_year <- function(date,
                              acad_year = format(Sys.Date(), "%Y") |> as.numeric(),
                              anchor_date = "2020-01-01",
                              anchor_year,
                              format = "date") {
  require(assertthat)
  require(lubridate)

  # Ensure anchor_date is a valid date
  assert_that(!is.na(parse_date_time(anchor_date, orders = "ymd")))

  # If it is defined, ensure anchor_year is a positive integer
  if (!missing(anchor_year)) {
    assert_that(
      is.count(anchor_year),
      msg = 'Since "anchor_year" is defined, it must be a valid year, i.e., a positive integer.')
  }

  anchor_date <- parse_date_time(anchor_date, orders = "ymd")
  anchor <- list(
    date = anchor_date,
    yr   = ifelse(missing(anchor_year),
                  format.Date(anchor_date, "%Y") |> as.numeric(),
                  anchor_year),
    mon  = format.Date(anchor_date, "%m") |> as.numeric(),
    day  = format.Date(anchor_date, "%d") |> as.numeric()
  )

  # Ensure acad_year is a vector of positive integers
  assert_that(all(is.numeric(acad_year)), msg = '"acad_year" must be a vector of positive integers.')
  assert_that(all(acad_year > 0), msg = '"acad_year" must be a vector of positive integers.')

  date <- list(
    full   = date,
    yr     = format.Date(date, "%Y") |> as.numeric(),
    mon    = format.Date(date, "%m") |> as.numeric(),
    day    = format.Date(date, "%d") |> as.numeric(),
    yr_adj = format.Date(date, "%Y") |> as.numeric() - acad_year
  )

  assert_that(grepl("date|count", format))
  if (format == "date") {
    date_norm <- ISOdate(anchor$yr + date$yr_adj, date$mon, date$day)
  } else if (format == "count") {
    date_norm <- difftime(ISOdate(anchor$yr + date$yr_adj, date$mon, date$day),
                          anchor_date,
                          units = "days")
  }

  return(date_norm)
}
