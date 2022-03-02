#' Calculates the academic, fiscal, or application year given a date object
#'
#' @param date a date object
#' @param acad_year a 4-digit integer giving the academic, fiscal, or admissions cycle year
#' @param anchor_date the date on which to normalize the input date(s)
#' @param format what the function should return, either a date or count of days from the anchor date
#'
#' @export

norm_date_on_year <- function(date,
                              acad_year = format(Sys.Date(), "%Y") |> as.numeric(),
                              anchor_date = "2020-01-01",
                              format = "date") {

  assertthat::assert_that(!is.na(lubridate::parse_date_time(anchor_date, orders = "ymd")))
  anchor_date <- lubridate::parse_date_time(anchor_date, orders = "ymd")
  anchor <- list(
    date = anchor_date,
    yr   = format(anchor_date, "%Y") |> as.numeric(),
    mon  = format(anchor_date, "%m") |> as.numeric(),
    day  = format(anchor_date, "%d") |> as.numeric(),
  )

  assertthat::assert_that(assertthat::is.count(acad_year))

  date <- list(
    full   = ,
    yr     = format(date, "%Y") |> as.numeric(),
    mon    = format(date, "%m") |> as.numeric(),
    day    = format(date, "%d") |> as.numeric(),
    yr_adj = format(date, "%Y") |> as.numeric() - acad_year
  )

  assertthat::assert_that(grepl("date|count", format))
  if (format == "date") {
    date_norm <- ISOdate(anchor$yr + date$yr_adj, date$mon, date$day)
  } else if (format == "count") {
    date_norm <- difftime(ISOdate(anchor$yr + date$yr_adj, date$mon, date$day),
                          anchor_date,
                          units = "days")
  }

  return(date_norm)
}
