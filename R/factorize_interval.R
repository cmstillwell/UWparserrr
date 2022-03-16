#' Converts a lubridate interval object into a character factor
#'
#' @param x a lubridate interval object
#' @return a string vector
#' @export
#'
factorize_interval <- function(x) {
  require(stringr)

  fctr <- x |>
    as.character() |>
    str_replace_all(pattern = " UTC", replacement = "") |>
    str_replace_all(pattern = "--", replacement = " to ") |>
    as.factor()

  return(fctr)
}
