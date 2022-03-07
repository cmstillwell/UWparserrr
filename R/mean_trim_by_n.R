#' Calculate a trimmed mean, removing n values from each end of the sequence
#'
#' @param x A numeric vector
#' @param n.trim A number
#'
#' @return The vector mean with 'n.trim' values removed from each end before calculating
#' @export
#'
mean_trim_by_n <- function(x, n.trim) {

  # ensure both inputs are numeric and that the trim value atomic
  assertthat::is.number(x)
  assertthat::is.number(n.trim)
  assertthat::assert_that(length(n.trim) == 1)

  n.trim <- round(n.trim)

  trim.pct <- (1 / length(x[!is.na(x)])) * n.trim

  trim.mean <- mean(x,
                    trim = trim.pct,
                    na.rm = TRUE)

  return(trim.mean)

}
