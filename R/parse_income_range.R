#' Parse and string specifying an income range
#'
#' @param income_range
#' @param stat_fun
#'
#' @return a number
#' @export
#'
#' @examples
#' parse_income_range("less than $25,000")
#' parse_income_range("$25,000-$50,000")
#' parse_income_range("Income: $30,000"
#' parse_income_range("No income information")
#'

parse_income_range <- function(income_range, stat_fun) {

  require(stringr)
  # Use regular expressions to find the income value
  # Search for a dollar sign followed by numbers and optional comma
    matches <- str_extract_all(income_range), "\\$[0-9,]+")

  if (length(matches) == 0) {
    # No valid income value found
    return(NA)
  }
else {
    # Extract all numeric values from the matches
    numeric_values <- as.numeric(gsub("[^0-9]", "", unlist(matches)))

    # Find the largest numeric value
    largest_value <- max(numeric_values)

    return(largest_value)
  }
}
