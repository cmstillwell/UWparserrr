#' Parse and re-level ambiguous numerical range factors
#'
#' By default, `fct_num_range()` returns the re-leveled factor
#'
#' This function takes a factor and typically returns the same but re-leveled so
#' that ambiguous numeric ranges (e.g. less than 10, 11-15, 16-20, more than 20)
#' are ordered correctly in the factor levels. The function can also be directed
#' to return a table with the full level parsing, including upper and lower
#' boundaries. This useful in complex cases where this parser is insufficient.
#' The function also removes any currency symbols and thousands separators.
#' It is localized for US - North America where "," is the thousands separator
#' and "." is the decimal separator.
#'
#' @param .fctr A factor or atomic vector that can be coerced into a factor
#' @param .range_sep A string which separates the two halves of the numeric range
#' @param .return A string ("factor" or "table") indicating the function output
#'
#' @returns Either the input factor re-leveled or the table of levels parsed into columns
#'
fct_num_range <- function(.fctr,
                          .range_sep = " - ",
                          .return = "factor") {
  require(stringr)
  require(readr)
  require(dplyr)

  # convert to a factor
  if (is.factor(.fctr) == FALSE) {
    .fctr <- factor(.fctr)
  }

  # save factor levels
  lvl <- levels(.fctr)

  # isolate any levels that don't contain any numbers
  notnum <- str_subset(lvl, "\\d", negate = TRUE)

  # assemble the regex search string for upper boundary
  rgx.upper <- paste("(?<=", .range_sep, ")[\\d,]+", sep = "")

  # make a table of factor levels and their range boundaries
  tbl_lvl_parse <- list(level = lvl) %>%
    as_tibble() %>%
    mutate(lower = parse_number(lvl, na = notnum,),
           upper = parse_number(str_extract(lvl, rgx.upper), na = notnum)) %>%
    arrange(lower, !is.na(upper), desc(upper))

  # set output

  output <- .fctr
  if (.return == "factor") {
      output <- factor(.fctr, levels = tbl_lvl_parse$level)
  } else if (.return == "table") {
      output <- tbl_lvl_parse
  } else if (TRUE) {
      warning('Argument ".return" was not a recognized format. ',
              'In the function call, use "factor" or "table" instead. ',
              'Returning vector returned as a factor without any other change.')
  }

  return(output)

}



