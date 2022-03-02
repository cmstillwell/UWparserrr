#' Parse gender data and fortify with a standardized, ternary gender field
#'
#' @param data A data.frame or something coercible to this
#' @param gender.field An unquoted field within 'data' where gender data lies
#' @param parsed.gender An unquoted field name to be appended to 'data'
#'
#' @return The input data.frame fortified with a standardized gender column
#'
#' @export
#'
parse_gender <- function(data,
                         gender.field = gender_identity,
                         parsed.gender = gender) {
  require(dplyr)
  require(stringr)

  gender.field <- enquo(gender.field)

  stopifnot(is.data.frame(data))

  data <- data %>%
    mutate({{ parsed.gender }} := case_when(
      str_detect(!!gender.field, "^[M|m](ale)?$")   ~ "Male",
      str_detect(!!gender.field, "^[F|f](emale)?$") ~ "Female",
      TRUE                                          ~ "Nonbinary"))

  return(data)
}
