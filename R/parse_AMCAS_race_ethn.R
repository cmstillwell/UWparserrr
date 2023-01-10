# Parser/Fortification Functions ------------------------------------------

#' Parse text-based racial identity field and fortify the dataset with
#' individual fields for each of the IPEDS racial categories.
#'
#' @param data A data.frame or tibble
#' @param race.field Unquoted name of a field
#' @param hispanic.field Unquoted name of a field
#'
#' @return A data.frame or tibble fortified with new columns
#' @export

parse_race_text <- function(data, race.field, hispanic.field) {
  require(tidyverse)

  race.field <- enquo(race.field)
  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(RE_Black      = str_detect(!!race.field,
                                      regex("afric|black", TRUE)),
           RE_Asian      = str_detect(!!race.field, regex("asian", TRUE)),
           RE_AsianURM   = str_detect(!!race.field,
                                      regex("viet|laot|cambod|hmong", TRUE)),
           RE_HawaiianPI = str_detect(!!race.field,
                                      regex("hawaii|pacific", TRUE)),
           RE_Native     = str_detect(!!race.field,
                                      regex("american indian|tribe", TRUE)),
           RE_White      = str_detect(!!race.field, regex("white", TRUE)),
           RE_Hispanic   = str_detect(!!hispanic.field,
                                      regex("(?<!not )hispanic|yes|^y$", TRUE)))
  return(data)
}


#' Parse code-based racial identity field and fortify the dataset with
#' individual fields for each of the IPEDS racial categories.
#'
#' @param data A data.frame containing race/ethnicity data
#' @param race.field Field name for racial self-identifications
#' @param hispanic.field Field name for Hispanic ethnicity self-identification
#'
#' @return Dataframe or tibble fortified with new columns
#' @export

parse_race_code <- function(data, race.field, hispanic.field) {
  require(tidyverse)

  race.field <- enquo(race.field)
  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(RE_Black      = str_detect(!!race.field, "(AA|B|BC|BO)(;|$)"),
           RE_Asian      = str_detect(!!race.field, "((;|^)A|A(C|F|J|K|O|P)|BA)(;|$)"),
           RE_AsianURM   = str_detect(!!race.field, "(AV|CA|LA)(;|$)"),
           RE_HawaiianPI = str_detect(!!race.field, "P(H|O|S)(;|$)"),
           RE_Native     = str_detect(!!race.field, "I(;|$)"),
           RE_White      = str_detect(!!race.field, "W(;|$)"),
           RE_Hispanic   = str_detect(!!hispanic.field,
                                      regex("(?<!not )hispanic|yes|^y$", TRUE)))
  return(data)
}


#' Parse a binary field of Hispanic/Latino designation
#'
#' @param data A data.frame
#' @param hispanic.field A boolean field
#'
#' @return A data.frame
#' @export
#'
parse_hispanic_boolean <- function(data, hispanic.field) {
  require(dplyr)

  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(RE_Hispanic = !!hispanic.field)
  return(data)
}


#' Parse the AMCAS Hispanic code field
#'
#' @param data A data.frame
#' @param hispanic.field Name of Hispanic field from AMCAS
#'
#' @return A data.frame
#' @export

parse_hispanic_code <- function(data, hispanic.field) {
  require(dplyr)
  require(stringr)

  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(RE_Hispanic = str_detect(!!hispanic.field, "(^|;)U(;|$)"))
  return(data)
}


#' Parse a text field containing Hispanic/Latino ethnicity information
#'
#' @param data A data.frame
#' @param hispanic.field A field name
#'
#' @return A fortified data.frame
#' @export

parse_hispanic_text <- function(data, hispanic.field) {
  require(dplyr)
  require(stringr)

  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(RE_Hispanic = str_detect(!!hispanic.field,
                                    regex("(?<!not )hispanic|^y$|yes",
                                          ignore.case = TRUE)))
  return(data)
}


#' Parse the fortified, standardized race and ethnicity columns in the dataset,
#' and fortify with a new column, calculated URM status.
#'
#' @param data A data.frame or tibble
#'
#' @return Data.frame or tibble fortified with new column
#' @export

parse_urm <- function(data) {
  require(tidyverse)

  # Confirm that all the individual URM fields exist in data.
  if (assert_IPEDS_fields(data) == FALSE) {
    stop("'data' must contain all '...' parsed fields. Try running the
        'parse_race' function first.")
  }

  data <- data %>%
    mutate(RE_URM = if_else(RE_Black + RE_AsianURM + RE_HawaiianPI + RE_Native +
                            RE_Hispanic > 0, TRUE, FALSE))

  return(data)
}


#' Creates a single, IPEDS-standardized field for race/ethnicity based on the
#' individual fields created through the 'parse_race' function
#'
#' @param data A dataframe or tibble
#'
#' @return Dataframe or tibble fortified with new column
#' @export

parse_ipeds <- function(data) {
  require(tidyverse)

  # Confirm that all the individual URM fields exist in data.
  if (assert_IPEDS_fields(data) == FALSE) {
    stop("'data' must contain all '...' parsed race/ethnicity fields. Try
    running the 'parse_race' function first.")
  }

  data <- data %>%
    mutate(RE_Count = (RE_Black + (RE_Asian | RE_AsianURM) + RE_HawaiianPI +
                       RE_Native + RE_White + RE_Hispanic),
           RE_IPEDS =
             case_when(RE_Hispanic   ~ "Hispanic/Latino",
                       RE_Count > 1  ~ "Two or more races",
                       RE_AsianURM   ~ "SE Asian (URM)",
                       RE_Asian      ~ "Asian",
                       RE_Black      ~ "Black / African American",
                       RE_HawaiianPI ~ "Native Hawaiian or Other Pacific Islander",
                       RE_Native     ~ "American Indian or Alaska Native",
                       RE_White      ~ "White",
                       TRUE        ~ "Race/ethnicity unknown"))
  return(data)
}


# Helper Functions --------------------------------------------------------

#' Tests whether the  field is formatted based on the AMCAS / Dept. of Education
#' racial code standard framework.
#'
#' @param .field A character vector
#'
#' @return TRUE or FALSE

test_race_field <- function(.field) {
  require(tidyverse)
  .field %>%
    str_detect("[A-Z;]+$") %>%
    all(na.rm = TRUE)
}


#' Tests whether all the parsed, individual racial data fields are present in
#' the supplied dataframe/tibble
#'
#' @param data A dataframe or tibble
#'
#' @return Boolean: TRUE (if all fields are present), FALSE (if any are missing)
assert_IPEDS_fields <- function(data) {
  all(c("RE_Black", "RE_AsianURM", "RE_Asian", "RE_HawaiianPI", "RE_Native",
        "RE_Hispanic") %in% names(data))
}
