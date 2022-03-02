# Parser/Fortification Functions ------------------------------------------

#' Parse text-based racial identity field and fortify the dataset with
#' individual fields for each of the IPEDS racial categories.
#'
#' @param data A data.frame or tibble
#' @param race.field Unquoted name of a field
#' @param hispanic.field Unquoted name of a field
#'
#' @return A data.frame or tibble fortified with new columns
#'
#' @export

parse_race_text <- function(data, race.field, hispanic.field) {
  require(tidyverse)

  race.field <- enquo(race.field)
  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(.Black      = str_detect(!!race.field,
                                    regex("afric|black", TRUE)),
           .Asian      = str_detect(!!race.field, regex("asian", TRUE)),
           .AsianURM   = str_detect(!!race.field,
                                    regex("viet|laot|cambod|hmong",
                                          TRUE)),
           .HawaiianPI = str_detect(!!race.field,
                                    regex("hawaii|pacific", TRUE)),
           .Native     = str_detect(!!race.field,
                                    regex("american indian|tribe", TRUE)),
           .White      = str_detect(!!race.field, regex("white", TRUE)),
           .Hispanic   = str_detect(!!hispanic.field,
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
parse_race_code <- function(data, race.field, hispanic.field) {
  require(tidyverse)

  race.field <- enquo(race.field)
  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(.Black      = str_detect(!!race.field, "(AA|B|BC|BO)(;|$)"),
           .Asian      = str_detect(!!race.field, "((;|^)A|A(C|F|J|K|O|P)|BA)(;|$)"),
           .AsianURM   = str_detect(!!race.field, "(AV|CA|LA)(;|$)"),
           .HawaiianPI = str_detect(!!race.field, "P(H|O|S)(;|$)"),
           .Native     = str_detect(!!race.field, "I(;|$)"),
           .White      = str_detect(!!race.field, "W(;|$)"))
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
    mutate(.Hispanic = !!hispanic.field)
  return(data)
}


#' Parse the AMCAS Hispanic code field
#'
#' @param data A data.frame
#' @param hispanic.field Name of Hispanic field from AMCAS
#'
#' @return A data.frame
#' @export
#'
parse_hispanic_code <- function(data, hispanic.field) {
  require(dplyr)
  require(stringr)

  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(.Hispanic = str_detect(!!hispanic.field, "(^|;)U(;|$)"))
  return(data)
}


#' Parse a text field containing Hispanic/Latino ethnicity information
#'
#' @param data A data.frame
#' @param hispanic.field A field name
#'
#' @return A fortified data.frame
#' @export
#'
parse_hispanic_text <- function(data, hispanic.field) {
  require(dplyr)
  require(stringr)

  hispanic.field <- enquo(hispanic.field)

  data <- data %>%
    mutate(.Hispanic = str_detect(!!hispanic.field,
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
parse_urm <- function(data) {
  require(tidyverse)

  # Confirm that all the individual URM fields exist in data.
  if (assert_IPEDS_fields(data) == FALSE) {
    stop("'data' must contain all '...' parsed fields. Try running the
        'parse_race' function first.")
  }

  data <- data %>%
    mutate(.URM =
             if_else(as.numeric(.Black + .AsianURM + .HawaiianPI + .Native +
                                  .Hispanic) > 0,
                     TRUE,
                     FALSE))
  return(data)
}


#' Creates a single, IPEDS-standardized field for race/ethnicity based on the
#' individual fields created through the 'parse_race' function
#'
#' @param data A dataframe or tibble
#'
#' @return Dataframe or tibble fortified with new column
parse_ipeds <- function(data) {
  require(tidyverse)

  # Confirm that all the individual URM fields exist in data.
  if (assert_IPEDS_fields(data) == FALSE) {
    stop("'data' must contain all '...' parsed race/ethnicity fields. Try
    running the 'parse_race' function first.")
  }

  data <- data %>%
    mutate(.Count = as.numeric(.Black + (.Asian | .AsianURM) + .HawaiianPI +
                                 .Native + .White + .Hispanic),
           .IPEDS =
             case_when(.Hispanic   ~ "Hispanic/Latino",
                       .Count > 1  ~ "Two or more races",
                       .AsianURM   ~ "SE Asian (URM)",
                       .Asian      ~ "Asian",
                       .Black      ~ "Black / African American",
                       .HawaiianPI ~ "Native Hawaiian or Other Pacific Islander",
                       .Native     ~ "American Indian or Alaska Native",
                       .White      ~ "White",
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
  all(c(".Black", ".AsianURM", ".Asian", ".HawaiianPI", ".Native",
        ".Hispanic") %in% names(data))
}
