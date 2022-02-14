parse_income <- function(df,
                         income = childhood_family_income_level,
                         test.switch = FALSE) {

  # STOP CALL - FUNCTION is not ready!!!
  stopifnot(test.switch)

  require(tidyverse)

  income = enquo(income)

  tbl_income <- df %>%
    select(!!income) %>%
    distinct() %>%
    mutate(income_sort = case_when(
      str_detect(!!income, "-")   ~ str_extract(!!income, "\\d{2,3},\\d{3}$"),
      str_detect(!!income, "\\$") ~ str_extract(!!income, "[\\d,]{6,7}"),
      TRUE                        ~ "Unknown")) %>%
    mutate(income_sort     = as.numeric(str_replace_all(income_sort,",", "")),
           income_quintile = case_when(income_sort < 1)) %>%
    arrange(income_sort)


  return(tbl_income)
}


# get a vector to test
fctr <- factor(tbl_income$`Childhood Family Income Level`)

fct_num_range <- function(.factor, .range_sep = " - ") {
  require(stringr)
  require(readr)

  # convert to a factor
  if (is.factor(.factor) == FALSE) {
    .factor <- factor(.factor)
  }

  # save factor levels
  lvl <- levels(.factor)

  # isolate any levels that don't contain any numbers
  notnum <- str_subset(lvl, "\\d", negate = TRUE)

  # assemble the regex search string for upper boundary
  rgx_upper <- paste("(?<=", test, ")[\\d,]+", sep = "")

  # make a table factor levels and their range boundaries
  tbl <- lvl %>%
    as_tibble() %>%
    mutate(lower = parse_number(lvl, na = notnum,),
           upper = parse_number(str_extract(lvl, rgx_upper), na = notnum)) %>%
    arrange(lower, desc(upper))
  #need to deal with the NA sort issue
  #https://stackoverflow.com/questions/25258817/how-to-have-nas-displayed-first-using-arrange
  return(tbl)

}



