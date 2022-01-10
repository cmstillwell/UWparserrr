parse_income <- function(df,
                         income = childhood_family_income_level) {
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
           income_quintile = case_when(income_sort < )) %>%
    arrange(income_sort)


  return(tbl_income)
}
