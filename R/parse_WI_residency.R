#' Appends a single Wisconsin residency determination field to the dataset
#'
#' @param df A dataframe
#' @param res_registar RO residency determination, field in dataframe
#' @param res_amcas AMCAS residency response, field inside dataframe
#' @param res_state AMCAS state of residence, field inside dataframe
#' @return A dataframe with the Wisconsin residency field appended
#'
#' @export
#'
parse_WI_residency <- function(df,
                               res_registar = residency_registrar,
                               res_amcas    = residency_response,
                               res_state    = residency_state_descr) {

  require(tidyverse)
  res_registar <- enquo(res_registar)
  res_amcas <- enquo(res_amcas)
  res_state <- enquo(res_state)

  t_WI_fortified <- df %>%
    mutate(WI_residency =
             case_when(grepl("^r",  !!res_registar, TRUE) ~ "Resident",
                       !is.na(!!res_registar)             ~ "Nonresident",
                       grepl("^r",  !!res_amcas, TRUE)    ~ "Resident",
                       grepl("^n",  !!res_amcas, TRUE)    ~ "Nonresident",
                       grepl("^wi", !!res_state, TRUE)    ~ "Resident",
                       TRUE                               ~ "Nonresident"))
  return(t_WI_fortified)
}
