#' Fortify applicant data with Y/N fields for major application steps
#'
#' @param df.omnibus A data.frame of the AMP "Omnibus" report
#'
#' @return The input data.frame with several additional columns for each of the
#' application steps: complete, interviewed, accepted, matriculated
#'
#' @export

fortify_app_steps <- function(df.omnibus) {

  require(tidyverse)

  # Check whether the function has been passed a data frame
  stopifnot(is.data.frame(df.omnibus))

  # fortify the data frame with additional columns for application process steps
  df.omnibus <- df.omnibus %>%
  mutate(admit_date      = coalesce(admit_MD_date,
                                    admit_MSTP_date,
                                    admit_WARM_date),
         YN_Complete_App = !is.na(app_cmplt_date),
         YN_Interviewed  = !is.na(msac_schd_date),
         YN_Offered      = !is.na(admit_date),
         YN_Matriculate  = !is.na(matriculate_date)
         )

}
