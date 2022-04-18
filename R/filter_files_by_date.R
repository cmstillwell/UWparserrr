#' Filter file list based on their date attributes
#'
#' @param file_names A character vector of fully expanded file paths (not a list)
#' @param n_return The number of files to be returned or "all" of them
#' @param date_filter Which file date property should be used for filtering?
#' @param return_what What should the output format be, a vector of "paths" or a "table" of files and their attributes?
#'
#' @return A list of fully expanded file paths or a table of filtered file names with their attributes
#' @export
#'
filter_files_by_date <- function(file_names,
                               n_return     = "all",
                               date_filter    = "mtime",
                               return_what  = "paths"){

  require(tidyverse)

  # Error handling
  # Date filter argument correct
  if (!date_filter %>% str_detect('^(m|a|c)time$')) {
    stop('Argument `date_filter` must be one of "mtime", "atime", or "ctime".')
  }

  # Return must be correctly specified
  if (or(return_what == "paths", return_what == "table")) {
    stop('Argument `return_what` must be either "paths" or "table".')
  }

  # Filter file names by date passed from function call
  filtered_names <- file_names %>%
    map_df(file.info, extra_cols = FALSE) %>%
    select(contains("time")) %>%
    rownames_to_column("name") %>%
    arrange(
      desc(
        case_when(
          date_filter == "mtime" ~ mtime,
          date_filter == "atime" ~ atime,
          date_filter == "ctime" ~ ctime)
      )
    )

  # return x number of files
  if (is.numeric(n_return)) {
    filtered_names <- filtered_names %>%
      slice(1:n_return)
  }

  # removes file info to return only names
  if (return_what == "paths") {
    filtered_names <- filtered_names %>%
      pull(name)
  }

  return(filtered_names)

}
