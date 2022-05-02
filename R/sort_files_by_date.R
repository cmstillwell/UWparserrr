#' Sort file list based on their date attributes
#'
#' @param file_names A character vector of fully expanded file paths (not a list)
#' @param n_return The number of files to be returned or "all" of them
#' @param date_sort Which file date property should be used for sorting?
#' @param return_what What should the output format be, a vector of "paths" or a "table" of files and their attributes?
#'
#' @return A list of fully expanded file paths or a table of sorts files names with their attributes
#' @export
#'
sort_files_by_date <- function(file_names,
                               n_return     = "all",
                               date_sort    = "mtime",
                               return_what  = "paths"){

  require(tidyverse)

  # Depreciation warning
  warning(paste0('This function has been depreciated and is no longer in active ',
                 'development. Use "filter_files_by_date" instead as it keeps ',
                 'consistent naming conventions with the rest the "tidyverse."'))

  # Error handling
  # Date sort argument correct
  if (!date_sort %>% str_detect('^(m|a|c)time$')) {
    stop('Argument `date_sort` must be one of "mtime", "atime", or "ctime".')
  }

  # Sort file names by date passed from function call
  sorted_names <- file_names %>%
    map_df(file.info, extra_cols = FALSE) %>%
    select(contains("time")) %>%
    rownames_to_column("name") %>%
    arrange(
      desc(
        case_when(
          date_sort == "mtime" ~ mtime,
          date_sort == "atime" ~ atime,
          date_sort == "ctime" ~ ctime)
      )
    )

  # return x number of files
  if (is.numeric(n_return)) {
    sorted_names <- sorted_names %>%
      slice(1:n_return)
  }

  # removes file info to return only names
  if (return_what == "paths") {
    sorted_names <- sorted_names %>%
      pull(name)
  }

  return(sorted_names)

}
