#' Sort selected files in a directory based on their date attribute
#'
#' @param folder_path A list or character vector of directories to search
#' @param file_pattern Regular expression selecting file list to be sorted
#' @param n_return How many files should be returned?
#' @param date_sort Which file date property should be used for sorting?
#' @param skip_temp Should temporary files be skipped? (Almost always "yes")
#' @param paths_only Return the full, sorted file paths?
#'
#' @return A list of fully expanded file paths
#' @export
#'

pull_files_by_date <- function(folder_path  = getwd(),
                               file_pattern = NULL,
                               n_return     = NULL,
                               date_sort    = "mtime",
                               skip_temp    = TRUE,
                               paths_only   = TRUE){
  require(tidyverse)

  # Error handling
  # Date sort argument correct
  if (!date_sort %>% str_detect('^(m|a|c)time$')) {
    stop('Argument `date_sort` must be one of "mtime", "atime", or "ctime".')
  }
  # File name pattern is a length-1 string
  if (!is.character(file_pattern) | length(file_pattern) != 1) {
    stop('Argument `file_pattern` must be a string vector of length = 1.')
  }

  # Get file names
  file_names <- folder_path %>%
    list.files(pattern = file_pattern, full.names = TRUE)

  # Strip out meta/temporary files created when a file is open elsewhere
  if (skip_temp == TRUE) {
    keep <- str_detect(file_names, "~\\$", negate = TRUE)
    file_names <- subset(file_names, keep)
  }

  # Warning - File list length = 0
  if (length(file_names) == 0) {
    warning(paste0('Function `list.files` returned no files matching the pattern. ',
                   'Check both the file search (regex) string: "', file_pattern,
                   '" and the searched folder: "', folder_path, '".'))
    return(file_names)
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
  if (paths_only == TRUE) {
    sorted_names <- sorted_names %>%
      pull(name) %>%
      path()
  }

  warning(paste0('This function has been depreciated and is no longer in active ',
                 'development. Workflows using "select_files_by_date" are ',
                 'preferred since they separte declaration of the path(s)',
                 'to be searched from the sorting and return steps.'))

  return(sorted_names)

}
