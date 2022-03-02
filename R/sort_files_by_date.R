sort_files_by_date <- function(folder_path  = getwd(),
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

  # Error handling - Ensure file list length > 0
  if (length(file_names) == 0) {
    stop('Function `list.files` returned nothing matching the specified pattern. Check your file pattern or directory.')
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

  return(sorted_names)

}
