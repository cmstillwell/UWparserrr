#' Keep only the most recent file in a directory that matches a pattern
#'
#' @param path Path/directory to be searched for matching files
#' @param file_pattern Regular expression to find matching files
#' @param return_dir True/False: Should the function return the working directory
#'
#' @return Defaults to the working directory which helps with piped operations
#' @export
#'
delete_old_files <- function(dir, file_pattern, return_dir = TRUE) {

  # Require the 'fs' package
  require(fs)

  # Get file list matching the given regular expression
  files <- dir_ls(dir, type = "file", regexp = file_pattern)

  # Error handling
    ## Ensure that 'dir' is a directory
      assertthat::assert_that(is_dir(dir))

    ## Ensure there are multiple files matching the regular expression
    ## Warn and continue (useful for piped expressions)
    if (length(files) < 2) {
      if (return_dir == TRUE) {
        warning(stringr::str_glue('Warning: Fewer than two files matching the pattern "{file_pattern}" were selected.',
                                  'No old file(s) to delete.',
                                  'Execution continuing.', .sep = " "))
        return(dir)
        exit()
      }
      # Stop out with error
      if (return_dir == FALSE) {
        stop(stringr::str_glue('Error: Fewer than two files matching the pattern "{file_pattern}" were selected.',
                               'No old file(s) to delete.',
                               'Function has nothing to return.',
                               'Execution halted.', .sep = " "))
      }
    }

  # Main Code: Find and delete old versions of a file
    ## Get file info
      files_info <- file_info(files)

    ## Arrange file list by creation date
      files_info_sorted <- files_info[order(files_info$modification_time),]

    ## Create the list of files to delete
      files_to_delete <- files_info_sorted[-nrow(files_info_sorted), "path"]

    ## Delete the old files
      as.character(files_to_delete$path) |>
        file_delete()

  # Return the working directory (useful for piped operations)
    if (return_dir == TRUE) {
      return(dir)
    }
}
