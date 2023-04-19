#' Keep only the most recent file in a directory that matches a pattern
#'
#' @param path Path/directory to be searched for matching files
#' @param file_pattern Regular expression to find matching files
#' @param return_dir True/False: Should the function return the working directory
#'
#' @return Defaults to the working directory which helps with piped operations
#' @export
#'
delete_old_files <- function(path, file_pattern, return_dir = TRUE) {

  # Require the 'fs' package
  require(fs)

  # Get file list matching the given regular expression
  files <- dir_ls(directory, type = "file", glob = file_pattern)

  # Ensure there are multiple files matchign the regular expression
  assertthat::assert_that(length(files) > 1,
                          "There must be multiple files matching the pattern in the directory.")

  # Get file info
  files_info <- file_info(files)

  # Arrange file list by creation date
  files_info_sorted <- files_info[order(files_info$mtime),]

  # Create the list of files to delete
  files_to_delete <- files_info_sorted[-nrow(files_info_sorted), "path"]

  # Delete the old files
  file_delete(files_to_delete)

  # Return the working directory (useful for piped operations)
  if (return_dir == TRUE) {
    return(directory)
  }
}
