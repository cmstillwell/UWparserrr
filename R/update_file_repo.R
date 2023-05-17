#' Update archive/repository files with most recent versions
#'
#' @param files A vector of (full) paths to the files that will be copied/moved
#' @param destination An atomic vector giving the destination directory
#' @param action File operation (copy or move) to be taken
#' @param overwrite Overwrite files with the same name(s) in the destination directory?
#'
#' @return Nothing. Message displayed upon success
#' @export

update_file_repo <- function(files,
                             destination = getwd(),
                             action = "copy",
                             return = "dir",
                             overwrite = FALSE) {
  require(fs)

  # Ensure a valid return type is indicated
  assertthat::assert_that(return == "dir" | return == "logical")

  # process only if the vector of file names has length > 0
  if (length(files) > 0) {

    dest_path <- path(destination, path_file(files))

    file_copy(files, dest_path, overwrite = overwrite)

    if (action == "move") {
      file_delete(files)
    }

    output_msg = paste0(
      action,
      " operation completed. ",
      length(dest_path),
      " file(s) processed."
      )

    message(output_msg)

    # Set return value
    return(ifelse(return == "dir", destination, TRUE))
  }

  # return error for when file path argument length equals zero.
  if (length(files) == 0) {
    warning("No files processed because argument was length 0.")

    return(ifelse(return == "dir", destination, FALSE))
  }

  }


