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
                             overwrite = FALSE) {
  require(fs)

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

  return(output_msg)

}
