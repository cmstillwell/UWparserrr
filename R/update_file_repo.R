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
                             clear_dest_dir = FALSE,
                             action = "copy",
                             return = "dir",
                             overwrite = FALSE) {
  require(fs)
  require(stringr)

    # Ensure a valid return type is indicated
  assertthat::assert_that(return == "dir" | return == "logical")

  # Manage the destination directory
  if (clear_dest_dir == FALSE) {
    n.dest.file <-
      dir_ls(destination, type = "file") |>
      length()

    chr.clear.msg <-
      str_glue("{n.dest.file} file(s) are already in the destination folder:",
               "'{destination}'. They have not been altered.")
  }

  if (clear_dest_dir) {
    dir_ls(destination, type = "file") |>
    file_delete()

    chr.clear.msg <-
      str_glue("All files in '{destination}' have been deleted.")
  }


  if (is.character(clear_dest_dir)) {
    dir_ls(destination, regexp = clear_dest_dir) |>
    file_delete()

    chr.clear.msg <-
      str_glue("All files in '{destination}' matching the regular expression ",
               "'{clear_dest_dir}' have been deleted.")
    }

  # process only if the vector of file names has length > 0
  if (length(files) > 0) {

    dest_path <- path(destination, path_file(files))

    file_copy(files, dest_path, overwrite = overwrite)

    if (action == "move") {
      file_delete(files)
    }

    output_msg <-
      str_glue(chr.clear,
               "{action} operation completed",
               "{length(dest_path)} new file(s) processed.",
               .sep = "; ")
    message(output_msg)

    # Set return value
    return(ifelse(return == "dir", destination, TRUE))
  }

  # return error for when file path argument length equals zero.
  if (length(files) == 0) {
    warning(str_glue("No files to {action} because the 'files' argument length is 0."))

    return(ifelse(return == "dir", destination, FALSE))
  }

  }


