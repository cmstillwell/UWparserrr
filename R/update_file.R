update_file <- function(file,
                        dest_path,
                        delete_orig = TRUE) {
  assertthat::assert_that(fs::is_absolute_path(file),
                          fs::is_dir(dest_path))
  assertthat::is.flag() # need to figure out how to check whether the update
  # operation flag is either move or copy

  source_path <- fs::path_file(file)
  dest_path <- file.path(dest_path, source_path)


  if (update_op == "move") {

  }

  }


