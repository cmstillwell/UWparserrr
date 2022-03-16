#' Build a file name with a date/time
#'
#' @param file.name A string giving the text portion of the file name
#' @param date.time A date/time defaults to the current time
#' @param round.time.to A string giving the date precision level
#' @param time.format A string giving the format of the date/time
#' @param date.position A string giving the position of the date within the
#' file name, either as a prefix or a suffix
#' @param file.extension A string giving the file name extension. Specification
#' of the extension works nicely with the 'export' function in the 'rio' package.
#'
#' @return A string used as a file name including the given date/time, stem, and
#' extension
#'
#' @export
#'
build_date_filename <- function(file.name,
                                date.time      = Sys.time(),
                                round.time.to  = "day",
                                time.format    = "%Y-%m-%d %H:%M:%S",
                                date.position  = "prefix",
                                file.extension = ".xlsx") {

  # Error checks
  assertthat::is.string(file.name)
  assertthat::is.string(file.extension)
  assertthat::assert_that(date.position == "prefix" | date.position == "suffix")
  assertthat::assert_that(grepl(pattern = "[ymdhms\\% \\:-]*",
                                x = file.name,
                                ignore.case = TRUE,
                                perl = TRUE))

  # Format and round
  date.time <- date.time |>
    as.character() |>
    strptime(format = time.format) |>
    trunc(units = round.time.to) |>
    as.character()

  # Trim colons from filename (because they're not allowed)
  date.time <- sub("\\:", "", date.time)

  # Trim zeroed seconds
  zero.end <- "0000$"
  zero.seconds <- "00$"
  ifelse(grepl(zero.end, file.name),
         yes = sub(zero.seconds, "", file.name),
         no  = file.name
         )

  # Build filename with date at the beginning
  if (date.position == "prefix") {
    file.name <- paste0(date.time, ", ", file.name, file.extension)
  }

  # Build filename with date at the end
  if (date.position == "suffix") {
    file.name <- paste0(file.name, ", ", date.time, file.extension)
    }

  return(file.name)
}
