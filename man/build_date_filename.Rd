% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_filename.R
\name{build_date_filename}
\alias{build_date_filename}
\title{Build a file name with a date/time}
\usage{
build_date_filename(
  file.name,
  date.time = Sys.time(),
  round.time.to = "day",
  time.format = "\%Y-\%m-\%d \%H:\%M:\%S",
  date.position = "prefix",
  file.extension = ".xlsx"
)
}
\arguments{
\item{file.name}{A string giving the text portion of the file name}

\item{date.time}{A date/time defaults to the current time}

\item{round.time.to}{A string giving the date precision level}

\item{time.format}{A string giving the format of the date/time}

\item{date.position}{A string giving the position of the date within the
file name, either as a prefix or a suffix}

\item{file.extension}{A string giving the file name extension. Specification
of the extension works nicely with the 'export' function in the 'rio' package.}
}
\value{
A string used as a file name including the given date/time, stem, and
extension
}
\description{
Build a file name with a date/time
}
