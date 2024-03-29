% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_AMCAS_race_ethn.R
\name{parse_race_text}
\alias{parse_race_text}
\title{Parse text-based racial identity field and fortify the dataset with
individual fields for each of the IPEDS racial categories.}
\usage{
parse_race_text(data, race.field, hispanic.field)
}
\arguments{
\item{data}{A data.frame or tibble}

\item{race.field}{Unquoted name of a field}

\item{hispanic.field}{Unquoted name of a field}
}
\value{
A data.frame or tibble fortified with new columns
}
\description{
Parse text-based racial identity field and fortify the dataset with
individual fields for each of the IPEDS racial categories.
}
