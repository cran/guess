% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nona.R
\name{nona}
\alias{nona}
\title{No NAs}
\usage{
nona(vec = NULL)
}
\arguments{
\item{vec}{Required. Character or Numeric vector.}
}
\value{
Character vector.
}
\description{
Converts NAs to 0s
}
\examples{
x <- c(NA, 1, 0); nona(x)
x <- c(NA, "dk", 0); nona(x)
}

