% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{rm_low}
\alias{rm_low}
\title{Remove the low relative items in each column}
\usage{
rm_low(otutab, relative_threshold = 1e-04)
}
\arguments{
\item{otutab}{otutab}

\item{relative_threshold}{threshold, default: 1e-4}
}
\value{
data.frame
}
\description{
Remove the low relative items in each column
}
\examples{
data(otutab)
rm_low(otutab)
}
