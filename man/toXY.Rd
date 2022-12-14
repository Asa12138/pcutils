% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{toXY}
\alias{toXY}
\title{transfer Geographical latitude and longitude to XY(m)}
\usage{
toXY(dat)
}
\arguments{
\item{dat}{a two-columns dataframe, first is latitude, second is longitude}
}
\description{
transfer Geographical latitude and longitude to XY(m)
}
\examples{
data.frame(row.names = letter[1:18],x=runif(18,30,35),y=runif(18,40,45))->geo
toXY(geo)
}
