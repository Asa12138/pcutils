% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{hebing}
\alias{hebing}
\title{Group your data}
\usage{
hebing(otutab, group, margin = 2, act = "mean")
}
\arguments{
\item{otutab}{data.frame}

\item{group}{group vector}

\item{margin}{1 for row and 2 for column(default: 2)}

\item{act}{do (default: mean)}
}
\value{
data.frame
}
\description{
Group your data
}
\examples{
data(otutab)
hebing(otutab, metadata$Group)
}