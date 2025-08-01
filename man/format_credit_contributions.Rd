% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/l_tools.R
\name{format_credit_contributions}
\alias{format_credit_contributions}
\title{Format CRediT Contributions with Optional Author Name Shortening}
\usage{
format_credit_contributions(df, short_name = FALSE)
}
\arguments{
\item{df}{A data frame with columns \code{Author} and \code{Contributions}}

\item{short_name}{Logical. If TRUE, format author names as initials (e.g., C.P.)}
}
\value{
A character string summarizing contributions in the format:
"Conceptualization, A, B; Methodology, C, D; ..."
}
\description{
Format CRediT Contributions with Optional Author Name Shortening
}
\examples{
df <- data.frame(
  Author = c("Chen Peng", "Xin Wei"),
  Contributions = c("Methodology,Visualization", "Methodology")
)
format_credit_contributions(df, short_name = TRUE)
}
