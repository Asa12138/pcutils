% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/additional.R
\name{cor_plot}
\alias{cor_plot}
\title{Plot correlation}
\usage{
cor_plot(
  env,
  env2 = NULL,
  geom = ggcor::geom_square,
  mode = 1,
  method = "pearson",
  mode_param = NULL,
  colors = get_cols(pal = "bluered")[2:10]
)
}
\arguments{
\item{env}{dataframe1}

\item{env2}{dataframe2 (default:NULL)}

\item{geom}{geom, default: \code{\link[ggcor]{geom_square}}}

\item{mode}{plot mode (1~4)}

\item{method}{one of "pearson","kendall","spearman"}

\item{mode_param}{parameters parse to \code{geom} (mode=1~2) or \code{\link[corrplot]{corrplot}} (mode=3)}

\item{colors}{color, default is \code{get_cols(pal="bluered")[2:10]}}
}
\value{
ggplot
}
\description{
Plot correlation
}
\examples{
\donttest{
if (interactive()) {
  data(otutab)
  cor_plot(metadata[, 3:10])
  cor_plot(metadata[, 3:10], mode = 2)
  cor_plot(metadata[, 3:10], mode = 3)
  cor_plot(t(otutab)[, 1:50], mode = 4)
}
}
}
