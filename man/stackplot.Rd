% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{stackplot}
\alias{stackplot}
\alias{areaplot}
\title{Plot a stack plot}
\usage{
stackplot(
  otutab,
  metadata = NULL,
  group = "Group",
  get_data = FALSE,
  bar_params = list(width = 0.7, position = "stack"),
  topN = 8,
  others = TRUE,
  relative = TRUE,
  legend_title = "",
  stack_order = TRUE,
  group_order = FALSE,
  facet_order = FALSE,
  style = c("group", "sample")[1],
  flow = FALSE,
  flow_params = list(lode.guidance = "frontback", color = "darkgray"),
  number = FALSE,
  repel = FALSE,
  format_params = list(digits = 2),
  text_params = list(position = position_stack())
)

areaplot(
  otutab,
  metadata = NULL,
  group = "Group",
  get_data = FALSE,
  bar_params = list(position = "stack"),
  topN = 8,
  others = TRUE,
  relative = TRUE,
  legend_title = "",
  stack_order = TRUE,
  group_order = FALSE,
  facet_order = FALSE,
  style = c("group", "sample")[1],
  number = FALSE,
  format_params = list(digits = 2),
  text_params = list(position = position_stack())
)
}
\arguments{
\item{otutab}{otutab}

\item{metadata}{metadata}

\item{group}{one group name of columns of metadata}

\item{get_data}{just get the formatted data?}

\item{bar_params}{parameters parse to \code{\link[ggplot2]{geom_bar}}}

\item{topN}{plot how many top species}

\item{others}{should plot others?}

\item{relative}{transfer to relative or absolute}

\item{legend_title}{fill legend_title}

\item{stack_order}{the order of stack fill}

\item{group_order}{the order of x group, can be T/F, or a vector of x, or a name, or "cluster"}

\item{facet_order}{the order of the facet}

\item{style}{"group" or "sample"}

\item{flow}{should plot a flow plot?}

\item{flow_params}{parameters parse to \code{\link[ggalluvial]{geom_flow}}}

\item{number}{show the number?}

\item{repel}{use the ggrepel::geom_text_repel instead of geom_text}

\item{format_params}{parameters parse to \code{\link[base]{format}}}

\item{text_params}{parameters parse to \code{\link[ggplot2]{geom_text}}}
}
\value{
a ggplot

a ggplot
}
\description{
Plot a stack plot

Plot a area plot
}
\examples{
data(otutab)
stackplot(otutab, metadata, group = "Group")
\donttest{
if (interactive()) {
  stackplot(otutab, metadata,
    group = "Group", style = "sample",
    group_order = TRUE, flow = TRUE, relative = FALSE
  )
}
}
data(otutab)
areaplot(otutab, metadata, group = "Id")
\donttest{
areaplot(otutab, metadata,
  group = "Group", style = "sample",
  group_order = TRUE, relative = FALSE
)
}
}
