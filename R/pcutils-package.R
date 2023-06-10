#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom utils data
#' @importFrom utils head
#' @import dplyr
#' @import ggplot2
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
