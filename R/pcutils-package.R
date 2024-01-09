#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom utils data head download.file untar combn
#' @importFrom stats rnorm sd var na.omit setNames median quantile as.formula coef cor dist start end hclust lm model.frame
#' @importFrom grDevices colorRampPalette
#' @import dplyr
#' @import ggplot2
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

# Load the value of the option on package startup
.onAttach <- function(libname, pkgname) {
    if (!dir.exists(tools::R_user_dir("pcutils"))) dir.create(tools::R_user_dir("pcutils"), recursive = TRUE)
    refresh_config()
}

# read user config first.
refresh_config <- function() {
    if (file.exists(system.file("config", package = "pcutils"))) {
        default_options <- readRDS(file = system.file("config", package = "pcutils"))
    } else {
        default_options <- NULL
    }
    file_path <- file.path(tools::R_user_dir("pcutils"), "config")
    if (file.exists(file_path)) {
        options_to_load <- readRDS(file = file_path)
        if (length(options_to_load) == 0) options_to_load <- NULL
        options_to_load <- pcutils::update_param(default_options, options_to_load)
    } else {
        options_to_load <- default_options
    }
    # set options
    options("pcutils_config" = options_to_load)
}

#' Show config
#'
#' @return config
#' @export
show_pcutils_config <- function() {
    refresh_config()
    config <- getOption("pcutils_config")
    return(config)
}

#' Set config
#'
#' @param item item
#' @param value value
#'
#' @return No value
#' @export
#'
set_pcutils_config <- function(item, value) {
    refresh_config()
    config <- getOption("pcutils_config")
    if (is.null(value)) {
        config <- config[-which(names(config) == item)]
    } else {
        config <- pcutils::update_param(config, setNames(list(value), item))
    }
    saveRDS(config, file = file.path(tools::R_user_dir("pcutils"), "config"))
    options("pcutils_config" = config)
    message("Set sucessfully!")
}
