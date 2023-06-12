#' Make a R-analysis project
#'
#' @param pro_n project name
#' @param root_dir root directory
#' @param bib cite papers bib, from Zotero
#' @param csl cite papers format, default science.csl
#'
#' @export
#' @return No return value
make_project <- function(pro_n, root_dir = "~/Documents/R/",
                         bib = "~/Documents/R/pc_blog/content/bib/My Library.bib",
                         csl = "~/Documents/R/pc_blog/content/bib/science.csl") {
  # pro_n='test'
  if (substr(root_dir, nchar(root_dir), nchar(root_dir)) == "/") {
    pro_dir <- paste0(root_dir, pro_n)
  } else {
    pro_dir <- paste0(root_dir, "/", pro_n)
  }

  if (dir.exists(pro_dir)) stop("directory exist, try other name")
  dir.create(pro_dir)
  lapply(c("data", "temp", "result", "summary", "analysis"), FUN = \(x)dir.create(paste(pro_dir, x, sep = "/")))
  lapply(c("R_config.R", paste0(pro_n, ".Rproj")), FUN = \(x)file.create(paste(pro_dir, "analysis", x, sep = "/")))
  if (file.exists(bib)) {
    file.copy(bib, paste(pro_dir, "analysis", "My_Library.bib", sep = "/"))
  }
  if (file.exists(csl)) {
    file.copy(csl, paste(pro_dir, "analysis", "my.csl", sep = "/"))
  }

  cat("Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: XeLaTeX", file = paste(pro_dir, "analysis", paste0(pro_n, ".Rproj"), sep = "/"))

  cat('devtools::load_all("~/Documents/R/pctax/")
devtools::load_all("~/Documents/R/pcutils/")
Packages <- c("dplyr", "reshape2","ggsci","ggpubr","RColorBrewer","cowplot","readr","tibble","vegan","ggrepel")
lib_ps(Packages)
kin_col=c(k__Bacteria="#a6bce3",k__Fungi="#fdbf6f",k__Metazoa="#fb9a99",k__Viridiplantae="#a9d483",
          k__Archaea="#1f78b4",k__Eukaryota_others="#8dd3c7",k__Viruses="#bda7c9")

add_theme()', file = paste(pro_dir, "analysis", "R_config.R", sep = "/"))

  message(paste0("Set `", pro_n, "` sucessfully! Open project at directory: `", pro_dir, "`"))
}

#' Add an analysis for a project
#'
#' @param analysis_n analysis name
#' @param title file title
#' @param author author
#' @param theme 1~10
#'
#' @export
#' @return No return value
add_analysis <- function(analysis_n, title = analysis_n, author = "Peng Chen", theme = 1) {
  pro_n <- list.files(pattern = "*.Rproj")
  pro_n <- gsub(".Rproj", "", pro_n)
  if (length(pro_n) != 1) stop("make sure there is a *.Rproj file in your `getwd()`")
  message(paste0("Add ", analysis_n, " for project ", pro_n))

  # rmdls=list.files(pattern = "*.Rmd")%>%gsub(".Rmd","",.)
  if (file.exists(paste0(analysis_n, ".Rmd"))) stop(paste0(analysis_n, ".Rmd already exists!"))
  file.create(paste0(analysis_n, ".Rmd"))
  if (dir.exists(analysis_n)) stop(paste0(analysis_n, " directory already exists!"))
  dir.create(analysis_n)

  my_bib <- "\nbibliography: My_Library.bib"
  if (!file.exists("My_Library.bib")) my_bib <- ""

  my_csl <- "\ncsl: ./my.csl"
  if (!file.exists("my.csl")) my_csl <- "\nbiblio-style: apa"

  if (theme != 1) {
    lib_ps("rmdformats", "prettydoc", "tufte", library = FALSE)
  }
  output_theme <- c("
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true", "
output:
  rmdformats::html_clean:
    toc: true
    toc_depth: 3", "
output:
  rmdformats::readthedown:
    toc_depth: 3", "
output:
  rmdformats::robobook:
    toc_depth: 3", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: cayman", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: tactile", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: architect", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: leonids", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: hpstr", "
output:
  tufte::tufte_html:
    toc: true
    toc_depth: 3")[theme]

  cat('---
title: "', title, '"
author:
  - ', author, "
date: ", date(), my_bib, "
link-citations: yes", my_csl, output_theme, '
editor_options:
  markdown:
    wrap: 150
---

```{r setup, include = FALSE}
path <- "', getwd(), '"
knitr::opts_chunk$set(eval=TRUE, #run code in chunks (default = TRUE)
                       highlight = TRUE, #highlight display
                       echo = TRUE, #whether to include the source code in the output
                       tidy=TRUE, #whether to organize the code
                       error = TRUE, #Whether to include error information in the output
                       warning = FALSE, #Whether to include warnings in the output (default = TRUE)
                       message = FALSE, #whether to include reference information in the output
                       cache=TRUE, #whether to cache
                       collapse = FALSE # output in one piece
                       )
knitr::opts_knit$set(root.dir = path)
```
Install and import all dependent packages, data import:
```{r import,echo=TRUE, message=FALSE, warning=FALSE, results="hide"}
source("./R_config.R")
output="', analysis_n, '/"
```
# Header1
```{r test,echo=TRUE,fig.cap="Test fig"}
a=data.frame(type=letters[1:6],num=c(1,3,3,4,5,10))
gghuan(a)+scale_fill_manual(values=get_cols(6,"col3"))
ggsave("', paste0(analysis_n, "/test.pdf"), '")
```

', file = paste0(analysis_n, ".Rmd"), sep = "")
}
