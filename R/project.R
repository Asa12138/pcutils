#' Re-install my packages
#' @param pkgs pkgs
#' @return No return value
reinstall_my_packages <- function(pkgs = c("pcutils", "pctax", "MetaNet", "ReporterScore")) {
    for (i in pkgs) {
        if (i == "pctax") i <- "pctax/pctax"
        if (i == "MetaNet") i <- "MetaNet/MetaNet"
        if (i == "ReporterScore") i <- "GRSA/ReporterScore"
        devtools::document(paste0("~/Documents/R/", i))

        tryCatch(
            {
                system(paste0("R CMD install ~/Documents/R/", i))
            },
            error = function(e) {
                warning(i, "failed, please check.")
            }
        )
    }
}

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

    tmp <- ""
    if (file.exists("~/Documents/R/pcutils")) {
        tmp <- paste0(tmp, 'devtools::load_all("~/Documents/R/pcutils")')
    } else {
        tmp <- paste0(tmp, "library(pcutils)")
    }
    if (file.exists("~/Documents/R/pctax/pctax")) {
        tmp <- paste0(tmp, '\ndevtools::load_all("~/Documents/R/pctax/pctax")')
    } else {
        tmp <- paste0(tmp, "\nlibrary(pctax)")
    }
    tmp <- paste0(tmp, '
Packages <- c("dplyr", "tidyr","ggsci","ggpubr","RColorBrewer","cowplot","readr","tibble","vegan","ggrepel")
lib_ps(Packages)
kin_col=c(k__Bacteria="#a6bce3",k__Fungi="#fdbf6f",k__Metazoa="#fb9a99",k__Viridiplantae="#a9d483",
          k__Archaea="#1f78b4",k__Eukaryota_others="#8dd3c7",k__Viruses="#bda7c9")

add_theme()')
    cat(tmp, file = paste(pro_dir, "analysis", "R_config.R", sep = "/"))

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
add_analysis <- function(analysis_n, title = analysis_n, author = "Asa12138", theme = 1) {
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


#' Make a Gitbook using bookdown
#'
#' @param book_n project name
#' @param root_dir root directory
#' @param bib cite papers bib, from Zotero
#' @param csl cite papers format, default science.csl
#' @param mode "gitbook","bs4"
#' @param author author
#'
#' @export
#' @return No return value
make_gitbook <- function(book_n, root_dir = "~/Documents/R/", mode = c("gitbook", "bs4")[1], author = "Asa12138",
                         bib = "~/Documents/R/pc_blog/content/bib/My Library.bib",
                         csl = "~/Documents/R/pc_blog/content/bib/science.csl") {
    lib_ps("bookdown", library = FALSE)
    mode <- match.arg(mode, c("gitbook", "bs4"))
    if (substr(root_dir, nchar(root_dir), nchar(root_dir)) == "/") {
        book_dir <- paste0(root_dir, book_n)
    } else {
        book_dir <- paste0(root_dir, "/", book_n)
    }

    if (dir.exists(book_dir)) stop("directory exist, try other name")

    if (mode == "gitbook") {
        bookdown::create_gitbook(path = book_dir)
    } else {
        bookdown::create_bs4_book(path = book_dir)
    }

    tmp <- readr::read_file(file.path(root_dir, book_n, "_output.yml"))
    tmp1 <- gsub("    toc:.*BRANCH/%s\n", "", tmp)
    cat(tmp1, file = file.path(root_dir, book_n, "_output.yml"))

    cat("\nlibrary(kableExtra)", file = file.path(root_dir, book_n, "_common.R"), append = TRUE)

    {
        tmp <- readr::read_file(file.path(root_dir, book_n, "index.Rmd"))
        tmp1 <- gsub("A Minimal Book Example", book_n, tmp)
        tmp1 <- gsub("John Doe", author, tmp1)
        if (mode == "gitbook") {
            tmp1 <- gsub("biblio-style: apalike", "link-citations: yes\ncolorlinks: yes\ngraphics: yes", tmp1)
        } else {
            tmp1 <- gsub("link-citations: yes", "link-citations: yes\ncolorlinks: yes\ngraphics: yes", tmp1)
        }
        tmp1 <- gsub("github-repo: rstudio/bookdown-demo\n", "", tmp1)
        if (file.exists(bib)) {
            file.copy(bib, paste(root_dir, book_n, "My_Library.bib", sep = "/"))
            tmp1 <- gsub("book\\.bib", "book.bib, My_Library.bib", tmp1)
        }
        if (file.exists(csl)) {
            file.copy(csl, paste(root_dir, book_n, "my.csl", sep = "/"))
            if (grepl("csl:", tmp1)) {
                tmp1 <- gsub("csl: .*\\.csl", "csl: my.csl", tmp1)
            } else {
                tmp1 <- gsub("graphics: yes", "graphics: yes\ncsl: my.csl", tmp1)
            }
        }
        cat(tmp1, file = file.path(root_dir, book_n, "index.Rmd"))
    }
    message(paste0(
        "Creat the book `", book_n, "` sucessfully! Open project at directory: `",
        book_dir, "`"
    ))
}

make_asa_web <- function() {
    rmarkdown::render_site(encoding = "UTF-8", input = "~/Documents/GitHub/Asa_web/", quiet = TRUE)
    system("rsync -a ~/Documents/GitHub/Asa_web/_site/* ~/Documents/GitHub/Asa12138.github.io/")
}

# =======Some tips========

#' How to use parallel
#'
#' @param loop the main function
#'
#' @export
#' @return No return value
how_to_use_parallel <- function(loop = function(i) {
                                    return(mean(rnorm(100)))
                                }) {
    lib_ps("clipr", library = FALSE)
    res_text <- paste0("#parallel
  reps=100;threads=1;verbose=TRUE
  #main function
  loop=", paste(deparse(loop), collapse = "\n"), '
  {
  if(threads>1){
    pcutils::lib_ps("foreach", "doSNOW", "snow", library = FALSE)
    if(verbose){
      pb <- utils::txtProgressBar(max = reps, style = 3)
      opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
    }
    else opts=NULL
    cl <- snow::makeCluster(threads)
    doSNOW::registerDoSNOW(cl)
    res <- foreach::`%dopar%`(foreach::foreach(i = 1:reps,
                                               .options.snow = opts),
                              loop(i))
    snow::stopCluster(cl)
    gc()
  }
  else {
    res <-lapply(1:reps, loop)
  }}
  #simplify method
  res=do.call(c,res)
')
    clipr::write_clip(res_text)
    message(res_text)
}

#' How to update parameters
#' @export
#' @return No return value
how_to_update_parameters <- function() {
    lib_ps("clipr", library = FALSE)
    res_text <- paste0('point_params = list(size=5,color="red")
ggplot(data.frame(x=1:5,y=5:1), aes(x = x, y = y))+
  do.call(geom_point, update_param(list(size=2,color="blue",alpha=0.5), point_params))')
    clipr::write_clip(res_text)
    message(res_text)
}

#' How to use sbatch
#'
#' @param mode 1~3
#'
#' @export
#' @return No return value
how_to_use_sbatch <- function(mode = 1) {
    if (mode == 1) {
        lib_ps("clipr", library = FALSE)
        res_text <- paste0("#!/bin/bash
#SBATCH --job-name=fastp
#SBATCH --output=/share/home/jianglab/pengchen/work/%x_%A_%a.out
#SBATCH --error=/share/home/jianglab/pengchen/work/%x_%A_%a.err
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2g
#SBATCH --time=14-00:00:00
echo start: `date +'%Y-%m-%d %T'`
start=`date +%s`
####################

do something

####################
echo end: `date +'%Y-%m-%d %T'`
end=`date +%s`
echo TIME:`expr $end - $start`s")
    }
    if (mode == 2) {
        lib_ps("clipr", library = FALSE)
        res_text <- paste0("#!/bin/bash
#SBATCH --job-name=fastp
#SBATCH --output=/share/home/jianglab/pengchen/work/%x_%A_%a.out
#SBATCH --error=/share/home/jianglab/pengchen/work/%x_%A_%a.err
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2g
#SBATCH --time=14-00:00:00
#SBATCH --array=1-39
echo start: `date +'%Y-%m-%d %T'`
start=`date +%s`
echo \"SLURM_ARRAY_TASK_ID: \" $SLURM_ARRAY_TASK_ID
samplelist=~/work/st/samplelist
sample=$(head -n $SLURM_ARRAY_TASK_ID $samplelist | tail -1)
echo handling: $sample
####################

fastp -w 2 -i ~/work/st/data/data/${sample}'_1.clean.fq.gz' -o ~/work/st/data/fastp/${sample}_1.fq \
-I ~/work/st/data/data/${sample}'_2.clean.fq.gz' -O ~/work/st/data/fastp/${sample}_2.fq -j ~/work/st/data/fastp/${sample}.json

####################
echo end: `date +'%Y-%m-%d %T'`
end=`date +%s`
echo TIME:`expr $end - $start`s")
    }
    if (mode == 3) {
        lib_ps("clipr", library = FALSE)
        res_text <- paste0("#!/bin/bash
#SBATCH --job-name=fastp
#SBATCH --output=/share/home/jianglab/pengchen/work/%x_%A_%a.out
#SBATCH --error=/share/home/jianglab/pengchen/work/%x_%A_%a.err
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2g
#SBATCH --time=14-00:00:00
#SBATCH --array=1-39

echo start: `date +'%Y-%m-%d %T'`
start=`date +%s`

echo \"SLURM_ARRAY_TASK_ID: \" $SLURM_ARRAY_TASK_ID
START=$SLURM_ARRAY_TASK_ID
NUMLINES=4 #how many sample in one array
STOP=$((SLURM_ARRAY_TASK_ID*NUMLINES))
START=\"$(($STOP - $(($NUMLINES - 1))))\"

#set the min and max
if [ $START -lt 1 ]
then
  START=1
fi
if [ $STOP -gt 39 ]
then
  STOP=39
fi

echo \"START=$START\"
echo \"STOP=$STOP\"

samplelist=~/work/st/samplelist
#############
for (( N = $START; N <= $STOP; N++ ))
do
  sample=$(head -n \"$N\" $samplelist | tail -n 1)
	echo $sample

  bowtie2 -p 8 -x ~/db/humangenome/hg38 -1 ~/work/st/data/fastp/${sample}_1.fq -2 ~/work/st/data/fastp/${sample}_2.fq \
  -S ~/work/st/data/rm_human/${sample}.sam --un-conc ~/work/st/data/rm_human/${sample}.fq --very-sensitive

done
##############
echo end: `date +'%Y-%m-%d %T'`
end=`date +%s`
echo TIME:`expr $end - $start`s")
    }
    clipr::write_clip(res_text)
    message(res_text)
}

#' How to set options in a package
#'
#' @param package package name
#'
#' @return No return value
#' @export
how_to_set_options <- function(package = "My_package") {
    lib_ps("clipr", library = FALSE)
    res_text <- paste0('# Load the value of the option on package startup
.onAttach <- function(libname, pkgname) {
  if(!dir.exists(tools::R_user_dir("', package, '")))dir.create(tools::R_user_dir("', package, '"),recursive = TRUE)
  refresh_config()
}

# read user config first.
refresh_config <- function() {
  if(file.exists(system.file("config", package = "pcutils")))
    default_options <- readRDS(file = system.file("config", package = "', package, '"))
  else return()
  file_path=file.path(tools::R_user_dir("', package, '"),"config")
  if (file.exists(file_path)) {
    options_to_load <- readRDS(file = file_path)
    if(length(options_to_load)==0)options_to_load=NULL
    options_to_load=pcutils::update_param(default_options,options_to_load)
  }
  else {
    options_to_load=default_options
  }
  # set options
  options("', package, '_config"=options_to_load)
}

#\' Show config
#\'
#\' @return config
#\' @export
show_', package, '_config=function(){
  config=getOption("', package, '_config")
  return(config)
}

#\' Set config
#\'
#\' @param item item
#\' @param value value
#\'
#\' @return No value
#\' @export
#\'
set_', package, '_config <- function(item,value) {
  refresh_config()
  config=getOption("', package, '_config")
  if(is.null(value))config=config[-which(names(config)==item)]
  else config=pcutils::update_param(config,setNames(list(value),item))
  saveRDS(config, file = file.path(tools::R_user_dir("', package, '"),"config"))
  options("', package, '_config"=config)
  message("Set sucessfully!")
}
')
    clipr::write_clip(res_text)
    message(res_text)
}
