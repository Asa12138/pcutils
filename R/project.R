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

    writeLines("Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: XeLaTeX", con = paste(pro_dir, "analysis", paste0(pro_n, ".Rproj"), sep = "/"), sep = "")

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
    writeLines(tmp, con = paste(pro_dir, "analysis", "R_config.R", sep = "/"), sep = "")

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

    writeLines(paste0('---
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

'), con = paste0(analysis_n, ".Rmd"), sep = "")
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
    writeLines(tmp1, con = file.path(root_dir, book_n, "_output.yml"), sep = "")

    cat(paste0("\nlibrary(kableExtra)"), file = file.path(root_dir, book_n, "_common.R"), append = TRUE)

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
    # 改成了quarto构建网站
    system("quarto render ~/Documents/GitHub/Asa_web/index.qmd --quiet")
    # rmarkdown::render_site(encoding = "UTF-8", input = "~/Documents/GitHub/Asa_web/", quiet = TRUE)
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
    res <- foreach::`%dopar%`(foreach::foreach(i = seq_len(reps),.options.snow = opts),
                              loop(i))
    snow::stopCluster(cl)
    gc()
  }
  else {
    res <-lapply(seq_len(reps), loop)
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

#' How to set font for ggplot
#'
#' @return No return value
#' @export
how_to_set_font_for_plot <- function() {
    lib_ps("clipr", library = FALSE)
    res_text <- paste0('
    lib_ps("sysfonts", "showtext", library = FALSE)

    font_file <- "/System/Library/Fonts/Supplemental/Songti.ttc"

    showtext::showtext_auto()

    # Add the font to showtext
    sysfonts::font_add("Songti", font_file)

    #Check the system font. If Songti appears, it indicates successful import
    sysfonts::font_families()

    ggplot(mtcars)+
      geom_point(aes(x=mpg,y=wt))+
      ggtitle("\u6211\u662f\u4e00\u4e2a\u4e2d\u6587\u6807\u9898")+
      theme(text = element_text(family = "Songti"))')

    clipr::write_clip(res_text)
    message(res_text)
}


# =======Packages========

parse_NEWS_md <- function(file_path = "NEWS.md") {
    # Read the content of the NEW.md file
    content <- readLines(file_path)

    # Initialize a list to store version-wise changes
    version_changes <- list()

    # Initialize variables to track the current version and section
    current_version <- NULL
    current_section <- NULL

    # Process each line in the file
    for (line in content) {
        # Check if the line indicates a version
        if (grepl("^#\\s+.*Notes$", line)) {
            # Extract the version number
            current_version <- gsub("^#.* v", "", gsub(" Notes$", "", line))
            # Create a new entry for the current version
            version_changes[[current_version]] <- list()
        } else if (grepl("^##", line)) {
            # Extract the section name
            section_name <- gsub("^##\\s*", "", line)
            # Set the current section variable
            current_section <- section_name
            # Initialize the section content for the current version
            version_changes[[current_version]][[current_section]] <- character()
        } else if (!is.null(current_version) && !is.null(current_section) && line != "") {
            # Add the line to the corresponding section
            version_changes[[current_version]][[current_section]] <-
                c(version_changes[[current_version]][[current_section]], line)
        }
    }

    # Remove leading and trailing whitespaces
    version_changes <- lapply(version_changes, function(version) {
        lapply(version, function(section) trimws(section))
    })

    # Return the parsed version-wise changes
    return(version_changes)
}

#' Update the NEW.md for a package
#'
#' @param package_dir default: "."
#' @param new_features new_features
#' @param bug_fixes bug_fixes
#' @param other_changes other_changes
#' @param ... additional info
#'
#' @return No value
#' @export
#'
update_NEWS_md <- function(package_dir = ".", new_features = character(),
                           bug_fixes = character(), other_changes = character(), ...) {
    pkg_info <- get_package_info(package_dir)
    pkg_name <- pkg_info$Package
    new_version <- pkg_info$Version

    file_path <- file.path(package_dir, "NEWS.md")
    # Parse existing NEW.md content
    if (file.exists(file_path)) {
        existing_versions <- parse_NEWS_md(file_path)
    } else {
        existing_versions <- NULL
    }

    # If there are any new changes, insert them into the existing version
    changes_to_insert <- c(list(
        "Added" = new_features,
        "Fixed" = bug_fixes,
        "Others" = other_changes
    ), list(...))
    for (i in names(changes_to_insert)) {
        if (length(changes_to_insert[[i]]) > 0) {
            changes_to_insert[[i]] <- paste("-", changes_to_insert[[i]],
                format(Sys.Date(), "<%Y-%m-%d, %a>"),
                sep = " "
            )
        }
    }

    # Check if the new version already exists
    if (new_version %in% names(existing_versions)) {
        # Insert new features, bug fixes, and other changes
        for (change_type in names(changes_to_insert)) {
            if (length(changes_to_insert[[change_type]]) > 0) {
                existing_versions[[new_version]][[change_type]] <-
                    c(changes_to_insert[[change_type]], existing_versions[[new_version]][[change_type]])
            }
        }
    } else {
        # If the version is new, create a new entry
        new_version_content <- changes_to_insert

        # Insert the new version into the existing versions
        existing_versions <- c(setNames(list(new_version_content), new_version), existing_versions)
    }

    # Write out the updated content to NEW.md
    write_NEWS_md(pkg_name, existing_versions, file_path)
}

# Function to write out the updated content to NEW.md
write_NEWS_md <- function(pkg_name, parsed_versions, file_path = "NEWS.md") {
    # Open the file for writing
    file <- file(file_path, "w")

    # Write each version and its content to the file
    for (version in names(parsed_versions)) {
        cat(paste0("# ", pkg_name, " v", version, " Notes\n\n"), file = file)

        # Write each section (New Features, Bug Fixes, Other Changes)
        for (section_name in names(parsed_versions[[version]])) {
            if (length(parsed_versions[[version]][[section_name]]) > 0) {
                cat(paste0("## ", section_name, "\n\n"), file = file)
                cat(paste(parsed_versions[[version]][[section_name]], collapse = "\n"), file = file)
                cat(paste0("\n\n"), file = file)
            }
        }
    }

    # Close the file
    close(file)
}

get_package_info <- function(package_dir = ".") {
    pkg <- basename(normalizePath(package_dir))
    utils::packageDescription(pkg, lib.loc = file.path(package_dir, ".."))
}

solve_no_visible_binding <- function(file = "~/Downloads/00check.log.txt") {
    # 读入文件
    log_file <- readLines("~/Downloads/00check.log.txt")

    # 挑出带有 'no visible binding for global variable' 的行
    error_lines <- grep("no visible binding for global variable", log_file, value = TRUE)

    # 提取错误信息
    error_messages <- gsub("(.*): no visible binding for global variable '([^']+).*", "\\1: \\2", error_lines)
    if (length(error_messages) < 1) {
        return(invisible())
    }

    # 初始化输出变量
    output <- ""

    # 初始化临时变量
    tmp <- ""
    tmp2 <- ""

    # 循环处理错误信息
    for (msg in error_messages) {
        x <- strsplit(msg, ": ")[[1]]

        if (tmp == x[1]) {
            tmp2 <- paste0(tmp2, "=", x[2])
        } else {
            if (tmp != "") {
                output <- paste(output, paste0(tmp, ":", tmp2, "=NULL\n"))
            }
            tmp <- x[1]
            tmp2 <- x[2]
        }
    }

    # 处理最后一个
    output <- paste(output, paste0(tmp, ":", tmp2, "=NULL"))

    # 输出结果
    cat(output, sep = "\n")
}


check_Rds <- function(package_folder_path = ".") {
    man_folder_path <- file.path(package_folder_path, "man")
    # 获取man文件夹下的所有文件
    man_files <- list.files(man_folder_path, pattern = "\\.Rd$", full.names = TRUE)
    # 存储结果的列表
    no_values <- c()
    not_good_examples <- c()

    # 遍历每个文件
    for (file_path in man_files) {
        # 读取文件内容
        content <- readLines(file_path)
        content <- paste0(content, collapse = "\n")
        # 检查是否包含\usage{}
        has_usage <- grepl("\\\\usage", content)
        # 如果包含\usage{}但没有\value{}
        if (has_usage) {
            # 检查是否包含\value{}
            has_value <- grepl("\\\\value", content)
            if (!has_value) no_values <- c(no_values, basename(file_path))
        }

        # 检查是否包含\examples{}
        has_examples <- grepl("\\\\examples", content)
        if (has_examples) {
            # 检查是否包含 #
            has_comment <- grepl("\\\\#", gsub(".*\\\\examples", "", content))
            # 检查是否包含\dontrun{}
            has_dontrun <- grepl("\\\\dontrun", content)
            if (has_comment || has_dontrun) not_good_examples <- c(not_good_examples, basename(file_path))
        }
    }
    pcutils::dabiao("Check \\value in .Rd files")
    if (length(no_values) > 0) {
        message("some .Rd files do not have \\value: \n", paste0(no_values, collapse = "\n"))
    } else {
        message("All is OK")
    }
    pcutils::dabiao("Check \\examples in .Rd files")
    if (length(not_good_examples) > 0) {
        message("some .Rd files do not have # or \\dontrun in \\examples: \n", paste0(not_good_examples, collapse = "\n"))
    } else {
        message("All is OK")
    }
}

check_TF_in_R_files <- function(package_folder_path = ".") {
    folder_path <- file.path(package_folder_path, "R")
    # 获取文件夹下的所有.R文件
    r_files <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)

    results <- TRUE

    # 遍历每个.R文件
    for (file_path in r_files) {
        # 读取文件内容
        content <- readLines(file_path)

        # 查找包含T/F的行数
        lines_with_TF <- grepl("(?<!['\",;:<>+{}\\[\\]^$@/_`])\\b[TF]\\b(?!['\",;:<>+{}\\[\\]^$@/_`])", content, perl = TRUE)

        # 如果有找到T/F，记录结果
        if (sum(lines_with_TF) > 0) {
            results <- FALSE
            # 记录文件路径和包含T/F的行数
            message("Find T/F in ", file_path, ":")
            message(paste0(paste0("line ", which(lines_with_TF)), collapse = "\n"))
        }
    }
    if (results) message("All is OK")
}

check_print_cat_in_R_files <- function(package_folder_path = ".", exclude = "print.R") {
    folder_path <- file.path(package_folder_path, "R")
    # 获取文件夹下的所有.R文件
    r_files <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
    r_files <- r_files[!grepl(exclude, r_files)]

    results <- TRUE

    # 遍历每个.R文件
    for (file_path in r_files) {
        # 读取文件内容
        content <- readLines(file_path)

        # 查找包含 cat 或 print 的行数
        lines_with_cat_print <- grepl("\\b(cat|print)\\([\\\"|\\']", content)
        # 如果是if (verbose)那就ok
        lines_with_verbose <- grepl("if (verbose)", content)

        lines_with_cat_print <- lines_with_cat_print & (!lines_with_verbose)
        # 如果有找到，记录结果
        if (sum(lines_with_cat_print) > 0) {
            results <- FALSE
            # 记录文件路径和包含 cat 或 print 的行数
            message("Find cat or print in ", file_path, ":")
            message(paste0(paste0("line ", which(lines_with_cat_print)), collapse = "\n"))
        }
    }
    if (results) message("All is OK")
}

#' Prepare a package
#'
#' @param pkg_dir defalut: "."
#' @param exclude vector for excluding .R files
#'
#' @return No value
#' @export
prepare_package <- function(pkg_dir = ".", exclude = "print.R") {
    # Check the package name is available or not
    # available::available(get_package_info(pkg_dir)$Package)
    pcutils::dabiao("Styler all codes")
    styler::style_pkg(pkg = pkg_dir, strict = TRUE, indent_by = 4)
    pcutils::dabiao("Write documents")
    devtools::document(pkg_dir)
    check_Rds(pkg_dir)
    pcutils::dabiao("Check T/F")
    check_TF_in_R_files(pkg_dir)
    pcutils::dabiao("Check cat/print")
    check_print_cat_in_R_files(pkg_dir, exclude = exclude)
}

#' Re-install my packages
#' @param pkgs pkgs
#' @return No return value
reinstall_my_packages <- function(pkgs = c("pcutils", "pctax", "MetaNet", "ReporterScore")) {
    for (i in pkgs) {
        if (i == "pctax") i <- "pctax/pctax"
        if (i == "MetaNet") i <- "MetaNet/MetaNet"
        if (i == "ReporterScore") i <- "GRSA/ReporterScore"
        if (i == "iCRISPR") i <- "CRISPR/iCRISPR"
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
