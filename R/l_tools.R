common_list <- list(
  taxaclass = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
)

some_packages <- c(
  "ggsankey" = "davidsjoberg/ggsankey",
  "sankeyD3" = "fbreitwieser/sankeyD3",
  "pctax" = "Asa12138/pctax",
  "MetaNet" = "Asa12138/MetaNet",
  "plot4fun" = "Asa12138/plot4fun",
  "iCRISPR" = "Asa12138/iCRISPR",
  "ReporterScore" = "Asa12138/ReporterScore",
  "ggcor" = "Github-Yilei/ggcor",
  "chorddiag" = "mattflor/chorddiag",
  "ggradar" = "ricardo-bion/ggradar",
  "pairwiseAdonis" = "pmartinezarbizu/pairwiseAdonis/pairwiseAdonis",
  "linkET" = "Hy4m/linkET",
  "ggchicklet" = "hrbrmstr/ggchicklet",
  "ggkegg" = "noriakis/ggkegg",
  "SpiecEasi" = "zdk123/SpiecEasi"
)

#' Generate Author-Affiliation Markdown and Write to Rmd File
#'
#' This function takes a data frame with two columns (`Author`, `Affiliation`),
#' deduplicates authors and affiliations while preserving their original order,
#' optionally adds custom author notes (e.g., "*", "#"), and writes the formatted
#' author list and affiliation footnotes into a `.Rmd` file compatible with
#' `bookdown::word_document2` output.
#'
#' @param df A data frame with two columns: the first for author names and the second for affiliations.
#' @param file Character. Output file path (e.g., "authors.Rmd") to write the formatted content.
#' @param affiliation_df Optional. A data frame of affiliations to define a fixed ordering. If NULL, uses unique order from `df`.
#' @param author_note Optional. A data frame with two columns: `Author` and `Note`, specifying symbol(s) (e.g., *, #) to append to each author.
#'
#' @return Invisibly returns a list with `author_line` and `affiliation_list` components.
#' @export
#'
generate_and_write_author_rmd <- function(df, file, affiliation_df = NULL, author_note = NULL) {
  colnames(df) <- c("Author", "Affiliation")
  df$Author <- trimws(df$Author)
  df$Affiliation <- trimws(df$Affiliation)

  df$Affiliation <- gsub(";\\s?", "; ", df$Affiliation)
  df <- explode(df, "Affiliation", split = "; ")

  # 保留单位顺序
  if (is.null(affiliation_df)) {
    unique_affils <- unique(df$Affiliation)
  } else {
    unique_affils <- unique(affiliation_df$Affiliation)
  }
  affil_id_map <- setNames(seq_along(unique_affils), unique_affils)

  # 保留作者顺序（首次出现顺序）
  author_order <- unique(df$Author)

  # 整理 author_note，转为命名向量（Author -> Note）
  note_map <- if (!is.null(author_note)) {
    setNames(as.character(author_note$Note), author_note$Author)
  } else {
    character(0)
  }

  # 汇总每位作者对应的单位编号
  author_affil_map <- lapply(author_order, function(author) {
    affils <- df$Affiliation[df$Author == author]
    affil_ids <- sort(unique(affil_id_map[affils]))
    note <- if (author %in% names(note_map)) note_map[[author]] else ""
    if (note == "") {
      return(paste0(author, "^", paste(affil_ids, collapse = ","), "^"))
    }
    paste0(author, "^", paste(c(affil_ids, paste0("\\", note)), collapse = ","), "^")
  })

  # 拼接作者信息行
  author_line <- paste(author_affil_map, collapse = ", ")

  # 拼接单位列表行
  affiliation_lines <- paste0("^", seq_along(unique_affils), "^", unique_affils, collapse = "\n\n")

  # 写入Rmd文件
  rmd_header <- "---\noutput:\n  bookdown::word_document2\n---\n\n"
  rmd_body <- paste0(author_line, "\n\n", affiliation_lines, "\n")
  writeLines(c(rmd_header, rmd_body), con = file)

  # 可选：返回生成内容（用于调试或直接显示）
  invisible(list(
    author_line = author_line,
    affiliation_list = affiliation_lines
  ))
}

#' Format CRediT Contributions with Optional Author Name Shortening
#'
#' @param df A data frame with columns `Author` and `Contributions`
#' @param short_name Logical. If TRUE, format author names as initials (e.g., C.P.)
#'
#' @return A character string summarizing contributions in the format:
#' "Conceptualization, A, B; Methodology, C, D; ..."
#' @export
#'
#' @examples
#' df <- data.frame(
#'   Author = c("Chen Peng", "Xin Wei"),
#'   Contributions = c("Methodology,Visualization", "Methodology")
#' )
#' format_credit_contributions(df, short_name = TRUE)
format_credit_contributions <- function(df, short_name = FALSE) {
  colnames(df) <- c("Author", "Contributions")
  df$Author <- trimws(df$Author)
  df$Contributions <- trimws(df$Contributions)

  # Author缩写
  if (short_name) {
    df$ShortAuthor <- sapply(df$Author, function(name) {
      parts <- unlist(strsplit(name, "\\s+"))
      paste0(substr(parts, 1, 1), collapse = ".") %>% paste0(".")
    })
  } else {
    df$ShortAuthor <- df$Author
  }

  # CRediT标准顺序
  credit_items <- c(
    "Conceptualization", "Data curation", "Formal analysis", "Funding acquisition",
    "Investigation", "Methodology", "Project administration", "Resources",
    "Software", "Supervision", "Validation", "Visualization",
    "Writing \u2013 original draft", "Writing \u2013 review & editing"
  )

  # 拆分每位作者的贡献项
  df$contrib_list <- strsplit(df$Contributions, "\\s*,\\s*")

  output_lines <- c()

  for (credit in credit_items) {
    authors_for_credit <- c()
    for (i in seq_len(nrow(df))) {
      if (credit %in% df$contrib_list[[i]]) {
        authors_for_credit <- c(authors_for_credit, df$ShortAuthor[i])
      }
    }
    if (length(authors_for_credit) > 0) {
      line <- paste0(credit, ", ", paste(authors_for_credit, collapse = ", "))
      output_lines <- c(output_lines, line)
    }
  }

  result <- paste(output_lines, collapse = "; ")
  return(result)
}


# =========Little tools=========
#' Print some message with =
#'
#' @param str output strings
#' @param ... strings will be paste together
#' @param char side chars default:=
#' @param n the number of output length
#' @param print print or message?
#' @param mode "middle", "left" or "right"
#'
#' @examples
#' dabiao("Start running!")
#'
#' @export
#' @return No return value
dabiao <- function(str = "", ..., n = 80, char = "=", mode = c("middle", "left", "right"), print = FALSE) {
  str <- paste0(c(str, ...), collapse = "")
  mode <- match.arg(mode, c("middle", "left", "right"))
  if (n < nchar(str)) n <- nchar(str)
  x <- (n - nchar(str)) %/% 2
  x2 <- n - nchar(str) - x
  switch(mode,
    "left" = {
      xx <- paste0(str, strrep(char, x + x2))
    },
    "middle" = {
      xx <- paste0(strrep(char, x), str, strrep(char, x2))
    },
    "right" = {
      xx <- paste0(strrep(char, x + x2), str)
    }
  )
  if (print) {
    cat(xx, "\n")
  } else {
    message(xx, "\n")
  }
}

#' Copy a vector
#'
#' @param vec a R vector object
#'
#' @export
#' @return No return value
copy_vector <- function(vec) {
  if (!interactive()) {
    stop("This function is not allowed in non-interactive mode.")
  }
  lib_ps("clipr", library = FALSE)
  if (is.numeric(vec)) {
    res <- paste0("c(", paste0(vec, collapse = ","), ")")
  } else {
    res <- paste0('c("', paste0(vec, collapse = '","'), '")')
  }
  if (!is.null(names(vec))) {
    res <- paste0("setNames(", res, ",c(", paste0('"', names(vec), '"', collapse = ","), "))")
  }

  clipr::write_clip(res)
  message("copy done, just Ctrl+V")
}

#' Copy a data.frame
#'
#' @param df a R data.frame object
#'
#' @export
#' @return No return value
copy_df <- function(df) {
  if (!interactive()) {
    stop("This function is not allowed in non-interactive mode.")
  }
  lib_ps("rio", library = FALSE)
  rio::export(df, file = "clipboard")
  message("copy done, just Ctrl+V")
}


#' Change factor levels
#'
#' @param x vector
#' @param levels custom levels
#' @param last put the custom levels to the last
#'
#' @return factor
#' @export
#'
#' @examples
#' change_fac_lev(letters[1:5], levels = c("c", "a"))
change_fac_lev <- function(x, levels = NULL, last = FALSE) {
  ordervec <- factor(x)
  if (!is.null(levels)) {
    levels <- intersect(levels, levels(ordervec))
    if (last) {
      shunxu <- c(setdiff(levels(ordervec), levels), levels)
    } else {
      shunxu <- c(levels, setdiff(levels(ordervec), levels))
    }
    ordervec <- factor(ordervec, levels = shunxu)
  }
  ordervec
}

#' Replace a vector by named vector
#'
#' @param x a vector need to be replaced
#' @param y named vector
#' @param fac consider the factor?
#' @param keep_origin keep_origin?
#'
#' @return vector
#' @export
#' @examples
#' tidai(c("a", "a", "b", "d"), c("a" = "red", b = "blue"))
#' tidai(c("a", "a", "b", "c"), c("red", "blue"))
#' tidai(c("A" = "a", "B" = "b"), c("a" = "red", b = "blue"))
#' tidai(factor(c("A" = "a", "B" = "b", "C" = "c")), c("a" = "red", b = "blue", c = "green"))
tidai <- function(x, y, fac = FALSE, keep_origin = FALSE) {
  if (is.null(y)) {
    return(x)
  }
  tmp <- y
  if (is.null(names(tmp))) {
    tmp <- rep(unique(tmp), len = length(unique(x)))
    if (fac) {
      names(tmp) <- levels(factor(x))
    } else {
      names(tmp) <- unique(x)
    }
  }
  if (keep_origin) {
    add <- setdiff(x, names(tmp))
    tmp <- c(tmp, setNames(add, add))
  }
  if (is.null(names(x))) {
    return(unname(tmp[as.character(x)]))
  }
  return(setNames(unname(tmp[as.character(x)]), names(x)))
}

#' Update the parameters
#'
#' @description
#' Keep the different parameters while use the same name in update first.
#'
#' @param default default (data.frame, list, vector)
#' @param update update (data.frame, list, vector)
#'
#' @export
#' @return same class of your input (data.frame, list or vector)
#' @examples
#' update_param(list(a = 1, b = 2), list(b = 5, c = 5))
#'
update_param <- function(default, update) {
  if (missing(default) || length(default) == 0) default <- NULL
  if (missing(update) || length(update) == 0) update <- NULL
  if (is.null(default)) {
    return(update)
  }
  if (is.null(update)) {
    return(default)
  }

  if (!identical(class(default), class(update))) stop("Two different class object is not allowed to update")
  if (is.data.frame(default)) {
    inter <- intersect(colnames(update), colnames(default))
    la <- setdiff(colnames(default), inter)
    return(cbind(default[, la, drop = FALSE], update))
  }
  if (is.list(default)) {
    if (is.null(names(update)) | is.null(names(default))) stop("No name")
    inter <- intersect(names(update), names(default))
    la <- setdiff(names(default), inter)
    return(append(default[la, drop = FALSE], update))
  }
  if (is.vector(default)) {
    if (is.null(names(update)) | is.null(names(default))) stop("No name")
    inter <- intersect(names(update), names(default))
    la <- setdiff(names(default), inter)
    return(c(default[la, drop = FALSE], update))
  }
}

#' Attach packages or install packages have not benn installed
#'
#' @param p_list a vector of packages list
#' @param all_yes all install try set to yes?
#' @param library should library the package or just get Namespace ?
#' @param ... packages
#'
#' @return No return value
#' @export
#'
lib_ps <- function(p_list, ..., all_yes = FALSE, library = TRUE) {
  no_p <- p_list[!vapply(p_list, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

  if (!interactive()) {
    if (length(no_p) > 0) {
      no_p <- paste0(seq_along(no_p), ". ", no_p)
      stop(paste0("exit, because some packages need to be installed:\n", paste(no_p, collapse = "\n")))
    }
  }

  p_list <- c(p_list, ...)
  for (p in p_list) {
    if (!requireNamespace(p, quietly = TRUE)) {
      if (!all_yes) {
        message(paste0(p, ": this package has not been installed yet, should it be installed?"))
        flag <- readline("yes/no(y/n)?")
      } else {
        flag <- "y"
      }

      if (tolower(flag) %in% c("yes", "y")) {
        if (p %in% names(some_packages)) {
          if (!requireNamespace("devtools", quietly = TRUE)) utils::install.packages("devtools")
          message("Install the ", p, "from github: ", some_packages[p])
          devtools::install_github(some_packages[p])
        } else {
          utils::install.packages(p)
        }
      } else {
        stop(paste0("exit, because '", p, "' needs to be installed"))
      }

      if (!requireNamespace(p, quietly = TRUE)) {
        if (!all_yes) {
          message(paste0(p, " is not available at CRAN, try Bioconductor?"))
          flag <- readline("yes/no(y/n)?")
        }

        if (tolower(flag) %in% c("yes", "y")) {
          if (!requireNamespace("BiocManager", quietly = TRUE)) utils::install.packages("BiocManager")
          BiocManager::install(p, update = FALSE)
        } else {
          stop(paste0("exit, because '", p, "' needs to be installed"))
        }
      }

      if (!requireNamespace(p, quietly = TRUE)) {
        stop("\nplease try other way (e.g. github...) to install ", p)
      }
    }
  }

  if (library) {
    for (p in p_list) {
      suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
    }
  }
}


#' Detach packages
#'
#' @param p_list a vector of packages list
#' @param origin keep the original Namespace
#' @param ... packages
#' @return No return value
#' @export
del_ps <- function(p_list, ..., origin = NULL) {
  p_list <- c(p_list, ...)
  p_list <- paste0("package:", p_list)
  all <- search()
  p_list <- p_list[p_list %in% all]
  if (!is.null(origin)) p_list <- setdiff(p_list, origin)
  for (p in p_list) {
    detach(p, character.only = TRUE)
  }
}

#' Three-line table
#'
#' @param df a data.frame
#' @param digits how many digits should remain
#' @param nrow show how many rows
#' @param ncol show how many columns
#' @param fig output as a figure
#' @param ... additional arguments e.g.(rows=NULL)
#' @param mode 1~2
#' @param background background color
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' \donttest{
#' if (require("kableExtra")) {
#'   data(otutab)
#'   sanxian(otutab)
#' }
#' }
sanxian <- function(df, digits = 3, nrow = 10, ncol = 10, fig = FALSE, mode = 1, background = "#D7261E", ...) {
  if (nrow(df) > nrow) df <- df[1:nrow, , drop = FALSE]
  if (ncol(df) > ncol) df <- df[, 1:ncol, drop = FALSE]

  if (fig) {
    lib_ps("ggpubr", library = FALSE)
    df %>%
      dplyr::mutate_if(is.numeric, \(x)round(x, digits = digits)) %>%
      ggpubr::ggtexttable(..., theme = ggpubr::ttheme("blank")) %>%
      ggpubr::tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 3) %>%
      ggpubr::tab_add_hline(at.row = nrow(df) + 1, row.side = "bottom", linewidth = 3) -> p
    return(p)
  } else {
    lib_ps("kableExtra", library = FALSE)
    if (mode == 1) {
      p <- kableExtra::kbl(df, digits = digits, ...) %>% kableExtra::kable_classic(full_width = FALSE, html_font = "Cambria")
    } else if (mode == 2) {
      p <- kableExtra::kbl(df, digits = digits, ...) %>%
        kableExtra::kable_classic(full_width = FALSE, html_font = "Cambria") %>%
        kableExtra::row_spec(0, bold = TRUE, color = "white", background = background) %>%
        kableExtra::row_spec(seq(2, nrow(df), 2), background = add_alpha(background))
    } else {
      p <- NULL
    }
    return(p)
  }
}

#' Grepl applied on a data.frame
#'
#' @param pattern search pattern
#' @param x your data.frame
#' @param ... addtitional arguments for gerpl()
#'
#' @return a logical matrix
#' @export
#' @examples
#' matrix(letters[1:6], 2, 3) |> as.data.frame() -> a
#' grepl.data.frame("c", a)
#' grepl.data.frame("\\w", a)
grepl.data.frame <- function(pattern, x, ...) {
  y <- if (length(x)) {
    do.call("cbind", lapply(x, "grepl", pattern = pattern, ...))
  } else {
    matrix(FALSE, length(row.names(x)), 0)
  }
  if (.row_names_info(x) > 0L) {
    rownames(y) <- row.names(x)
  }
  y
}

#' Gsub applied on a data.frame
#'
#' @param pattern search pattern
#' @param replacement a replacement for matched pattern
#' @param x your data.frame
#' @param ... additional arguments for gerpl()
#'
#' @return a data.frame
#' @export
#' @examples
#' matrix(letters[1:6], 2, 3) |> as.data.frame() -> a
#' gsub.data.frame("c", "a", a)
gsub.data.frame <- function(pattern, replacement, x, ...) {
  y <- if (length(x)) {
    do.call("cbind", lapply(x, "gsub", pattern = pattern, replacement = replacement, ...))
  } else {
    matrix(FALSE, length(row.names(x)), 0)
  }
  if (.row_names_info(x) > 0L) {
    rownames(y) <- row.names(x)
  }
  data.frame(y, check.names = FALSE)
}

#' Trans list (with NULL) to data.frame
#'
#' @param lst list (with NULL)
#'
#' @return a data.frame
#' @export
#'
list_to_dataframe <- function(lst) {
  # 提取每个列表元素并转换为数据框行
  df <- do.call(rbind, lapply(lst, function(x) {
    # 将 NULL 转换为 NA
    x[vapply(x, is.null, logical(length = 1L))] <- NA
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  return(df)
}

#' Check if a directory structure matches the expected structure
#'
#' This function compares the actual directory structure with a predefined expected structure
#' and returns `TRUE` if they match, otherwise `FALSE`. If `verbose` is `TRUE`, it prints
#' detailed information about missing or extra files/directories.
#'
#' @param root_path A character string specifying the root directory to check.
#' @param expected_structure A character vector specifying the expected directory structure.
#'                           Each element should be a relative path (e.g., "data/raw").
#' @param verbose A logical value. If `TRUE`, prints detailed information; if `FALSE`, suppresses output.
#' @param only_missing Only check the missing files/directories.
#'
#' @return A logical value: `TRUE` if the directory structure matches the expected structure,
#'         otherwise `FALSE`.
#'
#' @export
check_directory_structure <- function(root_path, expected_structure, only_missing = TRUE, verbose = FALSE) {
  # Get the actual directory structure
  actual_structure <- c(
    list.files(path = root_path, recursive = TRUE, full.names = FALSE),
    list.dirs(path = root_path, recursive = TRUE, full.names = FALSE)
  )

  # Check for missing files/directories
  missing_files <- setdiff(expected_structure, actual_structure)
  if (length(missing_files) > 0) {
    if (verbose) {
      cat("The following files or directories are missing:\n")
      print(missing_files)
    }
    return(FALSE)
  }

  if (!only_missing) {
    # Check for extra files/directories
    extra_files <- setdiff(actual_structure, expected_structure)
    if (length(extra_files) > 0) {
      if (verbose) {
        cat("The following files or directories are extra:\n")
        print(extra_files)
      }
      return(FALSE)
    }
  }

  # If no issues, return TRUE
  if (verbose) {
    cat("The directory structure matches the expected structure.\n")
  }
  return(TRUE)
}

# =======Read file========

#' Read some special format file
#'
#' @param file file path
#' @param format "blast", "diamond", "fa", "fasta", "fna", "faa", "bib", "gff", "gtf","jpg", "png", "pdf", "svg"...
#' @param just_print just print the file
#' @param all_yes all_yes?
#' @param ... additional arguments
#' @param density the resolution for reading pdf or svg
#'
#' @return data.frame
#' @export
#'
read.file <- function(file, format = NULL, just_print = FALSE, all_yes = FALSE, density = 120, ...) {
  if (!file.exists(file)) {
    stop(paste0(file, " does not exist!"))
  }
  if (!all_yes & !interactive()) {
    stop("This function is not allowed in non-interactive mode when all_yes is FALSE.")
  }

  if ((file.size(file) > 1e6) & !all_yes) {
    message(paste0(file, ": this file is a little big, still open?"))
    flag <- readline("yes/no(y/n)?")
    if (!tolower(flag) %in% c("yes", "y")) {
      return(NULL)
    }
  }
  if (just_print) {
    lib_ps("readr", library = FALSE)
    cat(readr::read_file(file))
  } else {
    if (is.null(format)) format <- tools::file_ext(file)
    format <- match.arg(format, c(
      "blast", "diamond", "gff", "gtf", "bed",
      "fa", "fasta", "fna", "faa",
      "bib", "ris",
      "jpg", "png", "pdf", "svg", "gif", "biom"
    ))

    if (format %in% c("gff", "gtf")) {
      # 读取文件内容
      lines <- readLines(file)
      # 过滤掉注释行
      data_lines <- lines[!grepl("^#", lines)]

      df <- utils::read.table(
        text = data_lines, sep = "\t",
        header = FALSE, stringsAsFactors = FALSE, comment.char = "",
        col.names = c("seqid", "source", "feature", "start", "end", "score", "strand", "phase", "attributes")
      )
      return(df)
    }

    if (format %in% c("bed")) {
      utils::read.table(file, header = FALSE, sep = "\t", stringsAsFactors = FALSE) -> df
      colnames(df) <- c(
        "chrom", "chromStart", "chromEnd", "name", "score", "strand", "thickStart",
        "thickEnd", "itemRgb", "blockCount", "blockSizes", "blockStarts"
      )[seq_len(ncol(df))]
      return(df)
    }

    if (format %in% c("fa", "fasta", "fna", "faa")) {
      df <- read_fasta(file)
      return(df)
    }

    if (format %in% c("bib", "ris")) {
      lib_ps("revtools", library = FALSE)
      df <- revtools::read_bibliography(filename = file, return_df = TRUE)
      return(df)
    }

    if (format %in% c("blast", "diamond")) {
      df <- utils::read.table(file,
        sep = "\t", header = FALSE, stringsAsFactors = FALSE, comment.char = "",
        col.names = c(
          "Qseqid", "Sseqid", "Pident", "Length", "Mismatch", "Gapopen",
          "Qstart", "Qend", "Sstart", "Send", "E_value", "Bitscore"
        )
      )
      return(df)
    }

    if (format %in% c("jpg", "png", "svg", "pdf")) {
      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))
      graphics::par(mar = rep(0, 4))

      lib_ps("magick", library = FALSE)
      image <- magick::image_read(file, density = density, ...)
      if (length(image) > 1) message("Your file has more than one page! Print the first one page.")
      plot(image[1])
    }
    if (format %in% c("gif")) {
      lib_ps("magick", library = FALSE)
      image <- magick::image_read(file, ...)
      print(image)
    }
  }
}

#' Read fasta file
#' @param fasta_file file path
#' @return data.frame
#' @export
read_fasta <- function(fasta_file) {
  fasta_data <- readLines(fasta_file)
  # create a null data.frame
  df <- data.frame(stringsAsFactors = FALSE)

  # initialize
  current_id <- ""
  current_seq <- ""

  # read fasta row by row
  for (line in fasta_data) {
    if (startsWith(line, ">")) {
      # start with > indicate name
      # add name and sequence of last one
      if (current_id != "") {
        df <- rbind(df, c(current_id, current_seq))
      }
      # update the new sequence
      current_id <- gsub(">", "", line)
      current_seq <- ""
    } else {
      # add sequence
      current_seq <- paste(current_seq, line, sep = "")
    }
  }

  # add the last sequence
  df <- rbind(df, c(current_id, current_seq))

  colnames(df) <- c("Sequence_ID", "Sequence")
  df
}

#' Write a data.frame to fasta
#'
#' @param df data.frame
#' @param file_path output file path
#' @param str_per_line how many base or animo acid in one line, if NULL, one sequence in one line.
#' @return No return value
#' @export
write_fasta <- function(df, file_path, str_per_line = 70) {
  file_conn <- file(file_path, "w")
  df <- as.data.frame(df)
  for (i in 1:nrow(df)) {
    sequence_id <- df[i, 1]
    sequence <- df[i, 2]

    writeLines(paste0(">", sequence_id), file_conn)
    if (is.null(str_per_line)) {
      writeLines(sequence, file_conn)
    } else if (str_per_line > 0) {
      split_sequence <- strsplit(sequence, split = "")
      split_sequence <- unlist(split_sequence)
      num_chunks <- ceiling(length(split_sequence) / str_per_line)
      for (j in 1:num_chunks) {
        start_index <- (j - 1) * str_per_line + 1
        end_index <- min(j * str_per_line, length(split_sequence))
        chunk <- paste(split_sequence[start_index:end_index], collapse = "")
        writeLines(chunk, file_conn)
      }
    } else {
      close(file_conn)
      stop("str_per_line should be NULL or number bigger than 1.")
    }
  }
  close(file_conn)
}

#' Transfer the format of file
#'
#' @param file input file
#' @param to_format transfer to
#' @param format input file format
#' @param ... additional argument
#' @param browser the path of Google Chrome, Microsoft Edge or Chromium in your computer.
#'
#' @return file at work directory
#' @export
#'
trans_format <- function(file, to_format, format = NULL, ..., browser = "/Applications/Microsoft\ Edge.app/Contents/MacOS/Microsoft\ Edge") {
  if (is.null(format)) format <- tools::file_ext(file)
  name <- tools::file_path_sans_ext(file)
  out <- paste0(name, ".", to_format)

  if (to_format == "jpeg") to_format <- "jpg"
  if (format == to_format) {
    message("The format is the same! No need to transfer.")
    return(invisible())
  }

  if (format == "svg") {
    if (to_format == "html") {
      file.copy(file, out)
    } else {
      lib_ps("rsvg", "grImport2", library = FALSE)
      rsvg::rsvg_svg(file, file)
      x <- grImport2::readPicture(file)
      g <- grImport2::pictureGrob(x)
      ggplot2::ggsave(g, filename = out, device = to_format, ...)
      invisible(g)
    }
  }

  if (format %in% c("pdf", "png", "jpg")) {
    lib_ps("magick", library = FALSE)
    img <- magick::image_read(file, density = 200, ...)
    g <- grid::rasterGrob(img, interpolate = TRUE)
    p <- ggplot2::ggplot() +
      ggplot2::annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      ggplot2::theme_void()
    ggplot2::ggsave(p, filename = out, device = to_format, ...)
    invisible(g)
  }

  if (format == "html") {
    if (to_format %in% c("pdf", "png", "jpeg")) {
      pagedown::chrome_print(file, out,
        wait = 0, browser = browser, format = to_format,
        options = list(
          # paperWidth=width,
          # pageRanges="1",
          # paperHeight=height,
          ...
        )
      )
    }
    if (to_format == "svg") {
      file.copy(file, out)
    }
  }
  message(paste0("Sucessfully transfered to ", to_format, " format!", "\nThe file is at ", out))
}

# ======= Web ========

#' Download File
#'
#' This function downloads a file from the provided URL and saves it to the specified location.
#'
#' @param file_path The full path to the file.
#' @param url The URL from which to download the file.
#' @param timeout timeout, 300s
#' @param force FALSE, if TRUE, overwrite existed file
#' @param ... add
#' @param proxy use proxy, default is FALSE
#'
#' @return No value
#' @export
download2 <- function(url, file_path, timeout = 300, force = FALSE, proxy = FALSE, ...) {
  if (file.exists(file_path) & !force) {
    return(invisible())
  } else {
    ori_time <- getOption("timeout")
    on.exit(options(timeout = ori_time))

    if (!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE)
    options(timeout = timeout)

    if (proxy) {
      lib_ps("r.proxy", library = FALSE)
      r.proxy::proxy()
    }

    # Download the file
    tryCatch(
      expr = {
        utils::download.file(url, destfile = file_path, ...)
      },
      error = function(e) {
        stop("Try downloading yourself from ", url)
      }
    )
  }
}


#' Search and browse the web for specified terms
#'
#' This function takes a vector of search terms, an optional search engine (default is Google),
#' and an optional base URL to perform web searches. It opens the default web browser
#' with search results for each term.
#'
#' @param search_terms A character vector of search terms to be searched.
#' @param engine A character string specifying the search engine to use (default is "google").
#'               Supported engines: "google", "bing".
#' @param base_url A character string specifying the base URL for web searches. If not provided,
#'                the function will use a default URL based on the chosen search engine.
#' @return No return value
#' @examples
#' \dontrun{
#' search_terms <- c(
#'   "s__Pandoraea_pnomenusa",
#'   "s__Alicycliphilus_sp._B1"
#' )
#'
#' # Using Google search engine
#' search_browse(search_terms, engine = "google")
#'
#' # Using Bing search engine
#' search_browse(search_terms, engine = "bing")
#' }
#'
#' @export
search_browse <- function(search_terms, engine = "google", base_url = NULL) {
  if (length(search_terms) > 30) stop("too many search_terms, please cut down to 30.")
  # 如果未提供基础URL，则根据搜索引擎设置默认URL
  if (is.null(base_url)) {
    if (engine == "google") {
      base_url <- "https://www.google.com/search?q="
    } else if (engine == "bing") {
      base_url <- "https://www.bing.com/search?q="
    } else {
      stop("Unsupported search engine. Supported engines: 'google', 'bing'")
    }
  }

  search_terms <- gsub("_", " ", search_terms)
  # 循环遍历搜索每个元素
  for (term in search_terms) {
    # 构建搜索 URL
    search_url <- paste0(base_url, utils::URLencode(term))

    # 在默认浏览器中打开搜索页面
    utils::browseURL(search_url)
  }
}

#' Translator
#'
#' language: en, zh, jp, fra, th..., see \code{https://www.cnblogs.com/pieguan/p/10338255.html}
#'
#' @param words words
#' @param from source language, default "en"
#' @param to target language, default "zh"
#' @param split split to blocks when your words are too much
#' @param verbose verbose
#'
#' @export
#' @return vector
#' @examples
#' \dontrun{
#' translator(c("love", "if"), from = "en", to = "zh")
#' }
translator <- function(words, from = "en", to = "zh", split = TRUE, verbose = TRUE) {
  pcutils_config <- show_pcutils_config()
  if (is.null(pcutils_config$baidu_appid) | is.null(pcutils_config$baidu_key)) {
    message("Please set the baidu_appid and baidu_key using set_pcutils_config:")
    message("first, get the appid and key from baidu: https://zhuanlan.zhihu.com/p/375789804 ,")
    message("then, set_pcutils_config('baidu_appid',your_appid),")
    message("and set_pcutils_config('baidu_key',your_key).")
    return(invisible())
  }

  if (identical(from, to)) {
    to <- setdiff(c("en", "zh"), from)[1]
    if (verbose) message("Same `from` and `to` language, change `to` to ", to)
  }
  words <- as.character(words)
  words[words == ""] <- " "
  orginal_words <- setNames(words, words)
  idx <- grepl("^\\s+$", words)

  if (sum(idx, na.rm = TRUE) > 0) {
    if (verbose) message("Some of your words are invalid")
    # words[idx]="NULL"
    words <- words[!idx]
  }
  words <- unique(words)
  if (length(words) == 0) {
    return(orginal_words)
  }

  if (length(orginal_words) > 1) {
    if (any(grepl("\n", words, fixed = TRUE))) {
      if (verbose) message("'\\n' was found in your words, change to ';'.")
    }
    words <- gsub("\n", ";", words, fixed = TRUE)
  } else {
    words <- strsplit(words, "\n+")[[1]]
  }
  input_words <- paste0(words, collapse = "\n")

  if (split) {
    split_words <- split_text(input_words, nchr_each = 5000)
  } else {
    split_words <- input_words
  }

  if (length(split_words) > 1) {
    res_ls <- lapply(split_words, translator, from = from, to = to, split = FALSE, verbose = FALSE)
    return(do.call(c, res_ls))
  }

  res <- baidu_translate(input_words, from = from, to = to)

  if (length(res) == length(words)) {
    names(res) <- words
    res1 <- tidai(orginal_words, res, keep_origin = TRUE)
  } else {
    warning("Some thing wrong with your words, make the output length not equal to the input length")
    res1 <- res
  }
  return(res1)
}

baidu_translate <- function(x, from = "en", to = "zh", pcutils_config = show_pcutils_config()) {
  lib_ps("openssl", "httr", "jsonlite", library = FALSE)
  water <- sample.int(4711, 1)
  sign <- sprintf("%s%s%s%s", pcutils_config$baidu_appid, x, water, pcutils_config$baidu_key)
  sign2 <- openssl::md5(sign)

  .query <- list(
    q = x, from = from, to = to,
    appid = pcutils_config$baidu_appid,
    salt = water, sign = sign2
  )

  url <- httr::modify_url("http://api.fanyi.baidu.com/api/trans/vip/translate",
    query = .query
  )
  url <- url(url, encoding = "utf-8")
  res <- jsonlite::fromJSON(url)

  return(res$trans_result$dst)
}

#' Split text into parts, each not exceeding a specified character count
#'
#' @param text Original text
#' @param nchr_each Maximum character count for each part
#' @return List of divided parts
#' @export
#'
#' @examples
#' \donttest{
#' original_text <- paste0(sample(c(letters, "\n"), 400, replace = TRUE), collapse = "")
#' parts <- split_text(original_text, nchr_each = 200)
#' lapply(parts, nchar)
#' }
split_text <- function(text, nchr_each = 200) {
  # Split the text by newline characters
  parts <- strsplit(text, "\n+")[[1]]

  # Initialize the result list
  result <- list()

  # Loop through each part to ensure each does not exceed the specified character count
  current_part <- parts[1]

  for (part in parts[-1]) {
    if (nchar(current_part) > nchr_each) message("Characters number of this paragraph is more than ", nchr_each)
    if (nchar(current_part) + nchar(part) <= nchr_each) {
      # If adding the current part to the new part does not exceed the specified character count,
      # merge them into the current part
      current_part <- paste(current_part, part, sep = "\n")
    } else {
      # If it exceeds the specified character count, add the current part to the result list
      # and start a new part
      result <- c(result, current_part)
      current_part <- part
    }
  }

  # Add the last part to the result list
  result <- c(result, current_part)

  # Return the result
  return(result)
}


#' Download genome files from NCBI based on accession number
#'
#' This function downloads specific genomic files from NCBI's FTP server
#' based on the provided accession number. It supports downloading
#' different types of files, or the entire directory containing the files.
#'
#' @param accession A character string representing the NCBI accession number
#'        (e.g., "GCF_001036115.1_ASM103611v1" or "GCF_001036115.1"). The accession can start with
#'        "GCF" or "GCA".
#' @param out_dir A character string representing the directory where the
#'        downloaded files will be saved. Defaults to the current working directory (".").
#' @param type A character string representing the type of file to download.
#'        Supported types are "all", "gff", "fna". If "all" is specified,
#'        the function will prompt the user to use command line tools to download
#'        the entire directory. Defaults to "gff".
#' @param file_suffix A character string representing the specific file suffix to download.
#'        If specified, this will override the `type` parameter. Defaults to NULL.
#' @param timeout A numeric value representing the maximum time in seconds to wait for the download. Defaults to 300.
#'
#' @details
#' If the provided `accession` does not contain the version suffix (e.g., "GCF_001036115.1"),
#' the function will query the NCBI FTP server to determine the full accession name.
#'
#' When `type` is set to "all", the function cannot download the entire directory
#' directly but provides a command line example for the user to download the directory
#' using tools like `wget`.
#' @return No value
#' @examples
#' \dontrun{
#' download_ncbi_genome_file("GCF_001036115.1", out_dir = "downloads", type = "gff")
#' download_ncbi_genome_file("GCF_001036115.1", out_dir = "downloads", file_suffix = "_genomic.fna.gz")
#' }
#'
#' @export
download_ncbi_genome_file <- function(accession, out_dir = ".", type = "gff", file_suffix = NULL, timeout = 300) {
  lib_ps("httr", library = FALSE)
  # 基础URL
  base_url <- "https://ftp.ncbi.nlm.nih.gov/genomes/all/"

  # 确定是GCA还是GCF开头
  prefix <- substr(accession, 1, 3)
  if (!(prefix %in% c("GCF", "GCA"))) {
    stop("Accession must start with 'GCF' or 'GCA'")
  }
  # 确定有无版本号
  if (substr(accession, 14, 14) != ".") {
    stop("Accession must have version number, such as GCF_001036115.1")
  }

  # 提取数字部分并格式化路径
  numbers <- gsub("GCF_|GCA_|\\..*", "", accession)
  formatted_path <- paste(substr(numbers, 1, 3), substr(numbers, 4, 6), substr(numbers, 7, 9), sep = "/")

  # 基础路径
  base_path <- paste0(base_url, prefix, "/", formatted_path, "/")

  # 如果accession不包含详细版本后缀，则需要查找完整路径
  if (!grepl("_", gsub("GCF_|GCA_", "", accession))) {
    res <- httr::GET(base_path)
    if (httr::status_code(res) != 200) {
      stop("Unable to access the directory: ", base_path)
    }
    # 提取文件夹列表
    content <- httr::content(res, "text")
    folders <- regmatches(content, gregexpr(paste0(accession, "_[^/]+"), content))
    if (length(folders[[1]]) == 0) {
      stop("No matching folder found for accession: ", accession)
    }
    # 取第一个匹配的文件夹名
    full_accession <- folders[[1]][1]
  } else {
    full_accession <- accession
  }

  # 如果指定了file_suffix，则直接使用file_suffix
  if (!is.null(file_suffix)) {
    file_name <- paste0(full_accession, file_suffix)
  } else {
    # 根据type确定文件后缀
    file_suffix <- switch(type,
      "all" = "",
      "gff" = "_genomic.gff.gz",
      "fna" = "_genomic.fna.gz",
      "gbff" = "_genomic.gbff.gz",
      "gtf" = "_genomic.gtf.gz",
      "faa" = "_protein.faa.gz",
      "gpff" = "_genomic.gpff.gz",
      stop("Unsupported type: ", type)
    )
    file_name <- if (type == "all") "" else paste0(full_accession, file_suffix)
  }

  # 生成最终的URL
  full_url <- paste0(base_path, full_accession, "/", file_name)

  # 创建输出目录（如果不存在）
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  if (type == "all") {
    # 下载整个文件夹
    message("Downloading the entire folder is not directly supported in R. You can use command line tools like wget or rsync for this purpose.")
    message("Example command: wget -r -np -nH --cut-dirs=5 ", full_url, "\n")
    return(full_url)
  } else {
    # 下载单个文件
    dest_file <- file.path(out_dir, basename(file_name))
    tryCatch(
      {
        download2(full_url, dest_file, timeout = timeout, mode = "wb")
        message("File downloaded successfully: ", dest_file)
      },
      error = function(e) {
        message("Error in downloading file: ", e)
      }
    )
  }
}
