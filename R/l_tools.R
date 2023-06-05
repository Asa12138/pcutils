# =========little tools=========
#' Print with =
#'
#' @param str output strings
#' @param ... strings will be paste together
#' @param char side chars default:=
#' @param n the number of output length
#' @param mode "middle", "left" or "right"
#'
#' @export
dabiao <- function(str = "", ..., n = 80, char = "=", mode = c("middle", "left", "right")) {
  str <- paste0(c(str, ...), collapse = "")
  mode <- match.arg(mode, c("middle", "left", "right"))
  if (n < nchar(str)) n <- nchar(str) + 2
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
  cat(xx, "\n")
}

#' Copy a vector
#'
#' @param vec a R vector object
#'
#' @export
#'
copy_vector <- function(vec) {
  lib_ps("clipr", library = F)
  clipr::write_clip(paste0('c("', paste0(vec, collapse = '","'), '")'))
  print("copy done, just Ctrl+V")
}

#' Change factor levels
#'
#' @param x vector
#' @param levels custom levels
#'
#' @return factor
#' @export
#'
#' @examples
#' change_fac_lev(letters[1:5],levels = c("b","a"))
change_fac_lev=function (x, levels = NULL)
{
  ordervec = factor(x)
  if (!is.null(levels)) {
    levels = intersect(levels, levels(ordervec))
    shunxu = c(levels, setdiff(levels(ordervec), levels))
    ordervec = factor(ordervec, levels = shunxu)
  }
  ordervec
}

#' Update the parameters
#' @description
#' Keep the different parameters while use the same name in update first.
#'
#' @param default default (data.frame, list, vector)
#' @param update update (data.frame, list, vector)
#'
#' @export
#'
#' @examples
#' update_param(list(a=1,b=2),list(b=5,c=5))
update_param=function(default,update){
  if(is.null(default))return(update)
  if(is.null(update))return(default)

  if(!identical(class(default),class(update)))stop("Two different class object is not allowed to update")
  if(is.data.frame(default)){
    inter = intersect(colnames(update), colnames(default))
    la = setdiff(colnames(default), inter)
    return(cbind(default[, la, drop = F], update))
  }
  if(is.list(default)){
    if(is.null(names(update))|is.null(names(default)))stop("No name")
    inter = intersect(names(update), names(default))
    la = setdiff(names(default), inter)
    return(append(default[la, drop = F], update))
  }
  if(is.vector(default)){
    if(is.null(names(update))|is.null(names(default)))stop("No name")
    inter = intersect(names(update), names(default))
    la = setdiff(names(default), inter)
    return(c(default[la, drop = F], update))
  }
}

#' Attach packages or install packages have not benn installed
#'
#' @param p_list a vector of packages list
#' @param all_yes all install try set to yes?
#' @param library should library the package or just get Namespace ?
#' @param ... packages
#'
#' @return NULL
#' @export
#'
#' @examples
#' lib_ps("ggplot2", "dplyr")
lib_ps <- function(p_list, ..., all_yes = F, library = T) {
  some_packages <- c(
    "ggsankey" = "davidsjoberg/ggsankey",
    "sankeyD3" = "fbreitwieser/sankeyD3",
    "pctax" = "Asa12138/pctax",
    "MetaNet" = "Asa12138/MetaNet",
    "ReporterScore" = "Asa12138/ReporterScore",
    "ggcor" = "Github-Yilei/ggcor",
    "chorddiag" = "mattflor/chorddiag",
    "inborutils" = "inbo/inborutils",
    "ggradar" = "ricardo-bion/ggradar",
    "pairwiseAdonis" = "pmartinezarbizu/pairwiseAdonis/pairwiseAdonis",
    "Vennerable" = "js229/Vennerable"
  )

  p_list <- c(p_list, ...)
  for (p in p_list) {
    if (!requireNamespace(p)) {
      if (!all_yes) {
        print(paste0(p, ": this package haven't install, should install?"))
        flag <- readline("yes/no(y/n)?")
      } else {
        flag <- "y"
      }

      if (tolower(flag) %in% c("yes", "y")) {
        if (p %in% names(some_packages)) {
          remotes::install_github(some_packages[p])
        } else {
          utils::install.packages(p)
        }
      } else {
        stop(paste0("exit, because '", p, "' need to install"))
      }

      if (!requireNamespace(p)) {
        if (!all_yes) {
          print(paste0(p, " is not available in CRAN, try Bioconductor?"))
          flag <- readline("yes/no(y/n)?")
        }

        if (tolower(flag) %in% c("yes", "y")) {
          if (!requireNamespace("BiocManager")) utils::install.packages("BiocManager")
          BiocManager::install(p)
        } else {
          stop(paste0("exit, because '", p, "' need to install"))
        }
      }

      if (!requireNamespace(p)) {
        cat("\n")
        stop("please try other way (github...) to install ", p)
      }
    }

    if (library) suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
}


#' Detach packages
#'
#' @param p_list a vector of packages list
#' @param origin keep the original Namespace
#' @param ... packages
#'
#' @export
del_ps <- function(p_list, ..., origin = NULL) {
  p_list <- c(p_list, ...)
  p_list <- paste0("package:", p_list)
  all <- search()
  p_list <- p_list[p_list %in% all]
  if (!is.null(origin)) p_list <- setdiff(p_list, origin)
  for (p in p_list) {
    detach(p, character.only = T)
  }
}

#' Three-line table
#'
#' @param df a dataframe
#' @param digits how many digits should remain
#' @param nrow show how many rows
#' @param ncol show how many columns
#' @param fig output as a figure
#' @param ... additional arguments e.g.(rows=NULL)
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' sanxian(otutab)
sanxian <- function(df, digits = 3, nrow = 10, ncol = 10, fig = F, ...) {
  if (nrow(df) > nrow) df <- df[1:nrow, , drop = F]
  if (ncol(df) > ncol) df <- df[, 1:ncol, drop = F]

  if (fig) {
    lib_ps("ggpubr", "dplyr", library = F)
    df %>%
      dplyr::mutate_if(is.numeric, \(x)round(x, digits = digits)) %>%
      ggpubr::ggtexttable(..., theme = ggpubr::ttheme("blank")) %>%
      ggpubr::tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 3) %>%
      ggpubr::tab_add_hline(at.row = nrow(df) + 1, row.side = "bottom", linewidth = 3) -> p
    return(p)
  } else {
    lib_ps("kableExtra", library = F)
    kableExtra::kbl(df, digits = digits, ...) %>% kableExtra::kable_classic(full_width = F, html_font = "Cambria")
  }
}

#' Transform a rgb vector to a Rcolor code
#'
#' @param x vector or three columns data.frame
#' @param rev reverse,transform a Rcolor code to a rgb vector
#'
#' @return Rcolor code like "#69C404"
#' @export
#'
#' @examples
#' rgb2code(c(12, 23, 34))
#' rgb2code("#69C404", rev = TRUE)
rgb2code <- function(x, rev = F) {
  lib_ps("dplyr", library = F)
  if (rev) {
    if (is.vector(x)) {
      grDevices::col2rgb(x) %>%
        t() %>%
        as.vector() -> A
      names(A) <- c("r", "g", "b")
      return(A)
    }
    if (is.data.frame(x)) {
      apply(x, 1, grDevices::col2rgb) %>% t() -> A
      colnames(A) <- c("r", "g", "b")
      rownames(A) <- rownames(x)
      return(A)
    }
  } else {
    if (length(x) != 3) stop("need r,g,b!")
    names(x) <- c("r", "g", "b")
    if (is.vector(x)) {
      return(grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))
    }
    if (is.data.frame(x)) {
      return(dplyr::transmute(x, code = grDevices::rgb(r, g, b, maxColorValue = 255)))
    }
  }
}

#' Judge if a characteristic is Rcolor
#' @param color characteristic
#'
#' @export
is.ggplot.color <- function(color) {
  is.col <- grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}|[A-Fa-f0-9]{8})$", color)
  is.name <- color %in% grDevices::colors()
  (is.col | is.name | is.na(color)) # NA accepted
}

#' Add alpha for a Rcolor
#' @param color Rcolor
#' @param alpha alpha, default 0.3
#'
#' @export
add_alpha <- function(color, alpha = 0.3) {
  color <- grDevices::col2rgb(color) %>%
    t() %>%
    grDevices::rgb(., maxColorValue = 255)
  paste0(color, as.hexmode(ceiling(255 * alpha)))
}

#' Plot a multi-pages pdf
#'
#' @param plist plot list
#' @param file prefix of your .pdf file
#' @param width width
#' @param height height
#' @param brower the path of Google Chrome, Microsoft Edge or Chromium in your computer.
#' @param ... additional arguments
#'
#' @export
plotpdf <- function(plist, file = "new", width = 8, height = 7, brower = "/Applications/Microsoft\ Edge.app/Contents/MacOS/Microsoft\ Edge", ...) {
  if (inherits(plist, "htmlwidget")) {
    lib_ps("pagedown", "htmlwidgets", library = F)
    if (!file.exists(brower)) stop(brower, "is not found in your computer, please give a right path for Google Chrome, Microsoft Edge or Chromium.")
    suppressMessages(htmlwidgets::saveWidget(plist, "tmppp.html"))
    pagedown::chrome_print("tmppp.html", paste0(file, ".pdf"),
      wait = 0, browser = brower,
      options = list(pageRanges = "1", paperWidth = width, paperHeight = height, ...)
    )
    file.remove("tmppp.html")
    message("pdf saved sucessfully")
  } else {
    grDevices::pdf(paste0(file, ".pdf"), width, height, ...)
    for (i in plist) {
      print(i)
    }
    grDevices::dev.off()
  }
}

#' Plot a gif
#'
#' @param plist plot list
#' @param file prefix of your .gif file
#' @param mode "gif" or "html"
#'
#' @export
plotgif <- function(plist, file = "new", mode = "gif") {
  lib_ps("animation", library = F)
  if (mode == "gif") {
    animation::saveGIF(
      for (i in plist) {
        print(i)
      },
      movie.name = paste0(file, ".gif")
    )
  }
  # transfer pngs to a gif use gifski::gifski()
  if (mode == "html") {
    nwd <- getwd()
    dir.create(paste0(file, "_html"))
    setwd(paste0(file, "_html"))
    animation::saveHTML(
      for (i in plist) {
        print(i)
      },
      movie.name = paste0(file, ".html")
    )
    setwd(nwd)
  }
}

#' Get n colors
#'
#' @param n how many colors you need
#' @param pal col1~3; or a vector of colors, you can get from here too.{RColorBrewer::brewer.pal(5,"Set2")} {ggsci::pal_aaas()(5)}
#' @param picture a picture file, colors will be extracted from the picture
#'
#' @return n colors
#' @export
#'
#' @examples
#' get_cols(10, "col2") -> my_cols
#' scales::show_col(my_cols)
#' scales::show_col(get_cols(15, RColorBrewer::brewer.pal(5, "Set2")))
#' scales::show_col(get_cols(15, ggsci::pal_aaas()(5)))
#' # scales::show_col(get_cols(4,picture="~/Desktop/test.png"))
get_cols <- function(n, pal = "col1", picture = NULL) {
  col1 <- c(
    "#8dd3c7", "#ffed6f", "#bebada", "#fb8072", "#80b1d3",
    "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
    "#ccebc5"
  )
  col2 <- c(
    "#a6cee3", "#78c679", "#c2a5cf", "#ff7f00", "#1f78b4", "#810f7c", "#ffff33",
    "#006d2c", "#4d4d4d", "#8c510a", "#d73027",
    "#7f0000", "#41b6c4", "#e7298a", "#54278f"
  )
  col3 <- c(
    "#a6bce3", "#fb9a99", "#fdbf6f", "#1f78b4", "#b2df8a", "#cab2d6", "#33a02c",
    "#e31a1c", "#ff7f00", "#6a3d9a",
    "#ffef00", "#b15928"
  )

  if (length(pal) == 1) pal <- get(pal)

  if (!is.null(picture)) {
    lib_ps("RImagePalette", "tools", "jpeg", "png", library = F)
    type <- tools::file_ext(picture)
    switch(type,
      "jpg" = {
        p1 <- jpeg::readJPEG(picture)
      },
      "png" = {
        p1 <- png::readPNG(picture)
      }
    )
    pal <- RImagePalette::image_palette(p1, n = n)
  }

  if (length(pal) < n) {
    res <- grDevices::colorRampPalette(pal)(n)
    return(res)
  }
  return(pal[seq_len(n)])
}


#' Add a global gg_theme and colors for plots
#'
#' @param set_theme your theme
#'
#' @return NULL
#' @export
#'
#' @examples
#' add_theme()
add_theme <- function(set_theme = NULL) {
  if (is.null(set_theme)) {
    mytheme <- {
      ggplot2::theme(text = ggplot2::element_text(family = "sans", size = 14)) +
        ggplot2::theme(
          plot.margin = grid::unit(rep(0.5, 4), "lines"),
          strip.background = ggplot2::element_rect(fill = NA)
        )
    }
    if (requireNamespace("ggpubr")) {
      mytheme <- {
        ggpubr::theme_pubr(base_size = 14, legend = "right") +
          ggplot2::theme(
            plot.margin = grid::unit(rep(0.5, 4), "lines"),
            strip.background = ggplot2::element_rect(fill = NA)
          )
      }
    }
  } else {
    stopifnot(inherits(set_theme, c("theme", "gg")))
    mytheme <- set_theme
  }
  mytheme <<- mytheme
}

#' Remove outliers
#'
#' @param x a numeric vector
#' @param factor default 1.5
#'
#' @export
#'
#' @examples
#' remove.outliers(c(1, 10:15))
remove.outliers <- function(x, factor = 1.5) {
  q25 <- stats::quantile(x, probs = 0.25)
  q75 <- stats::quantile(x, probs = 0.75)
  iqr <- unname(q75 - q25)
  lower.threshold <- q25 - (iqr * factor)
  upper.threshold <- q75 + (iqr * factor)
  res <- x[(x >= lower.threshold) & (x <= upper.threshold)]
  return(res)
}


#' Like uniq -c in shell to count a vector
#'
#' @param df two columns: first is type, second is number
#'
#' @export
#'
#' @examples
#' count2(data.frame(group = c("A", "A", "B", "C", "C", "A"), value = c(2, 2, 2, 1, 3, 1)))
count2 <- function(df) {
  res <- data.frame()
  type_p <- df[1, 1]
  n <- 0
  for (i in 1:nrow(df)) {
    type <- df[i, 1]
    if (type_p == type) {
      n <- n + df[i, 2]
    } else {
      res <- rbind(res, data.frame(type = type_p, n = n))
      n <- df[i, 2]
    }
    type_p <- type
  }
  res <- rbind(res, data.frame(type = type_p, n = n))
  colnames(res) <- colnames(df)[1:2]
  res
}

#' Grepl applied on a data.frame
#'
#' @param pattern search pattern
#' @param x your data.frame
#' @param ... addtitional arguments for gerpl()
#'
#' @return a logical data.frame
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

#' Group your data
#'
#' @param otutab dataframe
#' @param group group vector
#' @param margin 1 for row and 2 for column(default: 2)
#' @param act do (default: mean)
#'
#' @export
#'
#' @examples
#' data(otutab)
#' hebing(otutab,metadata$Group)
hebing<-function(otutab,group,margin=2,act='mean'){
  if (margin==2) {
    stats::aggregate(t(otutab),FUN=act,by=list(factor(group)))->a
    a[,-1]->a
    data.frame(t(a))->a
    levels(factor(group))->colnames(a)
  }
  else{
    stats::aggregate(otutab,FUN=act,by=list(factor(group)))->a
    a[,-1]->a
    levels(factor(group))->rownames(a)
  }
  return(a)
}

#' Split Composite Names
#'
#' @param x character vector
#' @param split character to split each element of vector on, see \code{\link[base]{strsplit}}
#' @param colnames colnames for the result
#' @param ... other arguments are passed to \code{\link[base]{strsplit}}
#'
#' @return dataframe
#' @export
#'
#' @examples
#' strsplit2(c("a;b", "c;d"), ";")
strsplit2 <- function(x, split, colnames = NULL, ...) {
  x <- as.character(x)
  n <- length(x)
  s <- strsplit(x, split = split, ...)
  nc <- unlist(lapply(s, length))
  out <- matrix("", n, max(nc))
  for (i in 1:n) {
    if (nc[i]) {
      out[i, 1:nc[i]] <- s[[i]]
    }
  }
  out <- as.data.frame(out)
  if (!is.null(colnames)) colnames(out) <- colnames
  out
}

#' Explode a dataframe if there are split charter in one column
#'
#' @param df dataframe
#' @param column column
#' @param split split string
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:2, b = c("a,b", "c"), c = 3:4)
#' explode(df, "b", ",")
explode <- function(df, column, split = ",") {
  lib_ps("tidyr", "dplyr", library = F)
  df <- tidyr::as_tibble(df)
  df[[column]] <- strsplit(df[, column, drop = T], split = split)
  tidyr::unnest(df, dplyr::all_of(column)) %>% as.data.frame()
}

#' Read some special format file
#'
#' @param file file path
#' @param format "blast","diamond"
#' @param just_print just print the file
#'
#' @return data.frame
#' @export
#'
read.file <- function(file, format = NULL, just_print = F) {
  if (just_print) {
    if (file.size(file) > 10000) {
      print(paste0(file, ": this file is a little big, still open?"))
      flag <- readline("yes/no(y/n)?")
      if (tolower(flag) %in% c("yes", "y")) {
        cat(readr::read_file(file))
      }
    } else {
      cat(readr::read_file(file))
    }
  } else {
    if (is.null(format)) format <- tools::file_ext(file)
    format <- match.arg(format, c(
      "blast", "diamond", "fa", "fasta", "fna", "gff", "gtf",
      "jpg", "png", "pdf", "svg"
    ))

    if (format %in% c("gff", "gtf")) {
      df <- utils::read.delim(file,
        header = FALSE, stringsAsFactors = FALSE, comment.char = "#",
        col.names = c("seqid", "source", "feature", "start", "end", "score", "strand", "phase", "attributes")
      )
      return(df)
    }

    if (format %in% c("fa", "fasta", "fna")) {
      df <- read_fasta(file)
      return(df)
    }

    if (format %in% c("blast", "diamond")) {
      df <- utils::read.table(file,
        sep = "\t",
        col.names = c(
          "Qseqid", "Sseqid", "Pident", "Length", "Mismatch", "Gapopen",
          "Qstart", "Qend", "Sstart", "Send", "E_value", "Bitscore"
        )
      )
      return(df)
    }

    if (format %in% c("jpg", "png")) {
      lib_ps("jpeg", "png", "graphics", library = F)
      switch(format,
        "jpg" = {
          p1 <- jpeg::readJPEG(file)
        },
        "png" = {
          p1 <- png::readPNG(file)
        }
      )
      graphics::plot(1:2, type = "n", axes = F, ylab = "n", xlab = "n", ann = FALSE)
      graphics::rasterImage(p1, 1, 1, 2, 2)
    }
    if (format == "svg") {
      lib_ps("grImport2", "ggpubr", library = F)
      x <- grImport2::readPicture(file)
      g <- grImport2::pictureGrob(x)
      p <- ggpubr::as_ggplot(g)
      p
    }
  }
}

#' Read fasta file
#' @param fasta_file file path
#'
#' @export
read_fasta <- function(fasta_file) {
  fasta_data <- readLines(fasta_file)
  # create a null dataframe
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

#' Write a dataframe to fasta
#'
#' @param df dataframe
#' @param file_path output file path
#'
#' @export
write_fasta <- function(df, file_path) {
  file_conn <- file(file_path, "w")

  for (i in 1:nrow(df)) {
    sequence_id <- df[i, 1]
    sequence <- df[i, 2]

    writeLines(paste0(">", sequence_id), file_conn)

    split_sequence <- strsplit(sequence, split = "")
    split_sequence <- unlist(split_sequence)
    num_chunks <- ceiling(length(split_sequence) / 70)
    for (j in 1:num_chunks) {
      start_index <- (j - 1) * 70 + 1
      end_index <- min(j * 70, length(split_sequence))
      chunk <- paste(split_sequence[start_index:end_index], collapse = "")
      writeLines(chunk, file_conn)
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
#' @param brower the path of Google Chrome, Microsoft Edge or Chromium in your computer.
#'
#' @return file at work directory
#' @export
#'
trans_format <- function(file, to_format, format = NULL, ..., brower = "/Applications/Microsoft\ Edge.app/Contents/MacOS/Microsoft\ Edge") {
  if (is.null(format)) format <- tools::file_ext(file)
  name <- tools::file_path_sans_ext(basename(file))
  out <- paste0(name, ".", to_format)

  if (to_format == "jpeg") to_format <- "jpg"
  if (format == to_format) stop("don not need transfer")

  lib_ps("ggplot2", library = F)
  if (format == "svg") {
    if (to_format == "html") {
      file.copy(file, out)
    } else {
      lib_ps("rsvg", "grImport2", library = F)
      rsvg::rsvg_svg(file, file)
      x <- grImport2::readPicture(file)
      g <- grImport2::pictureGrob(x)
      ggplot2::ggsave(g, filename = out, device = to_format, ...)
      invisible(g)
    }
  }
  if (format == "pdf") {
    lib_ps("pdftools", library = F)
    switch(to_format,
      "png" = {
        pdftools::pdf_convert(file, "png", filenames = out)
      },
      "jpg" = {
        pdftools::pdf_convert(file, "jpeg", filenames = out)
      },
      "jpeg" = {
        pdftools::pdf_convert(file, "jpeg", filenames = out)
      }
    )
  }
  # https://phantomjs.org/download.html
  # PhantomJS
  if (format == "png") {
    lib_ps("png", "grid", library = F)
    img <- png::readPNG(file)
    g <- grid::rasterGrob(img, interpolate = TRUE)
    p <- ggplot2::ggplot() +
      ggplot2::annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      ggplot2::theme_void()
    ggplot2::ggsave(p, filename = out, device = to_format, ...)
    invisible(g)
  }
  if (format == "jpg") {
    lib_ps("jpg", "grid", library = F)
    img <- jpeg::readJPEG(file)
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
        wait = 0, browser = brower, format = to_format,
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
}

#' Download supplemental materials according to a doi
#'
#' @param doi doi
#' @param dir dir
#' @param bget_path your bget_path
#'
#' @export
#'
get_doi <- function(doi, dir = "~/Downloads/", bget_path = "~/software/bget_0.3.2_Darwin_64-bit/bget") {
  if (!file.exists(bget_path)) stop("Cann't find bget! check `bget_path`")
  doi <- sub("https://doi.org/", "", doi)
  command <- paste0(bget_path, " doi ", doi, " -t2 --suppl --full-text -o ", dir)
  system(command)
}
