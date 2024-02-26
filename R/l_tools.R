common_list <- list(
    taxaclass = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
)

print_authors_affiliation <- function(authors = c("jc", "pc")) {
    affiliations <- c(
        "1" = "MOE Key Laboratory of Biosystems Homeostasis & Protection, and Zhejiang Provincial Key Laboratory of Cancer Molecular Cell Biology, Life Sciences Institute, Zhejiang University, Hangzhou, Zhejiang 310030, China",
        "2" = "State Key Laboratory for Diagnosis and Treatment of Infectious Diseases, National Clinical Research Center for Infectious Diseases, First Affiliated Hospital, Zhejiang University School of Medicine, Hangzhou, Zhejiang 310009, China",
        "3" = "Center for Life Sciences, Shaoxing Institute, Zhejiang University, Shaoxing, Zhejiang 321000, China",
        "4" = "BGI Research, Wuhan, Hubei 430074, China",
        "5" = "BGI Research, Shenzhen, Guangdong 518083, China",
        "6" = "Department of Genetics, Stanford University School of Medicine, Stanford, CA, USA"
    )
    author_list <- list(
        jc = 1:3,
        pc = 1:2,
        lye = 1:2,
        lz = 1:2,
        jlyq = 1:2,
        hzn = 1:2,
        cq = 1:2,
        tsj = 4:5,
        sxt = 6
    )
    pa <- affiliations[author_list[authors] %>% Reduce(union, .)]
    for (i in seq_along(pa)) {
        pa[i] <- paste0("^", i, "^", pa[i])
    }
    paste0(pa, collapse = "\n\n") %>% clipr::write_clip()
    message(paste0(pa, collapse = "\n\n"))
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
    lib_ps("clipr", library = FALSE)
    if (is.numeric(vec)) {
        clipr::write_clip(paste0("c(", paste0(vec, collapse = ","), ")"))
    } else {
        clipr::write_clip(paste0('c("', paste0(vec, collapse = '","'), '")'))
    }
    message("copy done, just Ctrl+V")
}

#' Copy a data.frame
#'
#' @param df a R data.frame object
#'
#' @export
#' @return No return value
copy_df <- function(df) {
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
    if (length(default) == 0) default <- NULL
    if (length(update) == 0) update <- NULL
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

        if (library) suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
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
#' data(otutab)
#' sanxian(otutab)
#' }
sanxian <- function(df, digits = 3, nrow = 10, ncol = 10, fig = FALSE, mode = 1, background = "#D7261E", ...) {
    if (nrow(df) > nrow) df <- df[1:nrow, , drop = FALSE]
    if (ncol(df) > ncol) df <- df[, 1:ncol, drop = FALSE]

    if (fig) {
        lib_ps("ggpubr", "dplyr", library = FALSE)
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

#' Gsub applied on a data.frame
#'
#' @param pattern search pattern
#' @param replacement a replacement for matched pattern
#' @param x your data.frame
#' @param ... additional arguments for gerpl()
#'
#' @return a logical data.frame
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
    y
}

# =======Read file========

#' Read some special format file
#'
#' @param file file path
#' @param format "blast", "diamond", "fa", "fasta", "fna", "gff", "gtf","jpg", "png", "pdf", "svg"...
#' @param just_print just print the file
#' @param all_yes all_yes?
#' @param ... additional arguments
#' @param density the resolution for reading pdf or svg
#'
#' @return data.frame
#' @export
#'
read.file <- function(file, format = NULL, just_print = FALSE, all_yes = FALSE, density = 120, ...) {
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
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        graphics::par(mar = rep(0, 4))

        if (is.null(format)) format <- tools::file_ext(file)
        format <- match.arg(format, c(
            "blast", "diamond", "fa", "fasta", "fna", "gff", "gtf",
            "jpg", "png", "pdf", "svg", "gif", "biom"
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

        if (format %in% c("biom")) {
            if (file.size(file) > 10000) {
                message(paste0(file, ": this biom file is a little big: ", file.size(file), " still open? (as 10Mb biom will be a about 3Gb data.frame!)"))
                flag <- readline("yes/no(y/n)?")
                if (tolower(flag) %in% c("yes", "y")) {
                    lib_ps("biomformat", library = FALSE)
                    dat.b <- biomformat::read_biom(file)
                    df <- data.frame(data.matrix(biomformat::biom_data(dat.b)), check.names = FALSE)
                } else {
                    return(NULL)
                }
            }
            return(df)
        }

        if (format %in% c("jpg", "png", "svg", "pdf")) {
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
    if (format == to_format) {
        message("do not need transfer")
        return(invisible())
    }

    lib_ps("ggplot2", library = FALSE)
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
        lib_ps("magick", "grid", library = FALSE)
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
#'
#' @return No value
#' @export
download2 <- function(url, file_path, timeout = 300, force = FALSE, ...) {
    if (file.exists(file_path) & !force) {
        return(invisible())
    } else {
        ori_time <- getOption("timeout")
        on.exit(options(timeout = ori_time))

        dir.create(dirname(file_path), recursive = TRUE)
        options(timeout = timeout)
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


#' Download supplemental materials according to a doi
#'
#' @param doi doi
#' @param dir dir
#' @param bget_path your bget_path
#'
#' @return file at work directory
#' @export
#'
get_doi <- function(doi, dir = "~/Downloads/", bget_path = "~/software/bget_0.3.2_Darwin_64-bit/bget") {
    if (!file.exists(bget_path)) stop("Cann't find bget! check `bget_path`")
    doi <- sub("https://doi.org/", "", doi)
    command <- paste0(bget_path, " doi ", doi, " -t2 --suppl --full-text -o ", dir)
    system(command)
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
#'     "s__Pandoraea_pnomenusa",
#'     "s__Alicycliphilus_sp._B1"
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
