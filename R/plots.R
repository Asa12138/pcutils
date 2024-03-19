pcutils_theme <- {
  ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      axis.text = element_text(color = "black"),
      plot.margin = grid::unit(rep(0.5, 4), "lines"),
      strip.background = ggplot2::element_rect(fill = NA)
    )
}

# ========Utils for ggplot=======

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
rgb2code <- function(x, rev = FALSE) {
  r <- g <- b <- NULL
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
#' @return TRUE or FALSE
#' @examples
#' is.ggplot.color("red")
#' is.ggplot.color("notcolor")
#' is.ggplot.color(NA)
#' is.ggplot.color("#000")
is.ggplot.color <- function(color) {
  is.col <- grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$", color)
  is.name <- color %in% grDevices::colors()
  (is.col | is.name | is.na(color)) # NA accepted
}

#' Add alpha for a Rcolor
#' @param color Rcolor
#' @param alpha alpha, default 0.3
#' @return 8 hex color
#' @export
#' @examples
#' add_alpha("red", 0.3)
add_alpha <- function(color, alpha = 0.3) {
  if ((alpha > 1) | (alpha < 0)) stop("alpha should be 0~1")
  color <- grDevices::col2rgb(color) %>%
    t() %>%
    grDevices::rgb(., maxColorValue = 255)
  fix <- as.hexmode(ceiling(255 * alpha))
  if (nchar(fix) == 1) fix <- paste0("0", fix)
  paste0(color, fix)
}

#' Plot a multi-pages pdf
#'
#' @param plist plot list
#' @param file prefix of your .pdf file
#' @param width width
#' @param height height
#' @param browser the path of Google Chrome, Microsoft Edge or Chromium in your computer.
#' @param ... additional arguments
#'
#' @return No return value
#' @export
plotpdf <- function(plist, file, width = 8, height = 7, browser = "/Applications/Microsoft\ Edge.app/Contents/MacOS/Microsoft\ Edge", ...) {
  if (inherits(plist, "htmlwidget")) {
    lib_ps("pagedown", "htmlwidgets", library = FALSE)
    if (!file.exists(browser)) stop(browser, "is not found in your computer, please give a right path for Google Chrome, Microsoft Edge or Chromium.")
    suppressMessages(htmlwidgets::saveWidget(plist, "tmppp.html"))
    pagedown::chrome_print("tmppp.html", paste0(file, ".pdf"),
      wait = 0, browser = browser,
      options = list(pageRanges = "1", paperWidth = width, paperHeight = height, ...)
    )
    file.remove("tmppp.html")
    message("pdf saved sucessfully in ", file, ".pdf")
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
#' @param speed 1
#' @param file prefix of your .gif file
#' @param ... add
#'
#' @return No return value
#' @export
plotgif <- function(plist, file, speed = 1, ...) {
  lib_ps("gifski", library = FALSE)
  gifski::save_gif(
    {
      for (i in plist) {
        print(i)
      }
    },
    gif_file = paste0(file, ".gif"),
    delay = 1 / speed,
    ...
  )

  # if (mode == "gif") {
  #     animation::saveGIF(
  #         for (i in plist) {
  #             print(i)
  #         },
  #         movie.name = paste0(file, ".gif")
  #     )
  # }
}

#' Get n colors
#'
#' @param n how many colors you need
#' @param pal col1~3; or a vector of colors, you can get from: `RColorBrewer::brewer.pal(5,"Set2")` or `ggsci::pal_aaas()(5)`
#'
#' @return a vector of n colors
#' @export
#'
#' @examples
#' get_cols(10, "col2") -> my_cols
#' scales::show_col(my_cols)
#' \donttest{
#' scales::show_col(get_cols(15, RColorBrewer::brewer.pal(5, "Set2")))
#' }
get_cols <- function(n = 11, pal = "col1") {
  col1 <- c(
    "#8dd3c7", "#F8CC00", "#bebada", "#fb8072", "#80b1d3",
    "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
    "#ccebc5"
  )
  col2 <- c(
    "#a6cee3", "#78c679", "#c2a5cf", "#ff7f00", "#1f78b4",
    "#810f7c", "#F8CC00", "#006d2c", "#4d4d4d", "#8c510a",
    "#d73027", "#7f0000", "#41b6c4", "#e7298a", "#54278f"
  )
  col3 <- c(
    "#a6bce3", "#fb9a99", "#fdbf6f", "#1f78b4", "#b2df8a",
    "#cab2d6", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
    "#F8CC00", "#b15928"
  )

  bluered <- rev(RColorBrewer::brewer.pal(11, "RdBu"))

  if (length(pal) == 1) pal <- get(pal)

  if (length(pal) < n) {
    res <- grDevices::colorRampPalette(pal)(n)
    return(res)
  }
  return(pal[seq_len(n)])
}

pal_pc <- function(palette = c("col1", "col2", "col3", "bluered"), alpha = 1, n = 11) {
  palette <- match.arg(palette)
  if (alpha > 1L | alpha <= 0L) {
    stop("alpha must be in (0, 1]")
  }
  raw_cols <- get_cols(n = n, pal = palette)
  alpha_cols <- add_alpha(raw_cols, alpha)
  scales::manual_pal(unname(alpha_cols))
}

#' Scale a fill color
#' @param palette col1~3; or a vector of colors, you can get from: `RColorBrewer::brewer.pal(5,"Set2")` or `ggsci::pal_aaas()(5)`
#' @param alpha alpha
#' @param n how many colors you need
#' @param ... additional
#' @param alpha alpha
#' @return scale_fill
#' @export
scale_fill_pc <- function(palette = c("col1", "col2", "col3", "bluered"), alpha = 1, n = 11, ...) {
  palette <- match.arg(palette)
  discrete_scale(
    "fill", "pc", pal_pc(palette, alpha, n = 11),
    ...
  )
}

#' Scale a fill color
#'
#' @param palette col1~3; or a vector of colors, you can get from: `RColorBrewer::brewer.pal(5,"Set2")` or `ggsci::pal_aaas()(5)`
#' @param n how many colors you need
#' @param ... additional
#' @param alpha alpha
#'
#' @return scale_color
#' @export
scale_color_pc <- function(palette = c("col1", "col2", "col3", "bluered"), alpha = 1, n = 11, ...) {
  palette <- match.arg(palette)
  discrete_scale(
    "color", "pc", pal_pc(palette, alpha, n = 11),
    ...
  )
}

#' Add a global gg_theme and colors for plots
#'
#' @param set_theme your theme
#'
#' @return No return value
#' @export
#'
#' @examples
#' add_theme()
add_theme <- function(set_theme = NULL) {
  if (is.null(set_theme)) {
    mytheme <- {
      ggplot2::theme_classic(base_size = 13) +
        ggplot2::theme(
          axis.text = element_text(color = "black"),
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

#' Scale a legend size
#'
#' @param scale default: 1.
#' @return "theme" "gg"
#' @export
#'
legend_size <- function(scale = 1) {
  ggplot2::theme(
    legend.title = ggplot2::element_text(size = 12 * scale),
    legend.text = ggplot2::element_text(size = 10 * scale),
    legend.key.size = grid::unit(7 * scale, "mm")
  )
}

#' Get a ggplot xlim and ylim
#'
#' @param p ggplot
#'
#' @return list
#' @export
#'
ggplot_lim <- function(p) {
  stopifnot(inherits(p, "ggplot"))
  p1 <- ggplot_build(p)
  lims <- list(
    x = p1$layout$panel_scales_x[[1]]$range$range,
    y = p1$layout$panel_scales_y[[1]]$range$range
  )
  lims
}


#' Generate labels position
#'
#' @param labels labels
#' @param input c(0,0)
#' @param x_offset 0.3
#' @param y_offset 0.15
#' @param just 0~5
#' @param nrows default: NULL
#' @param ncols default: NULL
#'
#' @return matrix
#' @export
#'
#' @examples
#' library(ggplot2)
#' labels <- vapply(1:8, \(i)paste0(sample(LETTERS, 4), collapse = ""), character(1))
#' df <- data.frame(label = labels, generate_labels(labels))
#' ggplot(data = df) +
#'   geom_label(aes(x = X1, y = X2, label = label))
generate_labels <- function(labels = NULL, input = c(0, 0), nrows = NULL, ncols = NULL, x_offset = 0.3, y_offset = 0.15, just = 1) {
  total_points <- length(labels)
  # Calculate the number of rows and columns
  if (is.null(nrows)) {
    rows <- ceiling(sqrt(total_points))
  } else {
    rows <- nrows
  }
  cols <- ceiling(total_points / rows)
  if (!is.null(ncols)) {
    cols <- ncols
    rows <- ceiling(total_points / cols)
  }

  # Generate points around the input point in a rectangular grid
  points <- matrix(ncol = 2, nrow = total_points)
  for (i in 1:total_points) {
    col_idx <- (i - 1) %% cols + 1
    row_idx <- floor((i - 1) / cols) + 1

    # Distribute points around the input with specified offsets
    if (col_idx == 1) {
      points[i, ] <- input + c(0, (row_idx - 1) * -y_offset)
    } else {
      points[i, ] <- points[i - 1, ] + c(x_offset * (0.5 + (nchar(labels[i - 1]) + nchar(labels[i])) / 12), 0)
    }
  }
  if (just != 1) {
    center <- apply(points, 2, mean)
    switch(just,
      "0" = {
        center <- center
      },
      "2" = {
        center[1] <- 2 * center[1]
        center[2] <- 0
      },
      "3" = {
        center[1] <- 0
        center[2] <- center[2] * 2
      },
      "4" = {
        center[1] <- 2 * center[1]
        center[2] <- center[2] * 2
      }
    )
    points[, 1] <- points[, 1] - center[1]
    points[, 2] <- points[, 2] - center[2]
  }
  return(points)
}


match_df <- function(otutab, metadata) {
  if (!setequal(rownames(metadata), colnames(otutab))) message("rownames don't match in tab and metadata")
  idx <- rownames(metadata) %in% colnames(otutab)
  metadata <- metadata[idx, , drop = FALSE]
  otutab <- otutab[, rownames(metadata), drop = FALSE]
  return(list(otutab = otutab, metadata = metadata))
}



#' Translate axis label of a ggplot
#'
#' @param gg a ggplot object to be translated
#' @param which vector contains one or more of 'x', 'y', 'label', 'fill', 'color'..., or 'facet_x', 'facet_y', 'labs' and 'all' to select which texts to be translated.
#' @param from source language
#' @param to target language
#' @param verbose verbose
#' @param keep_original_label keep the source language labels
#' @param original_sep default, '\\n'
#'
#' @return ggplot
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Subject = c("English", "Math"),
#'   Score = c(59, 98), Motion = c("sad", "happy")
#' )
#' ggp <- ggplot(df, mapping = aes(x = Subject, y = Score, label = Motion)) +
#'   geom_text() +
#'   geom_point() +
#'   labs(x = "Subject", y = "Score", title = "Final Examination")
#' ggplot_translator(ggp, which = "all")
#' }
ggplot_translator <- function(gg, which = c("x", "y"), from = "en", to = "zh",
                              keep_original_label = FALSE, original_sep = "\n", verbose = TRUE) {
  if (verbose) {
    message("Please set the font family to make the labels display well.\n see `how_to_set_font_for_plot()`.")
    lib_ps("sysfonts", "showtext", library = FALSE)
    showtext::showtext_auto()
  }
  if (identical(from, to)) {
    to <- setdiff(c("en", "zh"), from)[1]
    if (verbose) message("Same `from` and `to` language, change `to` to ", to)
  }

  stopifnot(inherits(gg, "gg"))
  which <- unique(which)

  # all_lab=c("x", "y", "label", "labs")
  # if(length(which)==1)which <- match.arg(which, c(all_lab, "all"))

  mappings <- unlist(lapply(gg$mapping, rlang::quo_text))
  if (length(gg$facet$params) > 0) {
    if (length(gg$facet$params$rows) > 0) {
      mappings <- c(mappings, "facet_y" = names(gg$facet$params$rows)[1])
    }
    if (length(gg$facet$params$cols) > 0) {
      mappings <- c(mappings, "facet_x" = names(gg$facet$params$cols)[1])
    }
  }

  if (identical(which, "all")) which <- c(names(mappings), "labs")

  if (is.null(attributes(gg)$translated)) {
    translated <- ""
  } else {
    translated <- attributes(gg)$translated
  }

  if (length(which) == 1) {
    if (which == "labs") {
      ori_labels <- unlist(gg$labels)
      trans_labels <- translator(ori_labels, from = from, to = to)
      if (keep_original_label) trans_labels <- paste0(trans_labels, original_sep, "(", names(trans_labels), ")")
      gg$labels <- as.list(setNames(trans_labels, names(ori_labels)))
    } else {
      col_name <- mappings[which]
      if (col_name %in% translated | is.na(col_name)) {
        return(gg)
      } else {
        attributes(gg)$translated <- c(translated, col_name)
      }
      words <- gg$data[[col_name]]
      # if (is.null(words)) message("You should use the `mapping=aes(x=x,y=y)` in `ggplot()` instead of `geom_XX`.")
      if (is.character(words) | is.factor(words)) {
        trans_words <- translator(words, from = from, to = to)
        if (keep_original_label) {
          trans_words <- setNames(
            paste0(trans_words, original_sep, "(", names(trans_words), ")"),
            names(trans_words)
          )
        } else {
          tmpdf <- data.frame(name = names(trans_words), trans = trans_words)
          tmpdf <- dplyr::distinct_all(tmpdf)
          if (any(duplicated(tmpdf$trans))) {
            duplicated_names <- tmpdf$trans[duplicated(tmpdf$trans)]
            indx <- which(trans_words %in% duplicated_names)
            trans_words[indx] <- paste0(trans_words[indx], original_sep, "(", names(trans_words[indx]), ")")
          }
        }
        if (is.factor(words)) {
          trans_words <- factor(trans_words, levels = trans_words[levels(words)])
        }
        gg$data[[col_name]] <- trans_words
      }
    }
  } else {
    for (i in which) {
      gg <- ggplot_translator(gg,
        which = i, from = from, to = to, keep_original_label = keep_original_label,
        original_sep = original_sep, verbose = FALSE
      )
    }
  }
  return(gg)
}

# ========Common plots=======

#' Plot a general venn (upset, flower)
#'
#' @param ... additional
#'
#' @return a plot
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   aa <- list(a = 1:3, b = 3:7, c = 2:4)
#'   venn(aa, mode = "venn")
#'   venn(aa, mode = "network")
#'   venn(aa, mode = "upset")
#'   data(otutab)
#'   venn(otutab, mode = "flower")
#' }
#' }
venn <- function(...) {
  UseMethod("venn")
}

venn_cal <- function(otu_time) {
  aa <- list()
  for (i in 1:ncol(otu_time)) {
    name <- colnames(otu_time)[i]
    aa[[name]] <- rownames(otu_time[otu_time[, i] > 0, ])
  }
  return(aa)
}

#' @method venn list
#' @rdname venn
#'
#' @param aa list
#' @param mode "venn","venn2","upset","flower","network"
#' @param elements_label logical, show elements label in network?
#' @param ... add
#'
#' @return a plot
#' @exportS3Method
venn.list <- function(aa, mode = "venn", elements_label = TRUE, ...) {
  if (is.null(names(aa))) names(aa) <- seq_along(aa)
  # if (length(aa) > 4 && mode == "venn") message("venn < 4, recommend upset or flower")
  if (mode == "venn") {
    lib_ps("ggVennDiagram", library = FALSE)
    do.call(
      ggVennDiagram::ggVennDiagram,
      update_param(
        list(x = aa, label_geom = "text", label_alpha = 1),
        list(...)
      )
    ) -> p
    p <- p + scale_color_manual(values = rep("black", length(aa))) +
      scale_fill_gradient(low = "white", high = "red2") +
      theme(legend.position = "none")
    return(p)
  }

  # if (mode == "venn2") {
  #   if (!requireNamespace("RBGL")) BiocManager::install("RBGL")
  #   lib_ps("Vennerable", library = FALSE)
  #   Vennerable::Venn(aa) -> aap
  #   Vennerable::plot(aap)
  #   # plot(aap,type="triangles")
  #   # plot(aap, doWeights = FALSE)
  #   # plot(aap, doWeights = FALSE,type="ellipses")
  #   # plot(aap, doWeights = FALSE,type="ChowRuskey")
  # }

  if (mode == "upset") {
    lib_ps("UpSetR", library = FALSE)
    UpSetR::upset(UpSetR::fromList(aa), order.by = "freq", nsets = length(aa), nintersects = 30, ...) -> p
    return(p)
  }
  if (mode == "flower") {
    venn_flower(aa, ...)
  }
  if (mode == "network") {
    venn_net_internal(aa, elements_label = elements_label, ...)
  }
}

venn_net_internal <- function(vennlist, elements_label = TRUE, ...) {
  lib_ps("igraph", library = FALSE)
  edgelist <- data.frame()
  groupss <- names(vennlist)
  for (i in groupss) {
    if (length(vennlist[[i]] > 0)) edgelist <- rbind(edgelist, data.frame(Group = i, elements = vennlist[[i]]))
  }

  nodelist1 <- data.frame(name = groupss, v_group = "Group", label = groupss, all_group = groupss)
  nodelist2 <- data.frame(name = unique(edgelist$elements), v_group = "elements", label = NA)
  if (elements_label) nodelist2$label <- nodelist2$name

  all_group <- edgelist %>%
    pcutils::squash("Group") %>%
    dplyr::rename(name = "elements", all_group = "Group")
  nodelist2 <- dplyr::left_join(nodelist2, all_group, by = "name")

  nice_size <- ceiling(60 / sqrt(nrow(nodelist1) + nrow(nodelist2))) + 1
  nodelist1$size <- 1.5 * nice_size
  nodelist2$size <- 0.5 * nice_size

  nodelist <- rbind(nodelist1, nodelist2)

  tmp_col <- paste0(nodelist$v_group, "-", nodelist$all_group)
  nodelist$color <- tidai(tmp_col, pcutils::get_cols(nlevels(factor(tmp_col)), "col3"), fac = TRUE)
  nodelist$label.cex <- 0.08 * nodelist$size
  nodelist$label.color <- "black"

  edgelist$color <- tidai(edgelist$Group, unique(nodelist$color), fac = TRUE)
  edgelist$curved <- 0.3
  go <- igraph::graph_from_data_frame(edgelist, directed = FALSE, vertices = nodelist)

  igraph::plot.igraph(x = go, ...)
}

venn_flower <- function(aa) {
  lib_ps("plotrix", library = FALSE)
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(bty = "n", ann = FALSE, xaxt = "n", yaxt = "n", mar = c(1, 1, 1, 1))

  otu_num <- length(aa[[1]])
  core_otu_id <- aa[[1]]
  for (i in 2:length(aa)) {
    core_otu_id <- intersect(core_otu_id, aa[[i]])
    otu_num <- c(otu_num, length(aa[[i]]))
  }
  core_num <- length(core_otu_id)
  otu_num <- otu_num - core_num
  sample_id <- names(aa)
  n <- length(sample_id)

  ellipse_col <- grDevices::colorRampPalette(get_cols(10))(n)
  start <- 90
  a <- 0.5
  b <- 2.2
  r <- 0.5
  ellipse_col <- ellipse_col
  circle_col <- "white"


  plot(c(0, 10), c(0, 10), type = "n")
  deg <- 360 / n
  res <- lapply(1:n, function(t) {
    plotrix::draw.ellipse(
      x = 5 + cos((start + deg * (t - 1)) * pi / 180),
      y = 5 + sin((start + deg * (t - 1)) * pi / 180),
      col = ellipse_col[t],
      border = ellipse_col[t],
      a = 0.6, b = 2.2, angle = deg * (t - 1)
    )

    graphics::text(
      x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
      y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
      otu_num[t]
    )

    if (deg * (t - 1) < 180 && deg * (t - 1) > 0) {
      graphics::text(
        x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
        y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
        sample_id[t],
        srt = deg * (t - 1) - start,
        adj = 1,
        cex = 1
      )
    } else {
      graphics::text(
        x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
        y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
        sample_id[t],
        srt = deg * (t - 1) + start,
        adj = 0,
        cex = 1
      )
    }
  })
  plotrix::draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = NA)
  graphics::text(x = 5, y = 5, paste("Core:", core_num))
}


#' @param otutab table
#' @param mode "venn","venn2","upset","flower"
#' @param elements_label logical, show elements label in network?
#' @param ... add
#' @return a plot
#' @method venn data.frame
#' @rdname venn
#' @exportS3Method
venn.data.frame <- function(otutab, mode = "venn", elements_label = TRUE, ...) {
  venn_cal(otutab) -> aa
  venn.list(aa, mode = mode, elements_label = elements_label)
}


# Preprocess data for stack plot
pre_stack_data <- function(otutab, metadata = NULL, group = "Group",
                           topN = 8, others = TRUE, relative = TRUE,
                           stack_order = TRUE, group_order = FALSE, facet_order = FALSE,
                           style = c("group", "sample")[1]) {
  variable <- Taxonomy <- value <- n <- NULL
  if (is.numeric(metadata[, group, drop = TRUE])) warning("Recommend categorical variables")
  # prepare otutab and sampFile
  if (!is.null(metadata)) {
    match_res <- match_df(otutab, metadata)
    otutab <- match_res$otutab
    sampFile <- as.data.frame(match_res$metadata[, group], row.names = row.names(match_res$metadata))
    colnames(sampFile)[1] <- "group"
  } else {
    sampFile <- data.frame(row.names = colnames(otutab), group = colnames(otutab))
  }

  mean_sort <- as.data.frame(otutab[(order(-rowSums(otutab))), , drop = FALSE])

  if (is.numeric(topN)) {
    if (nrow(mean_sort) > topN) {
      other <- colSums(mean_sort[topN:dim(mean_sort)[1], ])
      mean_sort <- mean_sort[1:(topN - 1), ]
      mean_sort <- rbind(mean_sort, other)
      rownames(mean_sort)[topN] <- c("Other")
    }
  } else {
    other <- colSums(mean_sort[!rownames(mean_sort) %in% topN, ])
    mean_sort <- mean_sort[rownames(mean_sort) %in% topN, ]
    mean_sort <- rbind(mean_sort, other)
    rownames(mean_sort)[nrow(mean_sort)] <- c("Other")
  }

  if (style == "sample") {
    mean_sort$Taxonomy <- rownames(mean_sort)
    data_all <- as.data.frame(reshape2::melt(mean_sort, id.vars = c("Taxonomy")))
  } else {
    mat_t <- t(mean_sort)
    stats::aggregate(mat_t, by = list(sampFile$group), mean) %>% reshape2::melt(., id = 1) -> data_all
    colnames(data_all) <- c("variable", "Taxonomy", "value")
    data_all$value <- as.numeric(data_all$value)
    data_all$variable <- as.factor(data_all$variable)
  }

  if (relative) {
    data_all <- data_all %>%
      dplyr::group_by(variable, Taxonomy) %>%
      dplyr::summarise(n = sum(value)) %>%
      dplyr::mutate(value = n / sum(n))
  }

  if (style == "sample") {
    data_all <- merge(data_all, sampFile,
      by.x = "variable",
      by.y = "row.names"
    )

    group_by(data_all, group, Taxonomy) %>% summarise(value = mean(value)) -> data_all_facet
    # determine the facet order
    if (setequal(facet_order, 1)) {
      new_lev <- (data_all_facet %>% dplyr::filter(Taxonomy == rownames(mean_sort)[1]) %>%
        dplyr::arrange(value) %>% as.data.frame())[, 1] %>% as.character()
      data_all <- dplyr::mutate(data_all, group = factor(group, levels = new_lev))
    } else if (facet_order[1] %in% data_all$Taxonomy) {
      new_lev <- (data_all_facet %>% dplyr::filter(Taxonomy == facet_order) %>%
        dplyr::arrange(value) %>% as.data.frame())[, 1] %>% as.character()
      data_all <- dplyr::mutate(data_all, group = factor(group, levels = new_lev))
    } else if (any(facet_order %in% data_all_facet$group)) {
      data_all <- dplyr::mutate(data_all, group = change_fac_lev(group, levels = facet_order))
    }
  }

  if (!others) {
    data_all <- data_all[data_all$Taxonomy != "Other", ]
  }
  # determine the stack order
  if (setequal(stack_order, 1)) {
    data_all$Taxonomy <- factor(data_all$Taxonomy, levels = rownames(mean_sort))
  } else if (any(stack_order %in% data_all$Taxonomy)) {
    data_all$Taxonomy <- change_fac_lev(data_all$Taxonomy, levels = stack_order)
  }
  # determine the x axis order
  if (setequal(group_order, 1)) {
    new_lev <- (data_all %>% dplyr::filter(Taxonomy == rownames(mean_sort)[1]) %>%
      dplyr::arrange(value) %>% as.data.frame())[, 1] %>% as.character()
    data_all <- dplyr::mutate(data_all, variable = factor(variable, levels = new_lev))
  } else if (group_order[1] %in% data_all$Taxonomy) {
    new_lev <- (data_all %>% dplyr::filter(Taxonomy == group_order) %>%
      dplyr::arrange(value) %>% as.data.frame())[, 1] %>% as.character()
    data_all <- dplyr::mutate(data_all, variable = factor(variable, levels = new_lev))
  } else if (any(group_order %in% data_all$variable)) {
    # data_all <- dplyr::mutate(data_all, variable = change_fac_lev(variable, levels = group_order))
    data_all$variable <- change_fac_lev(data_all$variable, group_order)
  }
  attributes(data_all)$pre_data <- TRUE
  return(data_all)
}

#' Plot a stack plot
#'
#'
#' @param otutab otutab
#' @param metadata metadata
#' @param group one group name of columns of metadata
#' @param get_data just get the formatted data?
#' @param bar_params parameters parse to \code{\link[ggplot2]{geom_bar}}
#' @param topN plot how many top species
#' @param others should plot others?
#' @param relative transfer to relative or absolute
#' @param legend_title fill legend_title
#' @param stack_order the order of stack fill
#' @param group_order the order of x group
#' @param facet_order the order of the facet
#' @param style "group" or "sample"
#' @param flow should plot a flow plot?
#' @param flow_params parameters parse to \code{\link[ggalluvial]{geom_flow}}
#' @param number show the number?
#' @param format_params parameters parse to \code{\link[base]{format}}
#' @param text_params parameters parse to \code{\link[ggplot2]{geom_text}}
#' @param repel use the ggrepel::geom_text_repel instead of geom_text
#'
#' @import ggplot2
#' @export
#' @return a ggplot
#' @examples
#' data(otutab)
#' stackplot(otutab, metadata, group = "Group")
#' \donttest{
#' if (interactive()) {
#'   stackplot(otutab, metadata,
#'     group = "Group", style = "sample",
#'     group_order = TRUE, flow = TRUE, relative = FALSE
#'   )
#' }
#' }
stackplot <- function(otutab, metadata = NULL, group = "Group", get_data = FALSE,
                      bar_params = list(width = 0.7, position = "stack"),
                      topN = 8, others = TRUE, relative = TRUE, legend_title = "",
                      stack_order = TRUE, group_order = FALSE, facet_order = FALSE,
                      style = c("group", "sample")[1],
                      flow = FALSE, flow_params = list(lode.guidance = "frontback", color = "darkgray"),
                      number = FALSE, repel = FALSE, format_params = list(digits = 2),
                      text_params = list(position = position_stack())) {
  # Used to draw species stacking diagrams, suitable for processing various OTU similar data, input metatab as the basis for grouping.
  # style can choose "group" or "sample"
  # others=TRUE is used to choose whether to draw other than TopN
  # pmode can choose fill/stack/dodge
  variable <- value <- Taxonomy <- Taxonomy <- NULL

  if (is.null(attr(otutab, "pre_data"))) attr(otutab, "pre_data") <- FALSE
  if (attr(otutab, "pre_data")) {
    data_all <- otutab
  } else {
    data_all <- pre_stack_data(otutab, metadata, group, topN, others, relative, stack_order, group_order, facet_order, style)
  }
  if (get_data) {
    return(data_all)
  }

  # plot
  bar_params <- update_param(list(width = 0.7, position = "stack"), bar_params)
  flow_params <- update_param(list(lode.guidance = "frontback", color = "darkgray"), flow_params)
  format_params <- update_param(list(digits = 2), format_params)
  text_params <- update_param(list(position = position_stack()), text_params)

  if (style == "sample") {
    if (!flow) {
      p <- ggplot(data_all, aes(
        x = variable, y = value, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        # geom_bar(stat = "identity",  position = pmode) +
        do.call(geom_bar, append(list(stat = "identity"), bar_params)) +
        facet_grid(~group,
          as.table = FALSE,
          switch = "both", scales = "free", space = "free"
        )
    } else {
      lib_ps("ggalluvial", library = FALSE)
      p <- ggplot(data_all, aes(
        x = variable, y = value, alluvium = Taxonomy, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        # ggalluvial::geom_flow(stat = "alluvium", lode.guidance = "frontback", color = "darkgray") +
        do.call(ggalluvial::geom_flow, append(list(stat = "alluvium"), flow_params)) +
        ggalluvial::geom_stratum(stat = "alluvium") +
        facet_grid(~group,
          as.table = FALSE,
          switch = "both", scales = "free", space = "free"
        )
    }
    p <- p +
      theme(
        # strip.background = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)
      ) + xlab(group)
  } else {
    if (!flow) {
      p <- ggplot(data_all, aes(
        x = variable, y = value, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        do.call(geom_bar, append(list(stat = "identity"), bar_params))
    } else {
      lib_ps("ggalluvial", library = FALSE)
      p <- ggplot(data_all, aes(
        x = variable, y = value, alluvium = Taxonomy, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        do.call(ggalluvial::geom_flow, append(list(stat = "alluvium"), flow_params)) +
        ggalluvial::geom_stratum(stat = "alluvium")
    }
  }
  if (relative) {
    p <- p + scale_y_continuous(labels = scales::percent) + ylab("Relative Abundance (%)")
  } else {
    p <- p + ylab("Number")
  }

  if (number) {
    if (repel) {
      p <- p + do.call(ggrepel::geom_text_repel, (text_params))
    } else {
      p <- p + do.call(geom_text, (text_params))
    }
  }

  p + guides(fill = guide_legend(title = legend_title)) + xlab(group)
}


#' Plot a area plot
#' @rdname stackplot
#' @import ggplot2
#' @export
#' @return a ggplot
#' @examples
#' data(otutab)
#' areaplot(otutab, metadata, group = "Id")
#' \donttest{
#' areaplot(otutab, metadata,
#'   group = "Group", style = "sample",
#'   group_order = TRUE, relative = FALSE
#' )
#' }
areaplot <- function(otutab, metadata = NULL, group = "Group", get_data = FALSE,
                     bar_params = list(position = "stack"),
                     topN = 8, others = TRUE, relative = TRUE, legend_title = "",
                     stack_order = TRUE, group_order = FALSE, facet_order = FALSE,
                     style = c("group", "sample")[1],
                     number = FALSE, format_params = list(digits = 2), text_params = list(position = position_stack())) {
  variable <- value <- Taxonomy <- Taxonomy <- variable2 <- NULL

  if (is.null(attr(otutab, "pre_data"))) attr(otutab, "pre_data") <- FALSE

  if (attr(otutab, "pre_data")) {
    data_all <- otutab
  } else {
    data_all <- pre_stack_data(otutab, metadata, group, topN, others, relative, stack_order, group_order, facet_order, style)
  }
  if (get_data) {
    return(data_all)
  }
  # plot
  bar_params <- update_param(NULL, bar_params)
  format_params <- update_param(list(digits = 2), format_params)
  text_params <- update_param(list(position = position_stack()), text_params)

  # 变为数字向量
  data_all$variable2 <- as.numeric(data_all$variable)

  if (style == "sample") {
    if (TRUE) {
      p <- ggplot(data_all, aes(
        x = variable2, y = value, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        # geom_bar(stat = "identity",  position = pmode) +
        do.call(geom_area, bar_params) +
        facet_grid(~group,
          as.table = FALSE,
          switch = "both", scales = "free", space = "free"
        )
    }
    p <- p +
      theme(
        # strip.background = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)
      ) + xlab(group)
  } else {
    if (TRUE) {
      p <- ggplot(data_all, aes(
        x = variable2, y = value, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        do.call(geom_area, bar_params)
    }
  }
  # 强行加上原来的label
  p <- p + scale_x_continuous(breaks = 1:nlevels(data_all$variable), labels = levels(data_all$variable))

  if (relative) {
    p <- p + scale_y_continuous(labels = scales::percent) + ylab("Relative Abundance (%)")
  } else {
    p <- p + ylab("Number")
  }

  if (number) p <- p + do.call(geom_text, (text_params))

  p + guides(fill = guide_legend(title = legend_title)) + xlab(group)
}

#' Plot a boxplot
#'
#'
#' @param tab your dataframe
#' @param group which colname choose for group or a vector
#' @param metadata the dataframe contains the group
#' @param mode 1~9, plot style, try yourself
#' @param group_order the order of x group
#' @param facet_order the order of the facet
#' @param alpha whether plot a group alphabeta by test of method
#' @param method test method:wilcox, tukeyHSD, LSD, (default: wilcox), see \code{\link{multitest}}
#' @param alpha_param parameters parse to \code{\link[ggplot2]{geom_text}}
#' @param point_param parameters parse to \code{\link[ggplot2]{geom_jitter}}
#' @param p_value1 multi-test of all group
#' @param p_value2 two-test of each pair
#' @param stat_compare_means_param parameters parse to \code{\link[ggpubr]{stat_compare_means}}
#' @param trend_line add a trend line
#' @param trend_line_param parameters parse to \code{\link[ggplot2]{geom_smooth}}
#' @param only_sig only_sig for p_value2
#'
#' @return a ggplot
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' a <- data.frame(a = 1:18, b = runif(18, 0, 5))
#' group_box(a, group = rep(c("a", "b", "c"), each = 6))
group_box <- function(tab, group = NULL, metadata = NULL, mode = 1,
                      group_order = NULL, facet_order = NULL,
                      alpha = FALSE, method = "wilcox", alpha_param = list(color = "red"), point_param = NULL,
                      p_value1 = FALSE, p_value2 = FALSE, only_sig = TRUE, stat_compare_means_param = NULL,
                      trend_line = FALSE, trend_line_param = list(color = "blue")) {
  # data transform
  g_name <- NULL

  value <- indexes <- variable <- high <- low <- text_param <- y <- NULL
  if (is.vector(tab)) {
    tab <- data.frame(value = tab)
  } else {
    tab <- select_if(tab, is.numeric)
  }
  if ("group" %in% colnames(tab)) stop("group can not be one of colnames(tab)")

  if (is.null(metadata) && is.null(group)) {
    # a single boxplot
    md <- data.frame(tab, group = "value", check.names = FALSE)
  } else {
    if (is.null(metadata) && !is.null(group)) {
      md <- data.frame(tab, group = group, check.names = FALSE)
    } else if (!is.null(metadata) && !is.null(group)) {
      if (!all(rownames(metadata) %in% rownames(tab))) message("rownames dont match in tab and metadata")
      idx <- rownames(metadata) %in% rownames(tab)
      metadata <- metadata[idx, , drop = FALSE]
      tab <- tab[rownames(metadata), , drop = FALSE]
      md <- data.frame(tab, group = metadata[, group, drop = TRUE], check.names = FALSE)
      g_name <- group
    }
  }
  md$group <- change_fac_lev(md$group, levels = group_order)

  md %>% reshape2::melt(id.vars = "group", variable.name = "indexes") -> md
  md$indexes <- change_fac_lev(md$indexes, levels = facet_order)

  # main plot
  if (mode == 1) {
    p <- ggplot(md, aes(x = group, y = value, color = group, group = group)) +
      stat_boxplot(geom = "errorbar", width = 0.15) +
      geom_boxplot(outlier.shape = NA) +
      do.call(geom_jitter, update_param(list(width = 0.15, alpha = 0.8, size = 0.5), point_param))
  }
  if (mode == 2) {
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      # stat_boxplot(geom = "errorbar",width=0.15)+
      geom_boxplot(color = "black", outlier.shape = NA) +
      do.call(geom_jitter, update_param(list(color = "black", width = 0.15, alpha = 0.8, size = 0.5), point_param))
  }
  if (mode == 3) {
    lib_ps("gghalves", library = FALSE)
    p <- ggplot(md, aes(x = group, y = value, color = group, group = group)) +
      gghalves::geom_half_violin(aes(fill = group), side = "l", trim = FALSE) +
      geom_boxplot(
        # position = position_nudge(x = .22),
        color = "black",
        linewidth = 0.6,
        width = 0.05,
        outlier.shape = NA
      ) +
      do.call(gghalves::geom_half_point, update_param(list(side = "r", alpha = 0.8, size = 0.5), point_param))
  }
  if (mode == 4) {
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA) +
      do.call(geom_jitter, update_param(list(width = 0.15, alpha = 0.8, size = 0.5), point_param))
  }
  if (mode == 5) {
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      do.call(geom_dotplot, update_param(list(binaxis = "y", stackdir = "center", position = "dodge"), point_param)) +
      # 添加误差线
      stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "black", size = 1.2) + # 添加均值散点
      stat_summary(fun = "mean", fun.args = list(mult = 1), geom = "point", color = "white", size = 4)
  }
  if (mode == 6) {
    lib_ps("ggbeeswarm", library = FALSE)
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      do.call(ggbeeswarm::geom_quasirandom, update_param(list(width = 0.5, alpha = 0.8, size = 2, shape = 21), point_param)) +
      # 添加误差线
      stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "black", size = 1.2) +
      # 添加均值散点
      stat_summary(fun = "mean", fun.args = list(mult = 1), geom = "point", color = "white", size = 4)
  }
  if (mode == 7) {
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      # 添加柱形图
      stat_summary(fun = mean, geom = "bar", fun.args = list(mult = 1), colour = "black", fill = "white", width = .7) +
      # 添加误差线
      stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", color = "black", width = .2) +
      # 添加抖动散点图
      do.call(geom_jitter, update_param(list(width = 0.2, alpha = 0.8, size = 2, shape = 21), point_param))
  }
  if (mode == 8) {
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      # 添加柱形图
      stat_summary(fun = mean, geom = "bar", fun.args = list(mult = 1), colour = "black", width = .7) +
      # 添加误差线
      stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", color = "black", width = .2) +
      # 添加抖动散点图
      do.call(geom_jitter, update_param(list(width = 0.2, alpha = 0.8, size = 0.5), point_param))
  }
  if (mode == 9) {
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      geom_violin(trim = FALSE, width = 0.5) +
      geom_segment(aes(
        x = as.numeric(as.factor(group)) - 0.05, y = value,
        xend = as.numeric(as.factor(group)) + 0.05,
        yend = value, group = group
      ), color = "black") +
      # 添加均值,作为一条宽度为0.3的黑线
      stat_summary(
        fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
        width = 0.55, color = "black", linewidth = 1
      )
  }

  p <- p + guides(color = guide_legend(g_name), fill = guide_legend(g_name)) +
    ylab(label = NULL) + xlab(label = NULL)

  # trend line
  if (trend_line) {
    p <- p + do.call(geom_smooth, update_param(
      list(mapping = aes(group = 1), method = "glm", se = FALSE, alpha = 0.8),
      trend_line_param
    ))
  }

  # facet?
  flag <- (ncol(tab) == 1)
  if (!flag) {
    p <- p + facet_wrap(. ~ indexes, scales = "free_y")
  } else {
    ylab <- colnames(tab)[1]
    p <- p + ylab(ylab)
  }

  # p-value?
  if (is.character(p_value1) | p_value1 == TRUE) {
    lib_ps("ggpubr", library = FALSE)
    if (p_value1 == TRUE) p_value1 <- NULL
    md %>% summarise(low = min(value, na.rm = TRUE), high = max(value, na.rm = TRUE)) -> aa
    #    p <- p + ggpubr::stat_compare_means(show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1)
    p <- p + do.call(ggpubr::stat_compare_means, update_param(list(
      show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1
    ), stat_compare_means_param))
  }

  # only_sig displays only significant pairwise p-values
  if (is.character(p_value2) | p_value2 == TRUE) {
    lib_ps("ggpubr", library = FALSE)
    if (p_value2 == TRUE) p_value2 <- "wilcox"
    if (!flag) {
      if (only_sig) {
        only_sig <- FALSE
        warning("`only_sig` cannot be used when facet, we set `only_sig=FALSE`. \n  please use `cowplot` package for combining each facet plot with `only_sig=TRUE`.")
      }
    }
    if (!only_sig) {
      comparisons <- utils::combn(levels(md$group), 2) %>% split(col(.))
    } else {
      if (flag) {
        aa <- multitest(md$value, md$group, return = p_value2) %>% cbind(., indexes = colnames(tab))
        comparisons <- list()
        for (i in 1:nrow(aa)) {
          for (j in i:nrow(aa)) {
            if (length(intersect(strsplit(aa[i, "groups"], "")[[1]], strsplit(aa[j, "groups"], "")[[1]])) == 0) {
              comparisons <- append(comparisons, list(c(i, j)))
            }
          }
        }
      }
    }
    p <- p + do.call(ggpubr::stat_compare_means, update_param(list(
      show.legend = FALSE, method = p_value2, comparisons = comparisons
    ), stat_compare_means_param))
  }

  if (alpha) {
    a <- list()
    for (i in colnames(tab)) {
      filter(md, indexes == !!i) -> tmp
      a[[i]] <- multitest(tmp$value, tmp$group, return = method) %>% cbind(., indexes = i)
    }
    do.call(rbind, a) -> aa
    md %>%
      group_by(indexes) %>%
      summarise(low = min(value), high = max(value)) %>%
      left_join(aa, ., "indexes") -> aa
    aa$indexes <- factor(aa$indexes, levels = colnames(tab))
    if (mode %in% c(3, 4)) {
      p <- p + do.call(geom_text, update_param(
        list(
          data = aa, mapping = aes(x = variable, y = (high + 0.15 * (high - low)), label = groups),
          inherit.aes = FALSE, color = "red", size = 5, position = position_nudge(x = .1)
        ),
        alpha_param
      ))
    } else {
      p <- p + do.call(geom_text, update_param(
        list(
          data = aa, mapping = aes(x = variable, y = (high + 0.05 * (high - low)), label = groups),
          inherit.aes = FALSE, color = "red", size = 5
        ),
        alpha_param
      ))
    }
  }

  return(p)
}


#' Plot a doughnut chart
#'
#' @param tab two columns: first is type, second is number
#' @param reorder reorder by number?
#' @param mode plot style, 1~3
#' @param topN plot how many top items
#' @param name label the name
#' @param percentage label the percentage
#' @param text_params parameters parse to \code{\link[ggplot2]{geom_text}}
#' @param text_params2 parameters parse to \code{\link[ggplot2]{geom_text}}, for name=TRUE & mode=1,3
#' @param bar_params parameters parse to \code{\link[ggplot2]{geom_rect}}, for mode=1,3 or \code{\link[ggplot2]{geom_col}} for mode=2.
#'
#' @import ggplot2 dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#' a <- data.frame(type = letters[1:6], num = c(1, 3, 3, 4, 5, 10))
#' gghuan(a) + scale_fill_pc()
#' gghuan(a,
#'   bar_params = list(col = "black"),
#'   text_params = list(col = "#b15928", size = 3),
#'   text_params2 = list(col = "#006d2c", size = 5)
#' ) + scale_fill_pc()
#' gghuan(a, mode = 2) + scale_fill_pc()
#' gghuan(a, mode = 3) + scale_fill_pc()
gghuan <- function(tab, reorder = TRUE, mode = "1", topN = 5, name = TRUE, percentage = TRUE,
                   bar_params = NULL, text_params = NULL, text_params2 = NULL) {
  type <- ymax <- ymin <- rate_per <- fraction <- NULL
  if (ncol(tab) > 2) stop("need two columns: first is type, second is number")

  colnames(tab)[1] -> g_name
  colnames(tab) <- c("type", "n")

  plot_df <- tab %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(sum = sum(n))

  if (reorder) {
    plot_df$type <- stats::reorder(plot_df$type, plot_df$sum)
    plot_df <- dplyr::arrange(plot_df, -sum)
  }

  if (nrow(plot_df) > topN) {
    plot_df <- rbind(
      head(plot_df, topN),
      data.frame(
        type = "others",
        sum = sum(plot_df$sum[(topN + 1):nrow(plot_df)])
      )
    )

    plot_df$type <- stats::relevel(factor(plot_df$type), "others")
  }
  dplyr::mutate(plot_df, fraction = sum / sum(sum)) -> plot_df

  plot_df$ymax <- cumsum(plot_df$fraction)
  plot_df$ymin <- c(0, head(plot_df$ymax, n = -1))
  if (percentage) {
    plot_df$rate_per <- paste(as.character(round(100 * plot_df$fraction, 1)), "%", sep = "")
  } else {
    plot_df$rate_per <- plot_df$sum
  }

  if (mode == "1") {
    plt <- ggplot(data = plot_df, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 3.2, xmin = 1.7)) +
      do.call(geom_rect, update_param(list(alpha = 0.8), bar_params)) +
      xlim(c(0, 5)) +
      coord_polar(theta = "y") +
      do.call(geom_text, update_param(list(mapping = aes(x = 2.5, y = ((ymin + ymax) / 2), label = rate_per), size = 3.6, col = "white"), text_params))

    if (name) plt <- plt + do.call(geom_text, update_param(list(mapping = aes(x = 3.6, y = ((ymin + ymax) / 2), label = type), size = 4), text_params2))
  }
  if (mode == "2") {
    plt <- ggplot(plot_df, aes(x = type, y = fraction, fill = type)) +
      do.call(geom_col, update_param(list(position = "dodge2", show.legend = TRUE, alpha = .9), bar_params)) +
      coord_polar(theta = "x") +
      ylim(-min(plot_df$fraction), max(plot_df$fraction)) +
      do.call(geom_text, update_param(list(mapping = aes(
        x = type, y = fraction,
        label = paste0(type, ": ", rate_per)
      ), size = 4), text_params))
  }
  if (mode == 3) {
    plt <- ggplot(data = plot_df, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 3.2, xmin = 1.7)) +
      do.call(geom_rect, update_param(list(alpha = 0.8), bar_params)) +
      xlim(c(1.7, 3.5)) +
      coord_polar(theta = "y") +
      do.call(geom_text, update_param(list(mapping = aes(x = 2.8, y = ((ymin + ymax) / 2), label = rate_per), size = 3.6, col = "white"), text_params))

    if (name) plt <- plt + do.call(geom_text, update_param(list(mapping = aes(x = 3.4, y = ((ymin + ymax) / 2), label = type), size = 4), text_params2))
  }

  plt <- plt + theme_light() +
    labs(x = "", y = "", fill = g_name) +
    theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(panel.border = element_blank(), legend.position = "none")
  return(plt)
}


#' gghuan2 for multi-doughnut chart
#'
#' @param tab a dataframe with hierarchical structure
#' @param space_width the space width between doughnuts (0~1).
#' @param name label the name
#' @param percentage label the percentage
#' @param text_params parameters parse to \code{\link[ggplot2]{geom_text}}
#' @param bar_params parameters parse to \code{\link[ggplot2]{geom_rect}}
#' @param huan_width the huan width (numeric vector)
#' @param circle_width the center circle width
#' @param circle_label the center circle label
#' @param circle_label_params parameters parse to \code{\link[ggplot2]{geom_text}}
#'
#' @import ggplot2 dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#' data.frame(
#'   a = c("a", "a", "b", "b", "c"), b = c("a", LETTERS[2:5]), c = rep("a", 5),
#'   number = 1:5
#' ) %>% gghuan2()
gghuan2 <- function(tab = NULL, huan_width = 1, circle_width = 1, space_width = 0.2, circle_label = NULL,
                    name = TRUE, percentage = FALSE, text_params = NULL, circle_label_params = NULL, bar_params = NULL) {
  if (!is.numeric(tab[, ncol(tab)])) stop("the last column must be numeric")
  if ((space_width < 0) | space_width >= 1) stop("space_width should be [0,1)")
  type <- ymax <- ymin <- xmin <- xmax <- lab <- fraction <- NULL

  huan_widths <- c(circle_width, rep(huan_width, length = ncol(tab) - 1))
  plot_df_res <- data.frame()
  for (i in seq_len(ncol(tab) - 1)) {
    plot_df <- tab[, c(i, ncol(tab))]
    colnames(plot_df) <- c("type", "n")
    count2(plot_df) -> plot_df
    dplyr::mutate(plot_df, fraction = n / sum(n)) -> plot_df
    plot_df$ymax <- cumsum(plot_df$fraction)
    plot_df$ymin <- c(0, head(plot_df$ymax, n = -1))
    plot_df$xmax <- sum(huan_widths[seq_len(i + 1)])
    plot_df$xmin <- sum(huan_widths[seq_len(i)]) + space_width

    plot_df$lab <- ""
    if (percentage) {
      plot_df$lab <- paste0(as.character(round(100 * plot_df$fraction, 1)), "%", plot_df$lab)
    } else {
      plot_df$lab <- plot_df$n
    }

    if (name) plot_df$lab <- paste0(plot_df$type, "\n", plot_df$lab)

    plot_df_res <- rbind(plot_df_res, plot_df)
  }

  ggplot(data = plot_df_res, aes(fill = type, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)) +
    do.call(geom_rect, update_param(list(alpha = 0.8), bar_params)) +
    xlim(c(0, sum(huan_widths) + 1)) +
    coord_polar(theta = "y") +
    do.call(geom_text, update_param(list(
      mapping = aes(x = ((xmin + xmax) / 2) + 1, y = ((ymin + ymax) / 2), label = lab),
      size = 3, nudge_x = -1
    ), text_params)) +
    do.call(annotate, update_param(list(geom = "text", x = 0, y = 0, label = circle_label), circle_label_params)) +
    theme_light() +
    labs(x = "", y = "", fill = "") +
    theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(panel.border = element_blank(), legend.position = "none")
}


#' gg Histogram
#'
#' @param x vector
#' @param ... parameters parse to \code{\link[ggpubr]{gghistogram}}
#'
#' @return ggplot
#' @export
#'
#' @examples
#' if (requireNamespace("ggpubr")) {
#'   gghist(rnorm(100))
#' }
gghist <- function(x, ...) {
  lib_ps("ggpubr")
  p <- do.call(ggpubr::gghistogram, update_param(list(data = x, fill = "skyblue2", add = "median", add_density = TRUE), list(...)))

  # p <- ggplot()+
  #     geom_histogram(aes(x = x,y = after_stat(density)), fill = "skyblue2", color = "black", binwidth = 0.5)+
  #     geom_vline(aes(xintercept = median(x)), color = "red", linetype = "dashed", size = 1)+
  #     geom_density(aes(x = x), fill = NA)

  a <- round(summary(x), 2)
  lims <- ggplot_lim(p)
  p + annotate("text",
    x = 0.8 * lims$x[2] + 0.2 * lims$x[1], y = 0.8 * lims$y[2] + 0.2 * lims$y[1],
    label = paste0("Min: ", a[1], "\nMedian: ", a[3], "\nMean: ", a[4], "\nMax: ", a[6])
  )
}


#' Fit a linear model and plot
#'
#' @param tab your dataframe
#' @param var which colname choose for var or a vector
#' @param metadata the dataframe contains the var
#' @param lm_color "red"
#' @param ... parameters parse to \code{\link[ggplot2]{geom_point}}
#'
#' @return a ggplot
#' @export
#' @import ggplot2 dplyr
#' @examples
#' \donttest{
#' if (requireNamespace("ggpmisc")) {
#'   my_lm(runif(50), var = 1:50)
#'   my_lm(c(1:50) + runif(50, 0, 5), var = 1:50)
#' }
#' }
my_lm <- function(tab, var, metadata = NULL, lm_color = "red", ...) {
  lib_ps("ggpmisc", library = FALSE)
  # data transform
  g_name <- NULL
  value <- eq.label <- adj.rr.label <- p.value.label <- NULL
  if (is.vector(tab)) tab <- data.frame(value = tab)

  if (is.null(metadata)) {
    md <- data.frame(tab, var = var, check.names = FALSE)
  } else if (!is.null(metadata)) {
    if (!all(rownames(metadata) %in% rownames(tab))) message("rownames dont match in tab and metadata")
    idx <- rownames(metadata) %in% rownames(tab)
    metadata <- metadata[idx, , drop = FALSE]
    tab <- tab[rownames(metadata), , drop = FALSE]
    md <- data.frame(tab, var = metadata[, var, drop = TRUE], check.names = FALSE)
    g_name <- var
  }

  if (!all(apply(md, 2, is.numeric))) stop("need numeric")
  md %>% reshape2::melt(., id.vars = "var", variable.name = "indexes") -> md
  md$indexes <- factor(md$indexes, levels = colnames(tab))

  # main plot
  p <- ggplot(md, aes(var, value)) +
    geom_point(...) +
    geom_smooth(method = "lm", color = lm_color, se = FALSE, formula = "y~x") +
    labs(x = NULL, y = NULL) +
    ggpmisc::stat_poly_eq(
      aes(label = paste(after_stat(eq.label),
        after_stat(adj.rr.label),
        after_stat(p.value.label),
        sep = "~~~~~"
      )),
      formula = y ~ x, parse = TRUE, color = lm_color,
      size = 3, # Formula font size
      label.x = 0.05, label.y = 1.05
    )

  # facet?
  flag <- (ncol(tab) == 1)
  if (!flag) {
    p <- p + facet_wrap(. ~ indexes, scales = "free_y")
  } else {
    ylab <- colnames(tab)[1]
    p <- p + ylab(ylab)
  }

  p <- p + xlab(g_name)
  return(p)
}


# https://cloud.tencent.com/developer/article/1751856

#' Plot china map
#'
#' @param china_shp china.json file
#' @param download_dir download_dir, "pcutils_temp"
#'
#' @return a ggplot
#' @export
#' @import ggplot2
china_map <- function(china_shp = NULL, download_dir = "pcutils_temp") {
  name <- NULL
  lib_ps("ggspatial", "sf", library = FALSE)

  if (is.null(china_shp)) {
    china_shp <- file.path(download_dir, "china.json")
    china_shp_url <- "https://asa12138.github.io/FileList/china.json"
    download2(china_shp_url, china_shp)
  }

  if (!file.exists(china_shp)) stop("china_shp don't exsit.")

  china <- sf::read_sf(china_shp)

  ggplot() +
    geom_sf(
      data = china, fill = pcutils::get_cols(35, pal = "col3"),
      alpha = 0.8, linewidth = 0.5, color = "black"
    ) +
    geom_sf_text(data = china, aes(label = name), size = 3, family = "STKaiti") +
    # spatial-aware automagic north arrow
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(
      location = "tl", which_north = "false",
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    theme(
      # aspect.ratio = 1.25,
      # axis.text = element_blank(),
      # axis.ticks = element_blank(),
      axis.title = element_blank(),
      # panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey10", linetype = 1, linewidth = 1),
      plot.margin = unit(c(0, 0, 0, 0), "mm")
    )
}

#' Plot the sampling map
#'
#' @param metadata metadata must contains  "Longitude","Latitude"
#' @param group one column name of metadata which mapping to point color
#' @param label one column name of metadata which mapping to point label
#' @param point_params parameters parse to geom_point
#' @param mode 1~3. 1 use basic data from ggplot2. 2 use a shp_file. 3 use the leaflet.
#' @param shp_file a geojson file parse to `sf::read_sf`
#' @param crs crs coordinate: \code{https://asa-blog.netlify.app/p/r-map/#crs}
#' @param xlim xlim
#' @param ylim ylim
#' @param map_params parameters parse to geom_polygon (mode=1) or geom_sf (mode=2)
#' @param add_scale add annotation_scale
#' @param scale_params parameters parse to `ggspatial::annotation_scale`
#' @param add_north_arrow add annotation_north_arrow
#' @param north_arrow_params parameters parse to `ggspatial::annotation_north_arrow`
#' @param label_params parameters parse to geom_sf_text
#'
#' @return map
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' anno_df <- metadata[, c("Id", "long", "lat", "Group")]
#' colnames(anno_df) <- c("Id", "Longitude", "Latitude", "Group")
#' if (requireNamespace("ggspatial")) {
#'   sample_map(anno_df, mode = 1, group = "Group", xlim = c(90, 135), ylim = c(20, 50))
#' }
#' }
sample_map <- function(metadata, mode = 1, map_params = list(),
                       group = NULL, point_params = list(),
                       label = NULL, label_params = list(),
                       shp_file = NULL, crs = 4326, xlim = NULL, ylim = NULL,
                       add_scale = TRUE, scale_params = list(),
                       add_north_arrow = TRUE, north_arrow_params = list()) {
  long <- lat <- Longitude <- Latitude <- Group <- df2_sf <- NULL

  metadata <- data.frame(metadata, check.names = FALSE)
  if (is.null(group)) {
    metadata$Group <- "Sample"
  } else {
    metadata$Group <- metadata[, group]
  }

  # interactive map
  if (mode == 3) {
    lib_ps("leaflet", "htmltools", library = FALSE)
    # Prepare the text for the tooltip (HTML style):
    if (!is.null(label)) {
      gre_text <- paste(
        label, ": ", metadata[, label], "<br/>",
        "Group : ", metadata$Group, "<br/>",
        "Longitude : ", metadata$Longitude, "<br/>",
        "Latitude : ", metadata$Latitude
      )
    } else {
      gre_text <- paste(
        "Group: ", metadata$Group, "<br/>",
        "Longitude: ", metadata$Longitude, "<br/>",
        "Latitude: ", metadata$Latitude
      )
    }
    gre_text <- gre_text %>%
      lapply(htmltools::HTML)

    if (is.numeric(metadata$Group)) {
      type_col <- leaflet::colorNumeric(palette = get_cols(pal = "bluered"), domain = metadata$Group)
    } else {
      type_col <- leaflet::colorFactor(palette = get_cols(nlevels(factor(metadata$Group))), domain = metadata$Group)
    }

    if (is.null(xlim)) xlim <- range(metadata$Longitude)
    if (is.null(ylim)) ylim <- range(metadata$Latitude)
    Longitude_m <- mean(xlim)
    Latitude_m <- mean(ylim)
    zoom <- ceiling(20 / diff(xlim)) + 1

    color <- fillOpacity <- radius <- weight <- NULL

    if (length(point_params) > 0) {
      color <- point_params[["color"]]
      fillOpacity <- point_params[["alpha"]]
      radius <- point_params[["size"]]
      weight <- point_params[["alpha"]]
    }
    {
      if (is.null(color)) color <- "black"
      if (is.null(fillOpacity)) fillOpacity <- 0.7
      if (is.null(radius)) radius <- 8
      if (is.null(weight)) weight <- 1
    }
    inter_p <- leaflet::leaflet(metadata) %>%
      # 添加图层
      leaflet::addTiles() %>%
      # 确定中心点
      leaflet::setView(lng = Longitude_m, lat = Latitude_m, zoom = zoom) %>%
      # 添加散点注释
      leaflet::addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, fillColor = ~ type_col(Group),
        fillOpacity = fillOpacity, color = color, radius = radius, weight = weight,
        label = gre_text,
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px", direction = "auto"
        )
      ) %>%
      # 添加图例
      leaflet::addLegend(pal = type_col, values = ~Group, title = group, position = "bottomright")
    return(inter_p)
  }

  # ggplot map
  if (mode == 1) {
    lib_ps("ggspatial", library = FALSE)
    world_map <- ggplot2::map_data("world")
    p <- ggplot() +
      do.call(geom_polygon, update_param(list(
        data = world_map, mapping = aes(x = long, y = lat, group = group),
        fill = NA, color = "black"
      ), map_params)) +
      do.call(ggspatial::geom_spatial_point, update_param(
        list(data = metadata, mapping = aes(x = Longitude, y = Latitude, color = Group), crs = 4326),
        point_params
      )) +
      guides(color = guide_legend(title = group))

    if (!is.null(label)) {
      metadata$label <- metadata[, label]
      p <- p + do.call(
        ggspatial::geom_spatial_text,
        update_param(
          list(data = metadata, mapping = aes(x = Longitude, y = Latitude, label = label), crs = 4326),
          label_params
        )
      )
    }
  }
  if (mode == 2) {
    lib_ps("sf", library = FALSE)
    if (is.null(shp_file)) stop("mode 2 need shp_file")
    # shp_file="~/database/china.json"
    mapdata <- sf::read_sf(shp_file)

    if (!is.null(label)) {
      metadata$label <- metadata[, label]
    }
    anno_sf <- sf::st_as_sf(metadata, coords = c("Longitude", "Latitude"), crs = 4326)

    p <- ggplot() +
      do.call(geom_sf, update_param(list(data = mapdata, fill = NA, color = "black"), map_params)) +
      do.call(geom_sf, update_param(
        list(data = anno_sf, mapping = aes(fill = Group), shape = 21, colour = "black", stroke = .25),
        point_params
      )) +
      guides(fill = guide_legend(title = group))

    if (!is.null(label)) {
      p <- p + do.call(geom_sf_text, update_param(list(data = anno_sf, mapping = aes(label = label), size = 2.5, check_overlap = TRUE), label_params))
    }

    p <- p + coord_sf(crs = crs, xlim = xlim, ylim = ylim) +
      labs(x = "Longitude", y = "Latitude")

    if ((crs != 4326) & (!is.null(xlim) | !is.null(ylim))) {
      xlim1 <- xlim
      ylim1 <- ylim
      if (is.null(xlim)) xlim1 <- mean(metadata$Longitude)
      if (is.null(ylim)) ylim1 <- mean(metadata$Latitude)
      # 正确方法是先转换crs，使用转换后的数据
      data.frame(Longitude = xlim1, Latitude = ylim1) %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        sf::st_transform(df2_sf, crs = crs) -> tranlim
      if (!is.null(xlim)) xlim <- sf::st_coordinates(tranlim)[, "X"]
      if (!is.null(ylim)) ylim <- sf::st_coordinates(tranlim)[, "Y"]
      p <- p + coord_sf(crs = crs, xlim = xlim, ylim = ylim)
    }
  }

  # add scale and north_arrow
  if (add_scale) {
    lib_ps("ggspatial", library = FALSE)
    p <- p + do.call(ggspatial::annotation_scale, update_param(list(location = "bl"), scale_params))
  }
  if (add_north_arrow) {
    lib_ps("ggspatial", library = FALSE)
    p <- p + do.call(ggspatial::annotation_north_arrow, update_param(list(
      location = "tr", which_north = "false",
      style = ggspatial::north_arrow_fancy_orienteering
    ), north_arrow_params))
  }

  return(p)
}

#' Pie plot
#'
#' @param otutab otutab
#' @param topN topN
#' @param ... add
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' tax_pie(otutab, topN = 7) + scale_fill_pc()
#' }
tax_pie <- function(otutab, topN = 6, ...) {
  if (is.vector(otutab)) {
    otutab -> a
    if (!is.null(names(a))) names(a) <- seq_along(a)
  } else {
    rowSums(otutab) -> a
  }

  df <- data.frame(labels = names(a), va = a)
  gghuan(df, mode = 3, topN = topN, ...)
}


#' My Sunburst plot
#'
#' @param test a dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[plotly]{plot_ly}}
#' @return htmlwidget
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy, num = rowSums(otutab))[1:10, ] -> test
#' if (requireNamespace("plotly")) {
#'   my_sunburst(test)
#' }
#' }
my_sunburst <- function(test, ...) {
  test <- as.data.frame(test)
  if (length(unique(test[, 1])) > 1) {
    test <- cbind("Root" = " ", test)
  }
  nc <- ncol(test)
  if (nc < 3) stop("as least 3-columns dataframe")
  if (!is.numeric(test[, nc])) stop("the last column must be numeric")

  lib_ps("plotly", library = FALSE)
  target <- source <- weight <- NULL
  # change duplicated data

  # for (i in 1:(nc-1)){
  #   test[,i]=paste0(test[,i],strrep(" ",i-1))
  # }

  # merge to two columns
  links <- data.frame()
  for (i in 1:(nc - 2)) {
    test[, c(i, i + 1, nc)] -> tmp
    colnames(tmp) <- c("source", "target", "weight")
    tmp <- dplyr::group_by(tmp, source, target) %>% dplyr::summarise(weight = sum(weight), .groups = "keep")
    links <- rbind(links, tmp)
  }
  fig <- plotly::plot_ly(
    # 定义所有级别各类的标签
    labels = links$target,
    # 定义所有级别各类的父级，与上面定义的标签一一对应
    parents = links$source,
    # 定义各分类的值（一一对应）
    values = links$weight,
    text = links$weight,
    # 指定图表类型：sunburst
    type = "sunburst", ...
  )
  fig
}


#' My Treemap plot
#'
#' @param test a three-columns dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[plotly]{plot_ly}}
#'
#' @return htmlwidget
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy, num = rowSums(otutab))[1:10, c(4, 7, 8)] -> test
#' if (requireNamespace("treemap")) {
#'   my_treemap(test)
#' }
#' }
my_treemap <- function(test, ...) {
  test <- as.data.frame(test)
  # if(length(unique(test[,1]))>1){
  #   test=cbind("Root"=" ",test)
  # }
  nc <- ncol(test)
  if (nc != 3) stop("supports 3-columns dataframe")
  if (!is.numeric(test[, nc])) stop("the last column must be numeric")

  lib_ps("treemap", library = FALSE)
  target <- source <- weight <- NULL
  # change duplicated data

  # for (i in 1:(nc-1)){
  #   test[,i]=paste0(test[,i],strrep(" ",i-1))
  # }

  # merge to two columns
  links <- data.frame()
  for (i in 1:(nc - 2)) {
    test[, c(i, i + 1, nc)] -> tmp
    colnames(tmp) <- c("source", "target", "weight")
    tmp <- dplyr::group_by(tmp, source, target) %>% dplyr::summarise(weight = sum(weight), .groups = "keep")
    links <- rbind(links, tmp)
  }
  fig <- treemap::treemap(
    dtf = links,
    # 定义所有级别各类的标签
    index = c("source", "target"),
    # 定义各分类的值（一一对应）
    vSize = "weight", type = "index"
  )
  # if (d3) {
  #     lib_ps("d3treeR", library = FALSE)
  #     fig <- d3treeR::d3tree2(fig, rootname = colnames(test)[1])
  #     fig
  # }
}

#' My Voronoi treemap plot
#'
#' @param test a three-columns dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[voronoiTreemap]{vt_d3}}
#'
#' @return htmlwidget
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy, num = rowSums(otutab))[1:10, c(4, 7, 8)] -> test
#' if (requireNamespace("voronoiTreemap")) {
#'   my_voronoi_treemap(test)
#' }
#' }
my_voronoi_treemap <- function(test, ...) {
  test <- as.data.frame(test)
  nc <- ncol(test)
  if (nc != 3) stop("supports 3-columns dataframe")
  if (!is.numeric(test[, nc])) stop("the last column must be numeric")

  lib_ps("voronoiTreemap", library = FALSE)

  pal <- setNames(get_cols(length(unique(test[, 1]))), unique(test[, 1]))
  plotdat <- data.frame(
    h1 = "Total", h2 = test[, 1], h3 = test[, 2], color = pal[test[, 1]],
    weight = test[, 3], codes = test[, 2]
  )

  gdp_json <- voronoiTreemap::vt_export_json(voronoiTreemap::vt_input_from_df(plotdat))
  voronoiTreemap::vt_d3(gdp_json, ...)
}


#' My circo plot
#
#' @param df dataframe with three column
#' @param reorder reorder by number?
#' @param pal a vector of colors, you can get from here too: `RColorBrewer::brewer.pal(5,"Set2")` or `ggsci::pal_aaas()(5)`
#' @param mode "circlize","chorddiag"
#' @param ... \code{\link[circlize]{chordDiagram}}
#'
#' @return chordDiagram
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("circlize")) {
#'   data.frame(
#'     a = c("a", "a", "b", "b", "c"),
#'     b = c("a", LETTERS[2:5]), c = 1:5
#'   ) %>% my_circo(mode = "circlize")
#'   data(otutab)
#'   cbind(taxonomy, num = rowSums(otutab))[1:10, c(2, 6, 8)] -> test
#'   my_circo(test)
#' }
#' }
#'
my_circo <- function(df, reorder = TRUE, pal = NULL, mode = c("circlize", "chorddiag")[1], ...) {
  mode <- match.arg(mode, c("circlize", "chorddiag"))
  colnames(df) <- c("from", "to", "count")
  if (mode == "chorddiag") {
    # need a square matrix
    all_g <- unique(df$from, df$to)
    expand.grid(all_g, all_g) -> tab
    df <- left_join(tab, df, by = c("Var1" = "from", "Var2" = "to"))
    colnames(df) <- c("from", "to", "count")
  }

  tab <- reshape2::dcast(df, from ~ to, value.var = "count") %>%
    tibble::column_to_rownames("from") %>%
    as.matrix()
  tab[is.na(tab)] <- 0

  if (reorder) {
    colSums(tab) %>%
      sort(decreasing = TRUE) %>%
      names() -> s_name
    tab <- tab[, s_name]
    rowSums(tab) %>%
      sort(decreasing = TRUE) %>%
      names() -> s_name
    tab <- tab[s_name, ]
  }

  if (is.null(pal)) {
    pal <- get_cols(length(unique(c(colnames(tab), rownames(tab)))))
  } else if (is.null(names(pal))) pal <- rep(pal, length.out = length(unique(c(colnames(tab), rownames(tab)))))

  if (mode == "circlize") {
    lib_ps("circlize", library = FALSE)
    circlize::chordDiagram(tab, grid.col = pal, ...)
  }
  # if (mode == "chorddiag") {
  #     lib_ps("chorddiag", library = FALSE)
  #     chorddiag::chorddiag(tab, groupedgeColor = pal, ...)
  # }
}


#' My Circle packing plot
#'
#' @param test a dataframe with hierarchical structure
#' @param anno annotation tablewith rowname for color or fill.
#' @param mode 1~2
#' @param Group fill for mode2
#' @param Score color for mode1
#' @param label the labels column
#' @param show_level_name show which level name? a vector contains some column names.
#' @param show_tip_label show_tip_label, logical
#' @param str_width str_width
#'
#' @return ggplot
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy, weight = rowSums(otutab))[1:10, ] -> test
#' if (requireNamespace("igraph") && requireNamespace("ggraph")) {
#'   my_circle_packing(test)
#' }
#' }
my_circle_packing <- function(test, anno = NULL, mode = 1,
                              Group = "level", Score = "weight", label = "label",
                              show_level_name = "all", show_tip_label = TRUE, str_width = 10) {
  weight <- level <- Level <- NULL
  lib_ps("igraph", "ggraph", library = FALSE)
  test <- as.data.frame(test)
  if (length(unique(test[, 1])) > 1) {
    test <- cbind("Root" = " ", test)
  }
  nc <- ncol(test)
  if (nc < 3) stop("as least 3-columns dataframe")
  if (!is.numeric(test[, nc])) stop("the last column must be numeric")
  if (any(test[, nc] < 0)) stop("the weight must be bigger than 0.")

  link <- df2link(test, fun = sum)
  nodes <- link$nodes
  links <- link$links
  ttt <- igraph::graph_from_data_frame(d = as.data.frame(links), vertices = nodes)

  tmp_v <- as.data.frame(igraph::vertex.attributes(ttt))
  if (!is.null(anno)) {
    if (!"name" %in% colnames(anno)) {
      anno$name <- rownames(anno)
    }
    tmp_v <- dplyr::left_join(tmp_v, anno, by = "name", suffix = c(".x", ""))
  }

  tmp_v$Level <- factor(tmp_v$level, levels = colnames(test)[-ncol(test)])
  tmp_v$label <- tmp_v$name
  tmp_v$label <- ifelse(is.na(tmp_v[, label]), tmp_v$label, tmp_v[, label])

  if (identical(show_level_name, "all")) show_level_name <- colnames(test)[seq_len(ncol(test) - 2)]
  if (show_tip_label) show_level_name <- c(show_level_name, colnames(test)[ncol(test) - 1])
  tmp_v$label <- ifelse(tmp_v$level %in% show_level_name, tmp_v$label, NA)

  tmp_v$Group <- ifelse(tmp_v$level == colnames(test)[ncol(test) - 1], tmp_v[, Group], NA)
  tmp_v$Score <- ifelse(tmp_v$level == colnames(test)[ncol(test) - 1], tmp_v[, Score], NA)

  as.list(tmp_v) -> igraph::vertex.attributes(ttt)

  if (mode == 1) {
    p <- ggraph::ggraph(ttt, layout = "circlepack", weight = weight) +
      ggraph::geom_node_circle(aes(fill = Score)) +
      scale_fill_continuous(na.value = NA)
  }
  if (mode == 2) {
    p <- ggraph::ggraph(ttt, layout = "circlepack", weight = weight) +
      ggraph::geom_node_circle(aes(fill = Group)) +
      scale_fill_discrete(na.translate = FALSE)
  }
  p <- p + ggraph::geom_node_circle(aes(color = Level)) +
    ggraph::geom_node_text(aes(
      label = stringr::str_wrap(label, width = str_width), color = Level,
      size = weight
    ), show.legend = FALSE) +
    # ggraph::geom_node_text(aes(label=stringr::str_wrap(label,width = str_width),color=level,
    #                            filter=leaf,size = weight),show.legend = FALSE)+
    # ggraph::geom_node_text(aes(label=stringr::str_wrap(label,width = str_width),color=level,
    #                            filter=!leaf,size = weight),show.legend = FALSE,nudge_y = 0.5)+
    theme_void()
  p
}

# ========Easter eggs=======

#' Show my little cat named Guo Dong which drawn by my girlfriend.
#' @param mode 1~2
#' @return a ggplot
#' @export
my_cat <- function(mode = 1) {
  little_guodong <- NULL
  data("little_guodong", package = "pcutils", envir = environment())
  if (mode == 1) {
    p <- ggplot() +
      annotation_custom(little_guodong, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
  }
  if (mode == 2) {
    x <- y <- NULL
    lib_ps("ggimage", library = FALSE)
    t <- seq(0, 2 * pi, 0.08)
    d <- data.frame(x = 2 * (sin(t) - 0.5 * sin(2 * t)), y = 2 * (cos(t) - 0.5 * cos(2 * t)))

    temp <- tempdir()
    ggsave(filename = paste0(temp, "/", "little_guodong.png"), plot = little_guodong, bg = "transparent")
    p <- ggplot(d, aes(x, y)) +
      ggimage::geom_image(image = paste0(paste0(temp, "/", "little_guodong.png")), size = .05) +
      theme_void()
  }
  p
}
