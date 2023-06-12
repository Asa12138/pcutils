
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

match_df <- function(otutab, metadata) {
  if (!setequal(rownames(metadata), colnames(otutab))) message("rownames dont match in tab and metadata")
  idx <- rownames(metadata) %in% colnames(otutab)
  metadata <- metadata[idx, , drop = FALSE]
  otutab <- otutab[, rownames(metadata), drop = FALSE]
  return(list(otutab = otutab, metadata = metadata))
}

#' Plot a general venn (upset, flower)
#'
#' @param ... additional
#'
#' @return a plot
#' @export
#'
#' @examples
#' \donttest{
#' aa <- list(a = 1:3, b = 3:7, c = 2:4)
#' venn(aa, mode = "venn")
#' venn(aa, mode = "venn2", type = "ChowRuskey")
#' venn(aa, mode = "upset")
#' data(otutab)
#' venn(otutab, mode = "flower")
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
#' @param mode "venn","venn2","upset","flower"
#' @param ... add
#' @return a plot
#' @exportS3Method
venn.list <- function(aa, mode = "venn", ...) {
  if (is.null(names(aa))) names(aa) <- seq_along(aa)
  if (length(aa) > 4 && mode == "venn") message("venn < 4, recommend upset or flower")
  if (mode == "venn") {
    lib_ps("ggvenn", library = FALSE)
    ggvenn::ggvenn(aa) -> p
    return(p)
  }
  # if (mode == "venn2") {
  #   if (!requireNamespace("RBGL")) BiocManager::install("RBGL")
  #   if (!requireNamespace("graph")) BiocManager::install("graph")
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
    UpSetR::upset(UpSetR::fromList(aa), order.by = "freq", nsets = length(aa), nintersects = 30) -> p
    return(p)
  }
  if (mode == "flower") {
    lib_ps("RColorBrewer", "plotrix", library = FALSE)
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))

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

    graphics::par(bty = "n", ann = FALSE, xaxt = "n", yaxt = "n", mar = c(1, 1, 1, 1))

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
}


#' @param otutab table
#' @param mode "venn","venn2","upset","flower"
#' @param ... add
#' @return a plot
#' @method venn data.frame
#' @rdname venn
#' @exportS3Method
venn.data.frame <- function(otutab, mode = "venn", ...) {
  venn_cal(otutab) -> aa
  venn.list(aa, mode = mode)
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
#'
#' @import ggplot2
#' @export
#' @return a ggplot
#' @examples
#' data(otutab)
#' stackplot(otutab, metadata, group = "Group")
#' \donttest{
#' stackplot(otutab, metadata, group = "Group", group_order = TRUE, flow = TRUE, relative = FALSE)
#' }
stackplot <- function(otutab, metadata = NULL, group = "Group", get_data = FALSE,
                      bar_params = list(width = 0.7, position = "stack"),
                      topN = 8, others = TRUE, relative = TRUE, legend_title = "",
                      stack_order = TRUE, group_order = FALSE, facet_order = FALSE,
                      style = c("group", "sample")[1],
                      flow = FALSE, flow_params = list(lode.guidance = "frontback", color = "darkgray"),
                      number = FALSE, format_params = list(digits = 2), text_params = list(position = position_stack())) {
  # Used to draw species stacking diagrams, suitable for processing various OTU similar data, input metatab as the basis for grouping.
  # style can choose "group" or "sample"
  # others=TRUE is used to choose whether to draw other than TopN
  # pmode can choose fill/stack/dodge
  # library(ggplot2)
  # library(dplyr)
  lib_ps("reshape2", "scales", "dplyr", library = FALSE)
  variable=Taxonomy=value=NULL
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

  if (nrow(mean_sort) > topN) {
    other <- colSums(mean_sort[topN:dim(mean_sort)[1], ])
    mean_sort <- mean_sort[1:(topN - 1), ]
    mean_sort <- rbind(mean_sort, other)
    rownames(mean_sort)[topN] <- c("Other")
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
    if (facet_order == 1) {
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
  if (stack_order == 1) {
    data_all$Taxonomy <- factor(data_all$Taxonomy, levels = rownames(mean_sort))
  } else if (any(stack_order %in% data_all$Taxonomy)) {
    data_all$Taxonomy <- change_fac_lev(data_all$Taxonomy, levels = stack_order)
  }
  # determine the x axis order
  if (group_order == 1) {
    new_lev <- (data_all %>% dplyr::filter(Taxonomy == rownames(mean_sort)[1]) %>%
      dplyr::arrange(value) %>% as.data.frame())[, 1] %>% as.character()
    data_all <- dplyr::mutate(data_all, variable = factor(variable, levels = new_lev))
  } else if (group_order[1] %in% data_all$Taxonomy) {
    new_lev <- (data_all %>% dplyr::filter(Taxonomy == group_order) %>%
      dplyr::arrange(value) %>% as.data.frame())[, 1] %>% as.character()
    data_all <- dplyr::mutate(data_all, variable = factor(variable, levels = new_lev))
  } else if (any(group_order %in% data_all$variable)) {
    data_all <- dplyr::mutate(data_all, variable = change_fac_lev(variable, levels = group_order))
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

  if (number) p <- p + do.call(geom_text, (text_params))

  p + guides(fill = guide_legend(title = legend_title)) + xlab(group)
}



#' Plot a boxplot
#'
#'
#' @param tab your dataframe
#' @param group which colname choose for group or a vector
#' @param metadata the dataframe contains the group
#' @param mode 1~3, plot style
#' @param group_order the order of x group
#' @param facet_order the order of the facet
#' @param alpha whether plot a group alphabeta by test of method
#' @param method test method:wilcox, tukeyHSD, LSD, (default: wilcox), see \code{\link{multitest}}
#' @param alpha_param parameters parse to \code{\link[ggplot2]{geom_text}}
#' @param p_value1 multi-test of all group
#' @param p_value2 two-test of each pair
#' @param stat_compare_means_param parameters parse to \code{\link[ggpubr]{stat_compare_means}}
#' @param trend_line add a trend line
#' @param trend_line_param parameters parse to \code{\link[ggplot2]{geom_smooth}}
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
                      alpha = FALSE, method = "wilcox", alpha_param = list(color = "red"),
                      p_value1 = FALSE, p_value2 = FALSE, stat_compare_means_param = NULL,
                      trend_line = FALSE, trend_line_param = list(color = "blue")) {
  lib_ps("ggplot2", "dplyr", "reshape2", library = FALSE)
  # data transform
  g_name <- NULL

  value=indexes=variable=high=low=text_param=NULL
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
      geom_jitter(width = 0.15, alpha = 0.8, size = 0.5)
  }
  if (mode == 2) {
    p <- ggplot(md, aes(x = group, y = value, fill = group, group = group)) +
      # stat_boxplot(geom = "errorbar",width=0.15)+
      geom_boxplot(color = "black", outlier.shape = NA) +
      geom_jitter(color = "black", width = 0.15, alpha = 0.8, size = 0.5)
  }
  if (mode == 3) {
    lib_ps("gghalves", library = FALSE)
    p <- ggplot(md, aes(x = group, y = value, color = group, group = group)) +
      gghalves::geom_half_violin(aes(fill = group), side = "l", trim = FALSE) +
      gghalves::geom_half_point(side = "r", size = 0.5, alpha = 0.8) +
      geom_boxplot(
        position = position_nudge(x = .22),
        linewidth = 0.6,
        width = 0.2,
        outlier.shape = NA
      )
  }

  p <- p + guides(color = guide_legend(g_name), fill = guide_legend(g_name)) +
    ylab(label = NULL) + xlab(label = NULL)

  # trend line
  if (trend_line) p <- p + do.call(geom_smooth, update_param(list(mapping = aes(group = 1), method = "glm", se = FALSE, alpha = 0.8), trend_line_param))

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
    md %>% summarise(low = min(value), high = max(value)) -> aa
    #    p <- p + ggpubr::stat_compare_means(show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1)
    p <- p + do.call(ggpubr::stat_compare_means, update_param(list(
      show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1
    ), stat_compare_means_param))
  }

  if (is.character(p_value2) | p_value2 == TRUE) {
    lib_ps("ggpubr", library = FALSE)
    if (p_value2 == TRUE) p_value2 <- NULL
    comparisons <- utils::combn(levels(md$group), 2) %>% split(col(.))
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
    if (mode == 3) {
      # p <- p + geom_text(
      #   data = aa, aes(x = variable, y = (high + 0.15 * (high - low)), label = groups),
      #   inherit.aes = FALSE, color = alpha_color, size = 5, position = position_nudge(x = .1)
      # )
      p <- p + do.call(geom_text, update_param(
        list(
          data = aa, mapping = aes(x = variable, y = (high + 0.15 * (high - low)), label = groups),
          inherit.aes = FALSE, color = "red", size = 5, position = position_nudge(x = .1)
        ),
        alpha_param
      ))
    } else {
      # p <- p + geom_text(
      #   data = aa, aes(x = variable, y = (high + 0.05 * (high - low)), label = groups),
      #   inherit.aes = FALSE, color = alpha_color, size = 5
      # )
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
#'
#' @import ggplot2 dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#' a <- data.frame(type = letters[1:6], num = c(1, 3, 3, 4, 5, 10))
#' gghuan(a) + ggplot2::scale_fill_manual(values = get_cols(6, "col3"))
#' b <- data.frame(type = letters[1:12], num = c(1, 3, 3, 4, 15, 10, 35, 2:6))
#' gghuan(b) + ggplot2::theme(legend.position = "right")
gghuan <- function(tab, reorder = TRUE, mode = "1", topN = 5, name = TRUE, percentage = TRUE) {
  type=ymax=ymin=rate_per=fraction=NULL
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

  if (mode == 3) {
    lib_ps("ggpubr", library = FALSE)
    labs <- paste0(plot_df$type, "\n", plot_df$rate_per)
    p <- ggpubr::ggpie(plot_df, "fraction", label = labs, fill = "type") + theme(legend.position = "none")
    return(p)
  }

  if (mode == "1") {
    plt <- ggplot(data = plot_df, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 3.2, xmin = 1.7)) +
      geom_rect(alpha = 0.8) +
      xlim(c(0, 5)) +
      coord_polar(theta = "y") +
      geom_text(aes(x = 2.5, y = ((ymin + ymax) / 2), label = rate_per), size = 3.6, col = "white")
    if (name) plt <- plt + geom_text(aes(x = 3.6, y = ((ymin + ymax) / 2), label = type), size = 4)
  }
  if (mode == "2") {
    plt <- ggplot(plot_df, aes(x = type, y = fraction, fill = type)) +
      geom_col(position = "dodge2", show.legend = TRUE, alpha = .9) +
      coord_polar() +
      ylim(-min(plot_df$fraction), max(plot_df$fraction) + 0.2) +
      geom_text(aes(x = type, y = fraction + 0.1, label = paste0(type, "\n", rate_per)), size = 4)
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
#' @param break default 0.2
#' @param name label the name
#' @param number label the number
#' @param percentage label the percentage
#' @param text_col defalut, black
#'
#' @import ggplot2 dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#' data.frame(a = c("a", "a", "b", "b", "c"), aa = rep("a", 5),
#'      b = c("a", LETTERS[2:5]), c = 1:5) %>% gghuan2()
gghuan2 <- function(tab = NULL, `break` = 0.2, name = TRUE, number = TRUE, percentage = FALSE, text_col = "black") {
  if (!is.numeric(tab[, ncol(tab)])) stop("the last column must be numeric")
  if ((`break` < 0) | `break` >= 1) stop("`break` should be [0,1)")
  type=ymax=ymin=xmin=xmax=lab=fraction=NULL

  plot_df_res <- data.frame()
  for (i in seq_len(ncol(tab) - 1)) {
    plot_df <- tab[, c(i, ncol(tab))]
    colnames(plot_df) <- c("type", "n")
    count2(plot_df) -> plot_df
    dplyr::mutate(plot_df, fraction = n / sum(n)) -> plot_df
    plot_df$ymax <- cumsum(plot_df$fraction)
    plot_df$ymin <- c(0, head(plot_df$ymax, n = -1))
    plot_df$xmax <- i + 1
    plot_df$xmin <- i + `break`

    plot_df$lab <- ""
    if (percentage) plot_df$lab <- paste0(as.character(round(100 * plot_df$fraction, 1)), "%", plot_df$lab)
    if (number) plot_df$lab <- paste0(plot_df$n, "\n", plot_df$lab)
    if (name) plot_df$lab <- paste0(plot_df$type, "\n", plot_df$lab)

    plot_df_res <- rbind(plot_df_res, plot_df)
  }

  ggplot(data = plot_df_res, aes(fill = type, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)) +
    geom_rect(alpha = 0.8) +
    xlim(c(0, i + 2)) +
    coord_polar(theta = "y") +
    geom_text(aes(x = (xmin + xmax) / 2, y = ((ymin + ymax) / 2), label = lab), size = 3.6, col = text_col) +
    theme_light() +
    labs(x = "", y = "", fill = "") +
    theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(panel.border = element_blank(), legend.position = "none")
}


#' Fit a linear model and plot
#'
#' @param tab your dataframe
#' @param var which colname choose for var or a vector
#' @param metadata the dataframe contains the var
#' @param ... parameters parse to \code{\link[ggplot2]{geom_point}}
#'
#' @return a ggplot
#' @export
#' @import ggplot2 dplyr
#' @examples
#' \donttest{
#' my_lm(runif(50), var = 1:50)
#' my_lm(c(1:50) + runif(50, 0, 5), var = 1:50)
#' }
my_lm <- function(tab, var, metadata = NULL, ...) {
  lib_ps("reshape2", "ggpmisc", library = FALSE)
  # data transform
  g_name <- NULL
  value=eq.label=adj.rr.label=p.value.label=NULL
  if (is.vector(tab)) tab <- data.frame(value = tab)

  if (is.null(metadata)) {
    md <- data.frame(tab, var = var, check.names = FALSE)
  } else if (!is.null(metadata)) {
    if (!all(rownames(metadata) %in% rownames(tab))) message("rownames dont match in tab and metadata")
    idx <- rownames(metadata) %in% rownames(tab)
    metadata <- metadata[idx, , drop = FALSE]
    tab <- tab[rownames(metadata), , drop = FALSE]
    md <- data.frame(tab, var = metadata[, var], check.names = FALSE)
    g_name <- var
  }

  if (!all(apply(md, 2, is.numeric))) stop("need numeric")
  md %>% reshape2::melt(., id.vars = "var", variable.name = "indexes") -> md
  md$indexes <- factor(md$indexes, levels = colnames(tab))
  # main plot
  p <- ggplot(md, aes(var, value)) +
    geom_point(...) +
    geom_smooth(method = "lm", color = "red", se = FALSE, formula = "y~x") +
    ggpmisc::stat_poly_eq(
      aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), after_stat(p.value.label), sep = "~~~~~")),
      formula = y ~ x, parse = TRUE, color = "red",
      size = 3, # 公式字体大小
      label.x = 0.05, label.y = 1.05
    ) + # 位置 ，0-1之间的比例
    labs(x = NULL, y = NULL)
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
#' @param dir where to put the china.json file
#'
#' @return a ggplot
#' @export
#' @import ggplot2
china_map <- function(dir = "~/database/") {
  name=NULL
  lib_ps("ggspatial", "sf", library = FALSE)
  china_shp <- paste0(dir, "china.json")
  if (!file.exists(china_shp)) utils::download.file("https://gitcode.net/mirrors/lyhmyd1211/geomapdata_cn/-/raw/master/china.json?inline=false", china_shp)
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


#' Show my little cat named Guo Dong which drawn by my girlfriend.
#' @param mode 1~2
#' @return a ggplot
#' @export
my_cat <- function(mode = 1) {
  little_guodong=NULL
  data("little_guodong", package = "pcutils", envir = environment())
  if (mode == 1) {
    p <- ggplot() +
      annotation_custom(little_guodong, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
  }
  if (mode == 2) {
    x=y=NULL
    lib_ps("ggimage", library = FALSE)
    t <- seq(0, 2 * pi, 0.08)
    d <- data.frame(x = 2 * (sin(t) - 0.5 * sin(2 * t)), y = 2 * (cos(t) - 0.5 * cos(2 * t)))

    temp <- tempdir()
    ggsave(filename = paste0(temp, "/", "little_guodong.png"), plot = little_guodong, bg = "transparent")
    p <- ggplot(d, aes(x, y)) +
      ggimage::geom_image(image = paste0("images/smallguodong.ppp"), size = .05) +
      theme_void()
  }
  p
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
#' tax_pie(otutab,topN = 7)
#' }
tax_pie<-function(otutab,topN=6,...){
  lib_ps("ggpubr",library = FALSE)
  if(is.vector(otutab)){
    otutab->a
    if(!is.null(names(a)))names(a)=seq_along(a)
  }
  else rowSums(otutab)->a
  if(length(a)>topN){
    sort(a,decreasing = TRUE)[1:topN-1]->b
    other=sum(sort(a,decreasing = TRUE)[topN:length(a)])
    b<-c(b,other)
    names(b)[length(b)]<-'Others'}
  else b<-a

  df=data.frame(va=b,labels = paste0(names(b),"\n(",round(b/sum(b)*100,2),"%)"))
  ggpubr::ggpie(df,'va',fill=get_cols(length(b)),label = "labels",grepl=TRUE,...)
}

#' Word cloud plot
#'
#' @param str_vector string vector
#'
#' @export
#' @return a htmlwidget
#' @examples
#' \donttest{
#' data(otutab)
#' tax_wordcloud(taxonomy$Genus)
#' }
tax_wordcloud<-function(str_vector){
  lib_ps("wordcloud2",library = FALSE)
  remove_unclassfied<-\ (taxdf) {
    taxdf[grepl.data.frame("Unclassified|uncultured|Ambiguous|Unknown|unknown|metagenome|Unassig",
                           taxdf, ignore.case = TRUE)] <- NA
    return(taxdf)
  }
  sort(table(str_vector),decreasing = TRUE)[1:50]%>%as.data.frame()%>%
    remove_unclassfied()%>%stats::na.omit()%>%wordcloud2::wordcloud2(.,size=.7)
}

#' My circo plot
#
#' @param df dataframe with three column
#' @param reorder reorder by number?
#' @param pal a vector of colors, you can get from here too.{RColorBrewer::brewer.pal(5,"Set2")} {ggsci::pal_aaas()(5)}
#' @param mode "circlize","chorddiag"
#' @param ... \code{\link[circlize]{chordDiagram}}
#'
#' @return chordDiagram
#' @export
#'
#' @examples
#' \donttest{
#' data.frame(a=c("a","a","b","b","c"),b=c("a",LETTERS[2:5]),c=1:5)%>%my_circo(mode="circlize")
#' }
my_circo=function(df,reorder=TRUE,pal=NULL,mode=c("circlize","chorddiag"),...){
  mode=match.arg(mode,c("circlize","chorddiag"))
  colnames(df)=c("from","to","count")
  lib_ps("reshape2","tibble",library = FALSE)
  if(mode=="chorddiag"){
    #need a square matrix
    all_g=unique(df$from,df$to)
    expand.grid(all_g,all_g)->tab
    df=left_join(tab,df,by=c("Var1"="from","Var2"="to"))
    colnames(df)=c("from","to","count")}

  tab=reshape2::dcast(df,from~to,value.var = "count")%>%tibble::column_to_rownames("from")%>%as.matrix()
  tab[is.na(tab)]=0

  if(reorder){
    colSums(tab)%>%sort(decreasing = TRUE)%>%names()->s_name
    tab=tab[,s_name]
    rowSums(tab)%>%sort(decreasing = TRUE)%>%names()->s_name
    tab=tab[s_name,]
  }

  if(is.null(pal))pal=get_cols(length(unique(c(colnames(tab),rownames(tab)))))

  if(mode=="circlize"){
    lib_ps("circlize",library = FALSE)
    circlize::chordDiagram(tab,grid.col = pal,...)
  }
  # if(mode=="chorddiag"){
  #   lib_ps("chorddiag",library = FALSE)
  #   chorddiag::chorddiag(tab,groupedgeColor= pal,...)
  #   }
}


#=======some tips========

#' How to use parallel
#' @export
#' @return No return value
how_to_use_parallel=function(){
  message('  #parallel
  reps=100;threads=1
  #main function
  loop=function(i){
    return(mean(rnorm(100)))
  }
  {
  if(threads>1){
    pcutils::lib_ps("foreach","doSNOW","snow")
    pb <- utils::txtProgressBar(max =reps, style = 3)
    opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
    cl <- snow::makeCluster(threads)
    doSNOW::registerDoSNOW(cl)
    res <- foreach::foreach(i = 1:reps,.options.snow = opts,
                             .packages = c()) %dopar% {
                               loop(i)
                             }
    snow::stopCluster(cl)
    gc()
    pcutils::del_ps("doSNOW","snow","foreach")
  }
  else {
    res <-lapply(1:reps, loop)
  }}
  #simplify method
  res=do.call(c,res)
  pcutils::del_ps("foreach","doSNOW")
',"\n")
}

#' How to update parameters
#' @export
#' @return No return value
how_to_update_parameters=function(){
  message('point_params = list(size=5,color="red")
ggplot(data.frame(x=1:5,y=5:1), aes(x = x, y = y))+
  do.call(geom_point, update_param(list(size=2,color="blue",alpha=0.5), point_params))')
}
