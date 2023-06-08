
#' Scale a legend size
#'
#' @param scale default: 1.
#'
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
  metadata <- metadata[idx, , drop = F]
  otutab <- otutab[, rownames(metadata), drop = F]
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
#' \dontrun{
#' #aa <- list(a = 1:3, b = 3:7, c = 2:4)
#' #venn(aa, mode = "venn")
#' #venn(aa, mode = "venn2", type = "ChowRuskey")
#' #venn(aa, mode = "upset")
#' #data(otutab)
#' #venn(otutab, mode = "flower")
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
#'
#' @exportS3Method
venn.list <- function(aa, mode = "venn", ...) {
  if (is.null(names(aa))) names(aa) <- seq_along(aa)
  if (length(aa) > 4 && mode == "venn") print("venn < 4, recommend upset or flower")
  if (mode == "venn") {
    lib_ps("ggvenn", library = F)
    ggvenn::ggvenn(aa) -> p
    return(p)
  }
  if (mode == "venn2") {
    if (!requireNamespace("RBGL")) BiocManager::install("RBGL")
    if (!requireNamespace("graph")) BiocManager::install("graph")
    lib_ps("Vennerable", library = F)
    Vennerable::Venn(aa) -> aap
    Vennerable::plot(aap)
    # plot(aap,type="triangles")
    # plot(aap, doWeights = FALSE)
    # plot(aap, doWeights = FALSE,type="ellipses")
    # plot(aap, doWeights = FALSE,type="ChowRuskey")
  }
  if (mode == "upset") {
    lib_ps("UpSetR", library = F)
    UpSetR::upset(UpSetR::fromList(aa), order.by = "freq", nsets = length(aa), nintersects = 30) -> p
    return(p)
  }
  if (mode == "flower") {
    lib_ps("RColorBrewer", "plotrix", library = F)
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

    ellipse_col <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "Set3"))(n)
    start <- 90
    a <- 0.5
    b <- 2.2
    r <- 0.5
    ellipse_col <- ellipse_col
    circle_col <- "white"

    graphics::par(bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1, 1, 1, 1))

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
#'
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
#'
#' @examples
#' \dontrun{
#' #data(otutab)
#' #stackplot(otutab, metadata, group = "Group")
#' #stackplot(otutab, metadata, group = "Group", group_order = TRUE, flow = FALSE, relative = FALSE)
#' }
stackplot <- function(otutab, metadata = NULL, group = "Group", get_data = F,
                      bar_params = list(width = 0.7, position = "stack"),
                      topN = 8, others = T, relative = T, legend_title = "",
                      stack_order = T, group_order = F, facet_order = F,
                      style = c("group", "sample")[1],
                      flow = F, flow_params = list(lode.guidance = "frontback", color = "darkgray"),
                      number = F, format_params = list(digits = 2), text_params = list(position = position_stack())) {
  # Used to draw species stacking diagrams, suitable for processing various OTU similar data, input metatab as the basis for grouping.
  # style can choose "group" or "sample"
  # others=T is used to choose whether to draw other than TopN
  # pmode can choose fill/stack/dodge
  # library(ggplot2)
  # library(dplyr)
  lib_ps("reshape2", "scales", "dplyr", library = F)
  # prepare otutab and sampFile
  if (!is.null(metadata)) {
    match_res <- match_df(otutab, metadata)
    otutab <- match_res$otutab
    sampFile <- as.data.frame(match_res$metadata[, group], row.names = row.names(match_res$metadata))
    colnames(sampFile)[1] <- "group"
  } else {
    sampFile <- data.frame(row.names = colnames(otutab), group = colnames(otutab))
  }

  mean_sort <- as.data.frame(otutab[(order(-rowSums(otutab))), , drop = F])

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
      lib_ps("ggalluvial", library = F)
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
      lib_ps("ggalluvial", library = F)
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
#' @return a 'ggplot' plot object,
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' #a <- data.frame(a = 1:18, b = runif(18, 0, 5))
#' #group_box(a, group = rep(c("a", "b", "c"), each = 6), p_value1 = FALSE, p_value2 = TRUE)
#' }
group_box <- function(tab, group = NULL, metadata = NULL, mode = 1,
                      group_order = NULL, facet_order = NULL,
                      alpha = F, method = "wilcox", alpha_param = list(color = "red"),
                      p_value1 = F, p_value2 = F, stat_compare_means_param = NULL,
                      trend_line = F, trend_line_param = list(color = "blue")) {
  lib_ps("ggplot2", "dplyr", "reshape2", library = F)
  # data transform
  g_name <- NULL

  if (is.vector(tab)) {
    tab <- data.frame(value = tab)
  } else {
    tab <- select_if(tab, is.numeric)
  }
  if ("group" %in% colnames(tab)) stop("group can not be one of colnames(tab)")

  if (is.null(metadata) && is.null(group)) {
    # a single boxplot
    md <- data.frame(tab, group = "value", check.names = F)
  } else {
    if (is.null(metadata) && !is.null(group)) {
      md <- data.frame(tab, group = group, check.names = F)
    } else if (!is.null(metadata) && !is.null(group)) {
      if (!all(rownames(metadata) %in% rownames(tab))) message("rownames dont match in tab and metadata")
      idx <- rownames(metadata) %in% rownames(tab)
      metadata <- metadata[idx, , drop = F]
      tab <- tab[rownames(metadata), , drop = F]
      md <- data.frame(tab, group = metadata[, group, drop = T], check.names = F)
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
    lib_ps("gghalves", library = F)
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
  if (trend_line) p <- p + do.call(geom_smooth, update_param(list(mapping = aes(group = 1), method = "glm", se = F, alpha = 0.8), trend_line_param))

  # facet?
  flag <- (ncol(tab) == 1)
  if (!flag) {
    p <- p + facet_wrap(. ~ indexes, scales = "free_y")
  } else {
    ylab <- colnames(tab)[1]
    p <- p + ylab(ylab)
  }

  # p-value?
  if (is.character(p_value1) | p_value1 == T) {
    lib_ps("ggpubr", library = F)
    if (p_value1 == T) p_value1 <- NULL
    md %>% summarise(low = min(value), high = max(value)) -> aa
    #    p <- p + ggpubr::stat_compare_means(show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1)
    p <- p + do.call(ggpubr::stat_compare_means, update_param(list(
      show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1
    ), stat_compare_means_param))
  }

  if (is.character(p_value2) | p_value2 == T) {
    lib_ps("ggpubr", library = F)
    if (p_value2 == T) p_value2 <- NULL
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
        text_param
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
        text_param
      ))
    }
  }

  return(p)
}


#' Plot correlation
#'
#' @param env dataframe1
#' @param env2 dataframe2 (default:NULL)
#' @param mode plot mode (1~3)
#' @param method one of "pearson","kendall","spearman"
#' @param heat plot heatmap when columns >30
#' @param ... for \code{\link[pheatmap]{pheatmap}}
#' @param mode3_param parameters parse to \code{\link[corrplot]{corrplot}}
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' #data(otutab)
#' #cor_plot(metadata[, 3:10])
#' #cor_plot(metadata[, 3:10], mode = 2)
#' #cor_plot(t(otutab)[,1:50],mode=3,heat=FALSE)
#' }
cor_plot <- function(env, env2 = NULL, mode = 1, method = "pearson", heat = T, mode3_param = NULL, ...) {
  if (ncol(env) > 30 & heat) {
    lib_ps("pheatmap", library = F)
    stats::cor(env) -> a
    pheatmap::pheatmap(a, show_rownames = F, show_colnames = F, border_color = F, ...)
  } else {
    lib_ps("ggcor", library = F)
    ggcor::set_scale(c("#6D9EC1", "white", "#E46726"), type = "gradient2n")
    if (is.null(env2)) {
      if (mode == 1) {
        p <- ggcor::quickcor(env, method = method, cor.test = T) +
          ggcor::geom_square(data = ggcor::get_data(type = "lower", show.diag = FALSE)) +
          ggcor::geom_mark(data = ggcor::get_data(type = "upper", show.diag = FALSE), size = 2.5) +
          geom_abline(slope = -1, intercept = ncol(env) + 1)
        return(p)
      }

      if (mode == 2) {
        p <- env %>% ggcor::quickcor(
          circular = TRUE, cluster = TRUE, open = 45,
          method = method, cor.test = T
        ) +
          ggcor::geom_colour(colour = "white", size = 0.125) +
          ggcor::anno_row_tree() +
          ggcor::anno_col_tree() +
          ggcor::set_p_xaxis() +
          ggcor::set_p_yaxis()
        return(p)
      }

      if (mode == 3) {
        lib_ps("corrplot", library = F)
        ggcor::correlate(env, method = method, cor.test = T, p.adjust = T, p.adjust.method = "fdr") -> res2
        rownames(res2$p.value) <- rownames(res2$r)
        colnames(res2$p.value) <- colnames(res2$r)

        do.call(corrplot::corrplot, update_param(list(
          corr = res2$r, order = "hclust", p.mat = res2$p.value, sig.level = 0.05, insig = "blank",
          diag = FALSE, tl.cex = 0.5, addrect = 5, method = "color", outline = TRUE,
          col = RColorBrewer::brewer.pal(n = 10, name = "PuOr"), tl.srt = 45, tl.col = "black"
        ), mode3_param))
      }
    } else {
      if (mode == 1) {
        if (ncol(env2) == 1) {
          env2 <- cbind(env2, env2)
          p <- ggcor::quickcor(env, env2, method = method, cor.test = T) +
            ggcor::geom_square(data = ggcor::get_data(show.diag = FALSE)) +
            ggcor::geom_mark(data = ggcor::get_data(show.diag = FALSE), size = 2.5)
          p <- p + coord_fixed(xlim = c(0.5, 1.5))
        } else if (ncol(env) == 1) {
          env <- cbind(env, env)
          p <- ggcor::quickcor(env, env2, method = method, cor.test = T) +
            ggcor::geom_square(data = ggcor::get_data(show.diag = FALSE)) +
            ggcor::geom_mark(data = ggcor::get_data(show.diag = FALSE), size = 2.5)
          p <- p + coord_fixed(ylim = c(0.5, 1.5))
        } else {
          p <- ggcor::quickcor(env, env2, method = method, cor.test = T) +
            ggcor::geom_square(data = ggcor::get_data(show.diag = FALSE)) +
            ggcor::geom_mark(data = ggcor::get_data(show.diag = FALSE), size = 2.5)
        }
        return(p)
      }

      if (mode == 2) {
        p <- ggcor::quickcor(env, env2,
          circular = TRUE, cluster = TRUE, open = 45,
          method = method, cor.test = T
        ) +
          ggcor::geom_colour(colour = "white", size = 0.125) +
          ggcor::anno_row_tree() +
          ggcor::anno_col_tree() +
          ggcor::set_p_xaxis() +
          ggcor::set_p_yaxis()
        return(p)
      }

      if (mode == 3) {
        lib_ps("corrplot", library = F)
        ggcor::correlate(env, env2, method = method, cor.test = T, p.adjust = T, p.adjust.method = "fdr") -> res2
        rownames(res2$p.value) <- rownames(res2$r)
        colnames(res2$p.value) <- colnames(res2$r)

        corrplot::corrplot(res2$r,
          p.mat = res2$p.value, sig.level = 0.05, diag = FALSE, method = "square",
          tl.srt = 45, tl.col = "black", addCoef.col = "black", insig = "label_sig"
        )
      }
    }
  }
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
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' #a <- data.frame(type = letters[1:6], num = c(1, 3, 3, 4, 5, 10))
#' #gghuan(a) + ggplot2::scale_fill_manual(values = get_cols(6, "col3"))
#' #b <- data.frame(type = letters[1:12], num = c(1, 3, 3, 4, 15, 10, 35, 2:6))
#' #gghuan(b) + ggplot2::theme(legend.position = "right")
#' }
gghuan <- function(tab, reorder = T, mode = "1", topN = 5, name = T, percentage = T) {
  if (ncol(tab) > 2) stop("need two columns: first is type, second is number")

  colnames(tab)[1] -> g_name
  colnames(tab) <- c("type", "n")

  plot_df <- tab %>%
    group_by(type) %>%
    summarise(sum = sum(n))

  if (reorder) {
    plot_df$type <- stats::reorder(plot_df$type, plot_df$sum)
    plot_df <- arrange(plot_df, -sum)
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
  mutate(plot_df, fraction = sum / sum(sum)) -> plot_df

  plot_df$ymax <- cumsum(plot_df$fraction)
  plot_df$ymin <- c(0, head(plot_df$ymax, n = -1))
  if (percentage) {
    plot_df$rate_per <- paste(as.character(round(100 * plot_df$fraction, 1)), "%", sep = "")
  } else {
    plot_df$rate_per <- plot_df$sum
  }

  if (mode == 3) {
    lib_ps("ggpubr", library = F)
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


#' gghuan2 for multi-columns
#'
#' @param tab a dataframe with hierarchical structure
#' @param break default 0.2
#' @param name label the name
#' @param number label the number
#' @param percentage label the percentage
#' @param text_col defalut, black
#'
#' @import ggplot2 dplyr
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' #data.frame(a = c("a", "a", "b", "b", "c"), aa = rep("a", 5),
#' #      b = c("a", LETTERS[2:5]), c = 1:5) %>% gghuan2()
#' }
gghuan2 <- function(tab = NULL, `break` = 0.2, name = T, number = T, percentage = F, text_col = "black") {
  if (!is.numeric(tab[, ncol(tab)])) stop("the last column must be numeric")
  if ((`break` < 0) | `break` >= 1) stop("`break` should be [0,1)")

  plot_df_res <- data.frame()
  for (i in seq_len(ncol(tab) - 1)) {
    plot_df <- tab[, c(i, ncol(tab))]
    colnames(plot_df) <- c("type", "n")
    count2(plot_df) -> plot_df
    mutate(plot_df, fraction = n / sum(n)) -> plot_df
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
#' \dontrun{
#' #my_lm(runif(50), var = 1:50)
#' #my_lm(c(1:50) + runif(50, 0, 5), var = 1:50)
#' }
my_lm <- function(tab, var, metadata = NULL, ...) {
  lib_ps("reshape2", "ggpmisc", library = F)
  # data transform
  g_name <- NULL
  if (is.vector(tab)) tab <- data.frame(value = tab)

  if (is.null(metadata)) {
    md <- data.frame(tab, var = var, check.names = F)
  } else if (!is.null(metadata)) {
    if (!all(rownames(metadata) %in% rownames(tab))) message("rownames dont match in tab and metadata")
    idx <- rownames(metadata) %in% rownames(tab)
    metadata <- metadata[idx, , drop = F]
    tab <- tab[rownames(metadata), , drop = F]
    md <- data.frame(tab, var = metadata[, var], check.names = F)
    g_name <- var
  }

  if (!all(apply(md, 2, is.numeric))) stop("need numeric")
  md %>% reshape2::melt(., id.vars = "var", variable.name = "indexes") -> md
  md$indexes <- factor(md$indexes, levels = colnames(tab))
  # main plot
  p <- ggplot(md, aes(var, value)) +
    geom_point(...) +
    geom_smooth(method = "lm", color = "red", se = F, formula = "y~x") +
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
  lib_ps("ggspatial", "sf", library = F)
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
      # aspect.ratio = 1.25, #调节长宽比
      # axis.text = element_blank(),
      # axis.ticks = element_blank(),
      axis.title = element_blank(),
      # panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey10", linetype = 1, linewidth = 1),
      plot.margin = unit(c(0, 0, 0, 0), "mm")
    )
}

#' Plot a DNA double helix
#'
#' @export
#' @references \code{https://github.com/SherryDong/create_plot_by_R_base}
dna_plot <- function() {
  lib_ps("RColorBrewer", library = F)
  col_DNA <- RColorBrewer::brewer.pal(8, "Set1")[2]
  # A-green, T-red, C-yellow, G-blue
  col_ATCG <- c(
    RColorBrewer::brewer.pal(8, "Accent")[1], RColorBrewer::brewer.pal(11, "Set3")[4],
    RColorBrewer::brewer.pal(11, "Set3")[2], RColorBrewer::brewer.pal(11, "Paired")[1]
  )
  DNA_length <- 4 ## the code only applies when DNA_length%%2==0, if DNA_length%%2==1, need to modify
  x <- seq(-DNA_length * pi / 2, DNA_length * pi / 2, length.out = 1000) ##
  y1 <- cos(x) ## backbone up
  y2 <- cos(x + pi) ## backbone down
  # get the position of nucleotides
  xx <- seq(DNA_length * pi / 2, -DNA_length * pi / 2, length.out = DNA_length * 5 + 1)
  xx <- xx + (xx[2] - xx[1]) / 2
  # remove the first and the lines in the boundary region
  xx <- setdiff(xx, c(xx[c(1:DNA_length) * 5 - 2], min(xx)))
  plot(y1 ~ x, pch = 16, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "", bty = "n", col = "white")
  for (i in 1:length(xx)) {
    ybottom <- cos(xx[i]) # ybottom position
    ytop <- cos(xx[i] + pi) # yup position
    rr <- sample(1:4, 1) ## ATCG, random select one pair
    if (rr == 1) {
      segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[1], lwd = 4) ## A-T
      segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[2], lwd = 4)
    }
    if (rr == 2) {
      segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[2], lwd = 4) ## T-A
      segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[1], lwd = 4)
    }
    if (rr == 3) {
      segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[3], lwd = 4) ## C-G
      segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[4], lwd = 4)
    }
    if (rr == 4) {
      segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[4], lwd = 4) ## G-C
      segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[3], lwd = 4)
    }
  }
  lines(y1 ~ x, pch = 16, lwd = 8, col = col_DNA)
  lines(y2 ~ x, pch = 16, lwd = 8, col = col_DNA)
}

#' Show my little cat named Guo Dong which drawn by my girlfriend.
#' @param mode 1~2
#'
#' @export
my_cat <- function(mode = 1) {
  data("little_guodong", package = "pcutils", envir = environment())
  if (mode == 1) {
    p <- ggplot() +
      annotation_custom(little_guodong, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
  }
  if (mode == 2) {
    lib_ps("ggimage", library = F)
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
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' #data(otutab)
#' #tax_pie(otutab,topN = 7)
#' }
tax_pie<-function(otutab,topN=6,...){
  lib_ps("ggpubr",library = F)
  if(is.vector(otutab)){
    otutab->a
    if(!is.null(names(a)))names(a)=seq_along(a)
  }
  else rowSums(otutab)->a
  if(length(a)>topN){
    sort(a,decreasing = T)[1:topN-1]->b
    other=sum(sort(a,decreasing = T)[topN:length(a)])
    b<-c(b,other)
    names(b)[length(b)]<-'Others'}
  else b<-a

  df=data.frame(va=b,labels = paste0(names(b),"\n(",round(b/sum(b)*100,2),"%)"))
  ggpubr::ggpie(df,'va',fill=get_cols(length(b)),label = "labels",grepl=T,...)
}

#' Radar plot
#'
#' @param otu_time otutab
#' @param ... add
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #data(otutab)
#' #tax_radar(otutab[1:20,1:3])
#' }
tax_radar<-function(otu_time,...){
  lib_ps("ggradar","scales",library = F)
  otu_time[1:4,]%>%
    mutate_all(scales::rescale) %>%cbind(tax=rownames(.),.)%>%
    ggradar::ggradar(.,legend.text.size=10,...)
}

#' Word cloud plot
#'
#' @param str_vector string vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #data(otutab)
#' #tax_wordcloud(taxonomy$Genus)
#' }
tax_wordcloud<-function(str_vector){
  lib_ps("wordcloud2",library = F)
  remove_unclassfied<-\ (taxdf) {
    taxdf[grepl.data.frame("Unclassified|uncultured|Ambiguous|Unknown|unknown|metagenome|Unassig",
                           taxdf, ignore.case = TRUE)] <- NA
    return(taxdf)
  }
  sort(table(str_vector),decreasing = TRUE)[1:50]%>%as.data.frame()%>%
    remove_unclassfied()%>%na.omit()%>%wordcloud2::wordcloud2(.,size=.7)
}

#' Triangle plot
#'
#' @param otutab otutab
#' @param group group
#' @param scale default:F
#' @param class point color
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #data(otutab)
#' #triangp(otutab,metadata$Group,class=taxonomy$Phylum,scale=TRUE)
#' }
triangp<-function(otutab,group,scale=F,class=NULL){
  lib_ps("ggtern","vegan",library = F)
  group%>%as.factor()->group
  if (nlevels(group)!=3)stop("group is not 3, can't plot trip")
  hebing(otutab,group,act = 'mean')->tmp

  if (scale){mutate_all(tmp,scales::rescale)->tmp}

  tmp%>%as.data.frame()%>%mutate(sum=rowSums(.))->tmp1
  colnames(tmp1)[1:3]<-c('KO','OE','WT')

  if (is.null(class)){
    p=ggtern::ggtern(tmp1,aes(x=KO,y=OE,z=WT)) +
      geom_point(aes(size=sum))+#define data geometry
      labs(x=names(tmp)[1],y=names(tmp)[2],z=names(tmp)[3])
    return(p)
  }
  else {
    tmp1$class =class
    p=ggtern::ggtern(tmp1,aes(x=KO,y=OE,z=WT)) +
      geom_point(aes(size=sum,col=class))+#define data geometry
      labs(x=names(tmp)[1],y=names(tmp)[2],z=names(tmp)[3])
    return(p)
  }
}

#' My Sankey plot
#'
#' @param test a dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[sankeyD3]{sankeyNetwork}}
#' @param mode "sankeyD3","ggsankey"
#' @param space space width for ggsankey
#'
#' @export
#'
#' @import ggplot2 dplyr
#' @examples
#' \dontrun{
#' #data.frame(a=c("a","a","b","b","c"),aa=rep("a",5),b=c("a",LETTERS[2:5]),c=1:5)%>%
#' #   my_sankey(.,"gg",num=TRUE)
#' #data(otutab)
#' #cbind(taxonomy,num=rowSums(otutab))[1:10,]->test
#' #my_sankey(test)->p
#' }
my_sankey=function(test,mode=c("sankeyD3","ggsankey"),space=1,...){
  mode=match.arg(mode,c("sankeyD3","ggsankey"))
  test=as.data.frame(test)
  nc=ncol(test)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")
  if(mode=="sankeyD3"){
    lib_ps("sankeyD3",library = F)
    #change duplicated data
    for (i in 1:(nc-1)){
      test[,i]=paste0(test[,i],strrep(" ",i-1))
    }
    #merge to two columns
    links=data.frame()
    for (i in 1:(nc-2)){
      test[,c(i,i+1,nc)]->tmp
      colnames(tmp)=c("source","target","weight")
      tmp=group_by(tmp,source,target)%>%summarise(weight=sum(weight),.groups="keep")
      links=rbind(links,tmp)
    }
    #give ids
    nodes <- data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    p=sankeyD3::sankeyNetwork(Links = as.data.frame(links), Nodes = nodes,
                    Source = "IDsource", Target = "IDtarget",Value = "weight",
                    NodeID = "name",nodeWidth =10,units = 'TWh',
                    xAxisDomain =colnames(test)[-nc],
                    # height=400,width=500,
                    # colourScale=JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                    # numberFormat=".0f",
                    # fontSize = 8,dragY = T,nodeShadow = T,
                    # doubleclickTogglesChildren = T,
                    ...)

    return(p)
  }
  if(mode=="ggsankey"){
    lib_ps("ggsankey",library = F)
    df=ggsankey::make_long(test,1:(nc-1),value =!!nc)
    parms=list(...)

    if(!is.null(parms$num)){
      if((parms$num)){
        df%>%group_by(x,node)%>%summarise(value=sum(value))%>%mutate(label=paste0(node,"\n",value))->tmp
        df=left_join(df,tmp[,-3])}
      else df$label=df$node
    }
    else df$label=df$node

    p=ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, label = label,fill = factor(node),value=value)) +
      ggsankey::geom_sankey(flow.alpha = .6,node.color = "gray30",space = space) +
      ggsankey::geom_sankey_text(size = 3, color = "black",space = space) +
      ggsankey::theme_sankey(base_size = 18) +
      labs(x = NULL) + scale_fill_manual(values = get_cols(nlevels(factor(df$node))))+
      theme(legend.position = "none",plot.title = element_text(hjust = .5))
    return(p)
  }
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
#' \dontrun{
#' #data.frame(a=c("a","a","b","b","c"),b=c("a",LETTERS[2:5]),c=1:5)%>%my_circo(mode="chorddiag")
#' }
my_circo=function(df,reorder=T,pal=NULL,mode=c("circlize","chorddiag"),...){
  mode=match.arg(mode,c("circlize","chorddiag"))
  colnames(df)=c("from","to","count")
  lib_ps("reshape2","tibble",library = F)
  if(mode=="chorddiag"){
    #need a square matrix
    all_g=unique(df$from,df$to)
    expand.grid(all_g,all_g)->tab
    df=left_join(tab,df,by=c("Var1"="from","Var2"="to"))
    colnames(df)=c("from","to","count")}

  tab=reshape2::dcast(df,from~to,value.var = "count")%>%tibble::column_to_rownames("from")%>%as.matrix()
  tab[is.na(tab)]=0

  if(reorder){
    colSums(tab)%>%sort(decreasing = T)%>%names()->s_name
    tab=tab[,s_name]
    rowSums(tab)%>%sort(decreasing = T)%>%names()->s_name
    tab=tab[s_name,]
  }

  if(is.null(pal))pal=get_cols(length(unique(c(colnames(tab),rownames(tab)))))

  if(mode=="circlize"){
    lib_ps("circlize",library = F)
    circlize::chordDiagram(tab,grid.col = pal,...)
  }
  if(mode=="chorddiag"){
    lib_ps("chorddiag",library = F)
    chorddiag::chorddiag(tab,groupedgeColor= pal,...)
    }
}

#' My synteny plot
#'
#' @export
#'
my_synteny<-function(){
  modu_sum<-data.frame(module=c(1:5,1:5,1:3),
                       start=1,
                       end=5,
                       fill=get_cols(13)%>%sub("#","",.),
                       omics=c(rep("A",5),rep("B",5),rep("C",3)),
                       size=10,
                       color="252525")
  edge_sum<-data.frame(omics1=1:5,
                       start_1=c(1,2,3,4,3),
                       end_1=c(3,3,4,5,5),
                       omics2=c(3,2,3,2,5),
                       start_2=c(1,2,1,1,3),
                       end_2=c(3,3,2,2,5),
                       fill="cccccc",
                       type=c(3,3,2,2,1))

  lib_ps("RIdeogram",library = F)
  colnames(modu_sum)=c("Chr","Start","End","fill","species","size","color")
  colnames(edge_sum)=c("Species_1","Start_1","End_1","Species_2","Start_2","End_2","fill")
  RIdeogram::ideogram(karyotype = modu_sum, synteny =edge_sum)
  rsvg::rsvg_svg("chromosome.svg",file = "chromosome.svg")
  read.file("chromosome.svg")
}
#=======some tips========

#' How to use parallel
#' @export
how_to_use_parallel=function(){
  cat('  #parallel
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
how_to_update_parameters=function(){
  cat('point_params = list(size=5,color="red")
ggplot(data.frame(x=1:5,y=5:1), aes(x = x, y = y))+
  do.call(geom_point, update_param(list(size=2,color="blue",alpha=0.5), point_params))')
}
