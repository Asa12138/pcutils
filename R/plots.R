
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
#' data(otutab)
#' stackplot(otutab, metadata, group = "Group")
#' stackplot(otutab, metadata, group = "Group", group_order = TRUE, flow = FALSE, relative = FALSE)
#'
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
#' a <- data.frame(a = 1:18, b = runif(18, 0, 5))
#' group_box(a, group = rep(c("a", "b", "c"), each = 6), p_value1 = FALSE, p_value2 = TRUE)
#' multitest(a$a, group = rep(c("a", "b", "c"), each = 6))
#' group_box(a[, 1, drop = F], group = rep(c("a", "b", "c"), each = 6), p_value2 = T, mode = 3,
#'    stat_compare_means_param=list(comparisons = list(c("a", "b"))))
group_box <- function(tab, group = NULL, metadata = NULL, mode = 1,
                      group_order = NULL, facet_order = NULL,
                      alpha = F, method = "wilcox",alpha_param=list(color = "red"),
                      p_value1 = F,p_value2 = F,stat_compare_means_param=NULL,
                      trend_line = F,trend_line_param=list(color = "blue")) {
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
    p <- ggplot(md, aes(x=group, y=value, color = group, group = group)) +
      stat_boxplot(geom = "errorbar", width = 0.15) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.15, alpha = 0.8, size = 0.5)
  }
  if (mode == 2) {
    p <- ggplot(md, aes(x=group, y=value, fill = group, group = group)) +
      # stat_boxplot(geom = "errorbar",width=0.15)+
      geom_boxplot(color = "black", outlier.shape = NA) +
      geom_jitter(color = "black", width = 0.15, alpha = 0.8, size = 0.5)
  }
  if (mode == 3) {
    lib_ps("gghalves", library = F)
    p <- ggplot(md, aes(x=group, y=value, color = group, group = group)) +
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
  if (trend_line) p <- p + do.call(geom_smooth,update_param(list(mapping=aes(group = 1), method = "glm",se = F, alpha = 0.8),trend_line_param))

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
    lib_ps("ggpubr",library = F)
    if (p_value1 == T) p_value1 <- NULL
    md %>% summarise(low = min(value), high = max(value)) -> aa
#    p <- p + ggpubr::stat_compare_means(show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1)
    p=p+do.call(ggpubr::stat_compare_means,update_param(list(
      show.legend = FALSE, method = p_value1, label.x = 1, label.y.npc = 1
    ),stat_compare_means_param))
  }

  if (is.character(p_value2) | p_value2 == T) {
    lib_ps("ggpubr",library = F)
    if (p_value2 == T) p_value2 <- NULL
    comparisons = utils::combn(levels(md$group), 2) %>% split(col(.))
    p=p+do.call(ggpubr::stat_compare_means,update_param(list(
      show.legend = FALSE, method = p_value2,comparisons = comparisons
    ),stat_compare_means_param))
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
      p=p+do.call(geom_text,update_param(list(data = aa, mapping=aes(x = variable, y = (high + 0.15 * (high - low)), label = groups),
                                              inherit.aes = FALSE, color = "red", size = 5, position = position_nudge(x = .1)),
                                         text_param))
    } else {
      # p <- p + geom_text(
      #   data = aa, aes(x = variable, y = (high + 0.05 * (high - low)), label = groups),
      #   inherit.aes = FALSE, color = alpha_color, size = 5
      # )
      p=p+do.call(geom_text,update_param(list(data = aa, mapping=aes(x = variable, y = (high + 0.05 * (high - low)), label = groups),
                                              inherit.aes = FALSE, color = "red", size = 5),
                                         text_param))
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
#' data(otutab)
#' cor_plot(metadata[,3:10])
#' cor_plot(metadata[,3:10],mode=2)
#' #cor_plot(t(otutab)[,1:50],mode=3,heat=FALSE)
cor_plot<-function(env,env2=NULL,mode=1,method = "pearson",heat=T,mode3_param=NULL,...){
  if(ncol(env)>30&heat){
    lib_ps("pheatmap",library = F)
    stats::cor(env)->a
    pheatmap::pheatmap(a,show_rownames = F,show_colnames = F,border_color = F,...)
  }
  else {
    lib_ps("ggcor")
    ggcor::set_scale(c("#6D9EC1", "white", "#E46726"),type = "gradient2n")
    if(is.null(env2)){
      if(mode==1){p<-quickcor(env, method = method,cor.test = T) +
        geom_square(data = get_data(type = "lower", show.diag = FALSE)) +
        geom_mark(data = get_data(type = "upper", show.diag = FALSE), size = 2.5) +
        geom_abline(slope = -1, intercept=ncol(env)+1)
      detach('package:ggcor')
      return(p)
      }

      if(mode==2) {
        p<-env%>%quickcor(circular = TRUE, cluster = TRUE, open = 45,
                          method = method,cor.test = T) +
          geom_colour(colour = "white", size = 0.125) +
          anno_row_tree() +
          anno_col_tree() +
          set_p_xaxis() +
          set_p_yaxis()
        detach('package:ggcor')
        return(p)
      }

      if(mode==3){
        lib_ps("corrplot",library = F)
        ggcor::correlate(env, method = method,cor.test = T,p.adjust = T,p.adjust.method = "fdr")->res2
        rownames(res2$p.value)<-rownames(res2$r);colnames(res2$p.value)<-colnames(res2$r)

        do.call(corrplot::corrplot,update_param(list(
          corr = res2$r, order = "hclust", p.mat = res2$p.value, sig.level = 0.05, insig = "blank",
          diag = FALSE, tl.cex=0.5, addrect = 5, method="color", outline=TRUE,
          col=RColorBrewer::brewer.pal(n=10, name="PuOr"),tl.srt=45, tl.col="black"
        ),mode3_param))
      }
    }
    else{
      if(mode==1){
        if (ncol(env2)==1) {
          env2=cbind(env2,env2)
          p<-quickcor(env,env2, method = method,cor.test = T) +
            geom_square(data = get_data(show.diag = FALSE)) +
            geom_mark(data = get_data(show.diag = FALSE), size = 2.5)
          p=p+coord_fixed(xlim = c(0.5,1.5))
        }
        else if(ncol(env)==1){
          env=cbind(env,env)
          p<-quickcor(env,env2, method = method,cor.test = T) +
            geom_square(data = get_data(show.diag = FALSE)) +
            geom_mark(data = get_data(show.diag = FALSE), size = 2.5)
          p=p+coord_fixed(ylim = c(0.5,1.5))
        }
        else{
          p<-quickcor(env,env2, method = method,cor.test = T) +
            geom_square(data = get_data(show.diag = FALSE)) +
            geom_mark(data = get_data(show.diag = FALSE), size = 2.5)
        }
        detach('package:ggcor')
        return(p)
      }

      if(mode==2) {
        p<-quickcor(env,env2,circular = TRUE, cluster = TRUE, open = 45,
                    method = method,cor.test = T) +
          geom_colour(colour = "white", size = 0.125) +
          anno_row_tree() +
          anno_col_tree() +
          set_p_xaxis() +
          set_p_yaxis()
        detach('package:ggcor')
        return(p)
      }

      if(mode==3){
        lib_ps("corrplot",library = F)
        ggcor::correlate(env, env2,method = method,cor.test = T,p.adjust = T,p.adjust.method = "fdr")->res2
        rownames(res2$p.value)<-rownames(res2$r);colnames(res2$p.value)<-colnames(res2$r)

        corrplot::corrplot(res2$r, p.mat = res2$p.value, sig.level = 0.05,diag = FALSE,method="square",
                           tl.srt=45, tl.col="black",addCoef.col = "black",insig = "label_sig")
      }
    }
  }
}
