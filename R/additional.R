# Some function depends on packages not in CRAN.

# ggcor,
# pheatmap,
# corrplot,
# ggradar,
# ggsankey,
# sankeyD3,
# SoDA


#' Transfer Geographical latitude and longitude to XY(m)
#'
#' @param geo a two-columns dataframe, first is latitude, second is longitude
#'
#' @export
#' @return data.frame
#' @examples
#' \donttest{
#' data.frame(row.names = letters[1:18], x = runif(18, 30, 35), y = runif(18, 40, 45)) -> geo
#' toXY(geo)
#' }
toXY <- function(geo) {
    lib_ps("SoDA", library = FALSE)
    XY <- SoDA::geoXY(geo[, 1], geo[, 2])
    # geosphere::distm
    return(as.data.frame(row.names = rownames(geo), XY))
}

# ========Common plots=======

#' Plot correlation
#'
#' @param env dataframe1
#' @param env2 dataframe2 (default:NULL)
#' @param mode plot mode (1~3)
#' @param method one of "pearson","kendall","spearman"
#' @param heat plot heatmap when columns >30
#' @param ... for \code{\link[pheatmap]{pheatmap}}
#' @param mode3_param parameters parse to \code{\link[corrplot]{corrplot}}
#' @return ggplot
#' @import ggplot2
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cor_plot(metadata[, 3:10])
#' cor_plot(metadata[, 3:10], mode = 2)
#' cor_plot(t(otutab)[, 1:50], mode = 3, heat = FALSE)
#' }
cor_plot <- function(env, env2 = NULL, mode = 1, method = "pearson", heat = TRUE, mode3_param = NULL, ...) {
    if (ncol(env) > 30 & heat) {
        lib_ps("pheatmap", library = FALSE)
        stats::cor(env) -> a
        pheatmap::pheatmap(a, show_rownames = FALSE, show_colnames = FALSE, border_color = FALSE, ...)
    } else {
        lib_ps("ggcor", library = FALSE)
        if (isNamespaceLoaded("linkET")) lapply(c("ggcor", "linkET"), unloadNamespace)

        # ggcor::set_scale(bluered, type = "gradient2n")
        if (is.null(env2)) {
            if (mode == 1) {
                p <- ggcor::quickcor(env, method = method, cor.test = TRUE) +
                    ggcor::geom_square(data = ggcor::get_data(type = "lower", show.diag = FALSE)) +
                    ggcor::geom_mark(data = ggcor::get_data(type = "upper", show.diag = FALSE), size = 2.5) +
                    geom_abline(slope = -1, intercept = ncol(env) + 1) +
                    scale_fill_gradientn(colours = get_cols(pal = "bluered"), limit = c(-1, 1))
                return(p)
            }

            if (mode == 2) {
                p <- env %>% ggcor::quickcor(
                    circular = TRUE, cluster = TRUE, open = 45,
                    method = method, cor.test = TRUE
                ) +
                    ggcor::geom_colour(colour = "white", size = 0.125) +
                    ggcor::anno_row_tree() +
                    ggcor::anno_col_tree() +
                    ggcor::set_p_xaxis() +
                    ggcor::set_p_yaxis() +
                    scale_fill_gradientn(colours = get_cols(pal = "bluered"), limit = c(-1, 1))
                return(p)
            }

            if (mode == 3) {
                lib_ps("corrplot", library = FALSE)
                ggcor::correlate(env, method = method, cor.test = TRUE, p.adjust = TRUE, p.adjust.method = "fdr") -> res2
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
                    p <- ggcor::quickcor(env, env2, method = method, cor.test = TRUE) +
                        ggcor::geom_square(data = ggcor::get_data(show.diag = FALSE)) +
                        ggcor::geom_mark(data = ggcor::get_data(show.diag = FALSE), size = 2.5)
                    p <- p + coord_fixed(xlim = c(0.5, 1.5))
                } else if (ncol(env) == 1) {
                    env <- cbind(env, env)
                    p <- ggcor::quickcor(env, env2, method = method, cor.test = TRUE) +
                        ggcor::geom_square(data = ggcor::get_data(show.diag = FALSE)) +
                        ggcor::geom_mark(data = ggcor::get_data(show.diag = FALSE), size = 2.5)
                    p <- p + coord_fixed(ylim = c(0.5, 1.5))
                } else {
                    p <- ggcor::quickcor(env, env2, method = method, cor.test = TRUE) +
                        ggcor::geom_square(data = ggcor::get_data(show.diag = FALSE)) +
                        ggcor::geom_mark(data = ggcor::get_data(show.diag = FALSE), size = 2.5)
                }
                return(p + scale_fill_gradientn(colours = get_cols(pal = "bluered"), limit = c(-1, 1)))
            }

            if (mode == 2) {
                p <- ggcor::quickcor(env, env2,
                    circular = TRUE, cluster = TRUE, open = 45,
                    method = method, cor.test = TRUE
                ) +
                    ggcor::geom_colour(colour = "white", size = 0.125) +
                    ggcor::anno_row_tree() +
                    ggcor::anno_col_tree() +
                    ggcor::set_p_xaxis() +
                    ggcor::set_p_yaxis() +
                    scale_fill_gradientn(colours = get_cols(pal = "bluered"), limit = c(-1, 1))
                return(p)
            }

            if (mode == 3) {
                lib_ps("corrplot", library = FALSE)
                ggcor::correlate(env, env2, method = method, cor.test = TRUE, p.adjust = TRUE, p.adjust.method = "fdr") -> res2
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

#' Radar plot
#'
#' @param group_df group_df
#' @param ... add
#'
#' @export
#' @return ggplot
#' @examples
#' \donttest{
#' data(otutab)
#' tax_radar(otutab[1:6, 1:4])
#' }
tax_radar <- function(group_df, ...) {
    lib_ps("ggradar", "scales", library = FALSE)
    if (nrow(group_df) > 20 | ncol(group_df) > 6) {
        stop("too many columns or rows!")
    } else {
        group_df %>%
            dplyr::mutate_all(scales::rescale) %>%
            cbind(tax = rownames(.), .) %>%
            ggradar::ggradar(., legend.text.size = 10, ...)
    }
}


# 多余合并为others
gettop <- \(a, top){
    nc <- ncol(a)
    if (nc < 3) stop("as least 3-columns dataframe")
    if (!is.numeric(a[, nc])) stop("the last column must be numeric")
    colnames(a) -> cns
    colnames(a) <- c(paste0("f", seq_len(nc - 1)), "n")
    a <- mutate_at(a, seq_len(nc - 1), as.character)
    top <- rep(top, length.out = nc - 1)
    keep <- list()
    for (i in seq_len(nc - 1)) {
        tmpc <- colnames(a)[i]
        colnames(a)[i] <- "tmp"
        a %>%
            dplyr::group_by(tmp) %>%
            dplyr::summarise(count = sum(n)) %>%
            dplyr::arrange(-count) %>%
            head(top[i]) %>%
            dplyr::pull(tmp) -> keep[[i]]
        a <- mutate(a, tmp = ifelse(tmp %in% keep[[i]], tmp, paste0("other_", cns[i])))
        colnames(a)[i] <- tmpc
    }
    a <- a %>%
        dplyr::group_by_at(seq_len(nc - 1)) %>%
        dplyr::summarise(count = sum(n)) %>%
        dplyr::arrange(-count)
    colnames(a) <- cns
    as.data.frame(a)
}

#' My Sankey plot
#'
#' @param test a dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[sankeyD3]{sankeyNetwork}}
#' @param mode "sankeyD3","ggsankey"
#' @param space space width for ggsankey
#' @param topN "all" or numeric vector, determine how many topN shows in each column
#' @param width width
#' @param str_width str_width
#'
#' @export
#'
#' @import ggplot2 dplyr
#' @return ggplot or htmlwidget
#' @examples
#' \donttest{
#' data.frame(a = c("a", "a", "b", "b", "c"), aa = rep("a", 5), b = c("a", LETTERS[2:5]), c = 1:5) %>%
#'     my_sankey(., "gg", num = TRUE)
#' data(otutab)
#' cbind(taxonomy, num = rowSums(otutab))[1:10, ] -> test
#' my_sankey(test)
#' }
my_sankey <- function(test, mode = c("sankeyD3", "ggsankey"), topN = "all", space = 1, width = 0.1, str_width = 20, ...) {
    mode <- match.arg(mode, c("sankeyD3", "ggsankey"))
    test <- as.data.frame(test)
    nc <- ncol(test)
    if (nc < 3) stop("as least 3-columns dataframe")
    if (!is.numeric(test[, nc])) stop("the last column must be numeric")
    if (!identical(topN, "all")) {
        test <- gettop(test, topN)
    }

    target <- weight <- x <- node <- value <- next_x <- next_node <- label <- NULL
    if (mode == "sankeyD3") {
        lib_ps("sankeyD3", library = FALSE)
        # change duplicated data
        for (i in 1:(nc - 1)) {
            test[, i] <- paste0(test[, i], strrep(" ", i - 1))
        }
        # merge to two columns
        links <- data.frame()
        for (i in 1:(nc - 2)) {
            test[, c(i, i + 1, nc)] -> tmp
            colnames(tmp) <- c("source", "target", "weight")
            tmp <- group_by(tmp, source, target) %>% summarise(weight = sum(weight), .groups = "keep")
            links <- rbind(links, tmp)
        }
        # give ids
        nodes <- data.frame(name = c(as.character(links$source), as.character(links$target)) %>% unique())
        links$IDsource <- match(links$source, nodes$name) - 1
        links$IDtarget <- match(links$target, nodes$name) - 1

        p <- sankeyD3::sankeyNetwork(
            Links = as.data.frame(links), Nodes = nodes,
            Source = "IDsource", Target = "IDtarget", Value = "weight",
            NodeID = "name", nodeWidth = 10, units = "TWh",
            xAxisDomain = colnames(test)[-nc],
            # height=400,width=500,
            # colourScale=JS("d3.scaleOrdinal(d3.schemeCategory10);"),
            # numberFormat=".0f",
            # fontSize = 8,dragY = TRUE,nodeShadow = TRUE,
            # doubleclickTogglesChildren = TRUE,
            ...
        )

        return(p)
    }
    if (mode == "ggsankey") {
        lib_ps("ggsankey", library = FALSE)
        df <- ggsankey::make_long(test, 1:(nc - 1), value = !!nc)
        parms <- list(...)

        if (!is.null(parms$num)) {
            if ((parms$num)) {
                df %>%
                    group_by(x, node) %>%
                    summarise(value = sum(value)) %>%
                    mutate(label = paste0(node, "\n", value)) -> tmp
                df <- left_join(df, tmp[, -3])
            } else {
                df$label <- df$node
            }
        } else {
            df$label <- df$node
        }

        p <- ggplot(df, aes(
            x = x, next_x = next_x, node = node, next_node = next_node,
            label = stringr::str_wrap(label, width = str_width), fill = factor(node), value = value
        )) +
            ggsankey::geom_sankey(flow.alpha = .6, node.color = "gray30", space = space, width = width) +
            ggsankey::geom_sankey_text(size = 3, color = "black", space = space) +
            ggsankey::theme_sankey(base_size = 18) +
            labs(x = NULL) +
            scale_fill_manual(values = get_cols(nlevels(factor(df$node)))) +
            theme(legend.position = "none", plot.title = element_text(hjust = .5))
        return(p)
    }
}
