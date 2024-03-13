# Some function depends on packages not in CRAN.
# when merge main branch to CRAN, delete this file and the following packages in DESCRIPTION:

# ggcor,
# pheatmap,
# corrplot,
# ggradar,
# ggsankey,
# sankeyD3,
# SoDA,
# ggtern,
# ggnewscale,
# ggtree,
# ape

#' Transfer Geographical latitude and longitude to XY(m)
#'
#' @param geo a two-columns dataframe, first is latitude, second is longitude
#'
#' @export
#' @return data.frame
#' @examples
#' \donttest{
#' if(interactive()){
#' data.frame(row.names = letters[1:18], x = runif(18, 30, 35), y = runif(18, 40, 45)) -> geo
#' toXY(geo)
#' }
#' }
toXY <- function(geo) {
    lib_ps("SoDA", library = FALSE)
    XY <- SoDA::geoXY(geo[, 1], geo[, 2])
    # geosphere::distm
    return(as.data.frame(row.names = rownames(geo), XY))
}

# ========Common plots=======

#' Heatmap by ggplot
#'
#' @param otutab otutab
#' @param pal the main color pal, a vector of colors
#' @param scale "none", "row", "column"
#' @param row_annotation row annotation
#' @param col_annotation column annotation
#' @param rowname show row names?
#' @param colname show column names?
#' @param row_cluster cluster the row?
#' @param col_cluster cluster the column?
#' @param annotation_pal the annotation color pal, a list. e.g. list(Group=c("red","blue"))
#' @param tile_params tile_params parsed to \code{\link[ggplot2]{geom_tile}}
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' if(interactive()){
#' data(otutab)
#' ggheatmap(otutab[1:30, ],
#'     scale = "row", row_annotation = otutab[1:30, 1:2],
#'     col_annotation = metadata[, c(2, 4)]
#' )
#' }
ggheatmap <- function(otutab, pal = NULL, scale = "none",
                      rowname = TRUE, colname = TRUE, tile_params = list(),
                      row_cluster = FALSE, col_cluster = FALSE,
                      row_annotation = NULL, col_annotation = NULL, annotation_pal = NULL) {
    lib_ps("ggnewscale", "aplot", "ggtree", "ape", "vegan", library = FALSE)
    sample <- otu <- value <- Id <- NULL
    if (is.null(pal)) {
        pal <- get_cols(pal = "bluered")
    } else if (length(is.ggplot.color(pal)) < 2) stop("pal is wrong!")

    otutab %>% as.data.frame() -> otutab
    rownames(otutab) <- as.character(rownames(otutab))
    colnames(otutab) <- as.character(colnames(otutab))
    otutab -> d
    if (scale == "row") {
        d <- trans(d, method = "standardize", margin = 1)
    } else if (scale == "column") d <- trans(d, method = "standardize", margin = 2)

    rownames(d) -> d$otu

    dd <- reshape2::melt(d, id.vars = "otu", variable.name = "sample")
    dd$otu <- factor(dd$otu, levels = rev(rownames(d)))
    dd$sample <- factor(dd$sample, levels = colnames(d))

    p <- ggplot(dd, aes(x = sample, y = otu, fill = value)) +
        do.call(geom_tile, tile_params) +
        scale_fill_gradientn(colours = pal) +
        scale_y_discrete(position = "right") +
        theme_minimal() +
        xlab(NULL) +
        ylab(NULL)

    if (!rowname) {
        p <- p + theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )
    }
    if (!colname) {
        p <- p + theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )
    }

    if (!is.null(row_annotation)) {
        ca1 <- row_annotation
        rownames(ca1) -> ca1$Id
        pc1 <- ggplot()
        for (i in 1:(ncol(ca1) - 1)) {
            tmp <- ca1[, c(i, ncol(ca1))]
            pd1 <- reshape2::melt(tmp, id.vars = "Id", variable.name = "sample")
            if (i > 1) pc1 <- pc1 + ggnewscale::new_scale_fill()
            pc1 <- pc1 +
                geom_tile(data = pd1, aes(y = Id, x = sample, fill = value)) +
                labs(fill = colnames(ca1)[i])
            if (!is.null(annotation_pal[[colnames(ca1)[i]]])) {
                if (is.numeric(pd1$value)) {
                    pc1 <- pc1 + scale_fill_gradientn(colours = annotation_pal[[colnames(ca1)[i]]])
                } else {
                    pc1 <- pc1 + scale_fill_manual(values = annotation_pal[[colnames(ca1)[i]]])
                }
            }
        }
        pc1 <- pc1 +
            theme_minimal() +
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            ) +
            xlab(NULL) + ylab(NULL)
        p <- p %>% aplot::insert_left(pc1, width = 0.05 * (ncol(ca1) - 1))
    }

    if (row_cluster) {
        hclust(dist(otutab)) %>% ape::as.phylo() -> a
        p <- p %>% aplot::insert_left(ggtree::ggtree(a, branch.length = "none"), width = .1)
    }

    if (!is.null(col_annotation)) {
        ca <- col_annotation
        rownames(ca) -> ca$Id

        pc <- ggplot()
        for (i in 1:(ncol(ca) - 1)) {
            tmp <- ca[, c(i, ncol(ca))]
            pd <- reshape2::melt(tmp, id.vars = "Id", variable.name = "sample")

            if (i > 1) pc <- pc + ggnewscale::new_scale_fill()
            pc <- pc +
                geom_tile(data = pd, aes(x = Id, y = sample, fill = value)) +
                labs(fill = colnames(ca)[i]) +
                scale_y_discrete(position = "right")
            if (!is.null(annotation_pal[[colnames(ca)[i]]])) {
                if (is.numeric(pd$value)) {
                    pc <- pc + scale_fill_gradientn(colours = annotation_pal[[colnames(ca)[i]]])
                } else {
                    pc <- pc + scale_fill_manual(values = annotation_pal[[colnames(ca)[i]]])
                }
            }
        }
        pc <- pc +
            theme_minimal() +
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            ) +
            xlab(NULL) + ylab(NULL)

        p <- p %>% aplot::insert_top(pc, height = 0.05 * (ncol(ca) - 1))
    }
    if (col_cluster) {
        hclust(dist(t(otutab))) %>% ape::as.phylo() -> b
        p <- p %>% aplot::insert_top(ggtree::ggtree(b, branch.length = "none") +
            ggtree::layout_dendrogram(), height = .1)
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
#' @return ggplot
#' @import ggplot2
#' @export
#'
#' @examples
#' \donttest{
#' if(interactive()){
#' data(otutab)
#' cor_plot(metadata[, 3:10])
#' cor_plot(metadata[, 3:10], mode = 2)
#' cor_plot(t(otutab)[, 1:50], mode = 3, heat = FALSE)
#' }
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
#' if(interactive()){
#' data(otutab)
#' tax_radar(otutab[1:6, 1:4])
#' }
#' }
tax_radar <- function(group_df, ...) {
    lib_ps("ggradar", library = FALSE)
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
    tmp <- NULL
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
#' if(interactive()){
#' data.frame(a = c("a", "a", "b", "b", "c"), aa = rep("a", 5), b = c("a", LETTERS[2:5]), c = 1:5) %>%
#'     my_sankey(., "gg", num = TRUE)
#' data(otutab)
#' cbind(taxonomy, num = rowSums(otutab))[1:10, ] -> test
#' my_sankey(test)
#' }
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

#' Triangle plot
#'
#' @param group_df group_df
#' @param class point color
#'
#' @export
#' @return ggplot
#' @examples
#' \donttest{
#' if(interactive()){
#' data(otutab)
#' hebing(otutab, metadata$Group, act = "mean") -> tmp
#' triangp(tmp, class = taxonomy$Phylum)
#' }
#' }
triangp <- function(group_df, class = NULL) {
    lib_ps("ggtern", library = FALSE)
    if (ncol(group_df) != 3) stop("ncol of group_df is not 3, can't plot trip")
    KO <- OE <- WT <- NULL
    tmp <- group_df
    tmp %>%
        as.data.frame() %>%
        mutate(sum = rowSums(.)) -> tmp1
    colnames(tmp1)[1:3] <- c("KO", "OE", "WT")

    if (is.null(class)) {
        p <- ggtern::ggtern(tmp1, aes(x = KO, y = OE, z = WT)) +
            geom_point(aes(size = sum)) + # define data geometry
            labs(x = names(tmp)[1], y = names(tmp)[2], z = names(tmp)[3])
        return(p)
    } else {
        tmp1$class <- class
        p <- ggtern::ggtern(tmp1, aes(x = KO, y = OE, z = WT)) +
            geom_point(aes(size = sum, col = class)) + # define data geometry
            labs(x = names(tmp)[1], y = names(tmp)[2], z = names(tmp)[3])
        return(p)
    }
}
