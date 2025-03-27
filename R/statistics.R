# =====Data processing=====

#' Remove outliers
#'
#' @param x a numeric vector
#' @param factor default 1.5
#'
#' @export
#'
#' @return a numeric vector
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


#' Like `uniq -c` in shell to count a vector
#'
#' @param df two columns: first is type, second is number
#'
#' @export
#'
#' @return two columns: first is type, second is number
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

#' Group your data
#'
#' @param otutab data.frame
#' @param group group vector or one of colnames(metadata)
#' @param margin 1 for row and 2 for column(default: 2)
#' @param metadata metadata
#' @param act do (default: mean)
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#' data(otutab)
#' hebing(otutab, metadata$Group)
#' hebing(otutab, "Group", metadata = metadata, act = "sum")
hebing <- function(otutab, group, margin = 2, act = "mean", metadata = NULL) {
  # prepare otutab
  if (!is.null(metadata)) {
    if (length(group) != 1 | (!group[1] %in% colnames(metadata))) stop("group should be one of colnames(metadata)")
    if (margin == 2) {
      match_res <- match_df(otutab, metadata)
      otutab <- match_res$otutab
      group <- match_res$metadata[, group, drop = TRUE]
    } else {
      match_res <- match_df(t2(otutab), metadata)
      otutab <- t2(match_res$otutab)
      group <- match_res$metadata[, group, drop = TRUE]
    }
  }

  if (margin == 2) {
    stats::aggregate(t(otutab), FUN = act, by = list(factor(group))) -> a
    a[, -1, drop = FALSE] -> a
    data.frame(t2(a), check.names = FALSE) -> a
    levels(factor(group)) -> colnames(a)
  } else {
    stats::aggregate(otutab, FUN = act, by = list(factor(group))) -> a
    a[, -1, drop = FALSE] -> a
    levels(factor(group)) -> rownames(a)
  }
  return(a)
}

#' Group your data
#'
#' @param otutab data.frame
#' @param group_df group data.frame with two columns (id and group). The same ID can be mapped to multiple groups.
#' @param margin 1 for row and 2 for column(default: 2)
#' @param act do (default: mean)
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#' data(otutab)
#' hebing2(otutab, data.frame(id = c("NS1", "NS2", "NS1", "NS3"), group = c("A", "A", "B", "B")))
hebing2 <- function(otutab, group_df, margin = 2, act = "mean") {
  stopifnot(is.data.frame(group_df) & ncol(group_df) == 2)
  colnames(group_df) <- c("id", "group")

  if (margin == 2) {
    otutab <- t2(otutab)
    as.character(rownames(otutab)) -> otutab$id
    tmpdf <- dplyr::inner_join(group_df, otutab, by = "id")
    b <- pcutils::hebing(dplyr::select(tmpdf, -c("id", "group")), tmpdf$group, 1, act = act)
    b <- t2(b)
  } else {
    as.character(rownames(otutab)) -> otutab$id
    tmpdf <- dplyr::inner_join(group_df, otutab, by = "id")
    b <- pcutils::hebing(dplyr::select(tmpdf, -c("id", "group")), tmpdf$group, 1, act = act)
  }
  return(b)
}

#' Filter your data
#'
#' @param tab dataframe
#' @param sum the rowsum should bigger than sum (default:10)
#' @param exist the exist number bigger than exist (default:1)
#'
#' @return input object
#' @export
#'
#' @examples
#' data(otutab)
#' guolv(otutab)
guolv <- function(tab, sum = 10, exist = 1) {
  tab[rowSums(tab) > sum, ] -> tab
  tab[rowSums(tab > 0) > exist, ] -> tab
  return(tab)
}

#' Remove the low relative items in each column
#'
#' @param otutab otutab
#' @param relative_threshold threshold, default: 1e-4
#'
#' @export
#' @return data.frame
#' @examples
#' data(otutab)
#' rm_low(otutab)
rm_low <- function(otutab, relative_threshold = 1e-4) {
  # colSums(otutab)%>%summary()

  f_mpa <- otutab
  f_mpa_r <- as.data.frame(apply(f_mpa, 2, function(x) x / sum(x))) # 正确形式
  # f_mpa_r=f_mpa/colSums(f_mpa),错误,不能这样除
  f_mpa[f_mpa_r < relative_threshold] <- 0
  f_mpa <- f_mpa[rowSums(f_mpa) > 0, ]
  f_mpa
}

#' Trans format your data
#'
#' @param df dataframe
#' @param method "cpm","minmax","acpm","total","log", "max", "frequency", "normalize", "range", "rank", "rrank",
#' "standardize", "pa", "chi.square", "hellinger", "log", "clr", "rclr", "alr"
#' @param margin 1 for row and 2 for column(default: 2)
#' @param ... additional
#' @return data.frame
#' @export
#' @examples
#' data(otutab)
#' trans(otutab, method = "cpm")
#'
#' @seealso \code{\link[vegan]{decostand}}
trans <- function(df, method = "normalize", margin = 2, ...) {
  all <- c(
    "cpm", "minmax", "acpm", "total", "log1", "max", "frequency", "normalize", "range", "rank", "rrank",
    "standardize", "pa", "chi.square", "hellinger", "log", "clr", "rclr", "alr"
  )
  if (is.vector(df)) df <- data.frame(value = df)
  if (!method %in% all) stop("methods should be one of ", paste0(all, collapse = ", "))
  if (method == "cpm") {
    df <- apply(df, margin, \(x){
      x * 10**6 / sum(x)
    })
  } else if (method == "minmax") {
    df <- apply(df, margin, mmscale, ...)
  } else if (method == "acpm") {
    df <- asinh(apply(df, margin, \(x){
      x * 10**6 / sum(x)
    }))
  } else if (method == "log1") {
    df <- log(df + 1, ...)
  } else {
    lib_ps("vegan", library = FALSE)
    df <- vegan::decostand(df, method = method, margin, ...)
  }
  return(data.frame(df, check.names = FALSE))
}

#' Min_Max scale
#'
#' @param x a numeric vector
#' @param min_s scale min
#' @param max_s scale max
#' @param n linear transfer for n=1; the slope will change if n>1 or n<1
#' @param plot whether plot the transfer?
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' x <- runif(10)
#' mmscale(x, 5, 10)
mmscale <- function(x, min_s = 0, max_s = 1, n = 1, plot = FALSE) {
  if (n <= 0) stop("n should bigger than 0")
  if (max(x) == min(x)) {
    return(rep(min_s, length(x)))
  }
  x2 <- ((x - min(x))^n)
  y <- min_s + (x2) / (max(x2)) * (max_s - min_s)
  if (plot) plot(y ~ x)
  y
}


#' Split Composite Names
#'
#' @param x character vector
#' @param split character to split each element of vector on, see \code{\link[base]{strsplit}}
#' @param colnames colnames for the result
#' @param ... other arguments are passed to \code{\link[base]{strsplit}}
#'
#' @return data.frame
#' @export
#'
#' @examples
#' strsplit2(c("a;b", "c;d"), ";", colnames = c("col1", "col2"))
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

#' Transpose data.frame
#'
#' @param data data.frame
#' @return data.frame
#' @export
t2 <- function(data) {
  as.data.frame(t(data), optional = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Explode a data.frame if there are split charter in one column
#'
#' @param df data.frame
#' @param column column
#' @param split split string
#'
#' @export
#'
#' @return data.frame
#' @examples
#' \donttest{
#' df <- data.frame(a = 1:2, b = c("a,b", "c"), c = 3:4)
#' explode(df, "b", ",")
#' }
explode <- function(df, column, split = ",") {
  df <- tidyr::as_tibble(df)
  df[[column]] <- strsplit(df[, column, drop = TRUE], split = split)
  tidyr::unnest(df, dplyr::all_of(column)) %>% as.data.frame()
}

#' Squash one column in a data.frame using other columns as id.
#'
#' @param df data.frame
#' @param column column name, not numeric position
#' @param split split string
#'
#' @return data.frame
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1:2, 1:2), b = letters[1:4])
#' squash(df, "b", ",")
squash <- function(df, column, split = ",") {
  stats::aggregate(stats::reformulate(".", response = column), df, paste, collapse = split)
}

#' Convert Three-column Data to Distance Matrix
#'
#' This function converts a data frame with three columns (from, to, count) into a distance matrix.
#' The rows and columns of the matrix are all unique names from the 'from' and 'to' columns,
#' and the matrix values are filled with counts.
#'
#' @param data A data frame containing three columns: from, to, count.
#' @return A distance matrix where rows and columns are all unique names from 'from' and 'to' columns.
#' @export
#' @examples
#' data <- data.frame(
#'   from = c("A", "A", "B", "D"),
#'   to = c("B", "C", "A", "B"),
#'   count = c(1, 2, 3, 4)
#' )
#' df2distance(data)
df2distance <- function(data) {
  # Get all unique node names
  nodes <- unique(c(data$from, data$to))

  # Create an empty distance matrix
  distance_matrix <- matrix(0, nrow = length(nodes), ncol = length(nodes), dimnames = list(nodes, nodes))

  # Fill the distance matrix according to the data
  for (i in 1:nrow(data)) {
    from <- data$from[i]
    to <- data$to[i]
    count <- data$count[i]

    # Fill the count value into the corresponding position
    distance_matrix[from, to] <- count
  }
  # 如果需要对称矩阵，可以通过下面的代码实现
  distance_matrix[upper.tri(distance_matrix)] <- distance_matrix[upper.tri(distance_matrix)] + t(distance_matrix)[upper.tri(distance_matrix)]
  distance_matrix[lower.tri(distance_matrix)] <- t(distance_matrix)[lower.tri(distance_matrix)]

  return(distance_matrix)
}

#' Convert a distance matrix to a data frame
#'
#' This function converts a distance matrix into a data frame with three columns: from, to, count.
#' The rows and columns of the matrix are all unique names from the 'from' and 'to' columns,
#'
#' @param distance_matrix A distance matrix where rows and columns are all unique names from 'from' and 'to' columns.
#' @return A data frame containing three columns: from, to, count.
#' @export
#' @examples
#' distance_matrix <- matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8), nrow = 3)
#' distance2df(distance_matrix)
distance2df <- function(distance_matrix) {
  distance_matrix <- as.matrix(distance_matrix)
  df <- reshape2::melt(distance_matrix)
  colnames(df) <- c("from", "to", "count")
  df
}

#' Prepare a numeric string
#'
#' @param str  a string contain ',' and '-'
#' @param split_str split_str ","
#' @param continuous_str continuous_str "-"
#'
#' @return vector
#' @export
#'
#' @examples
#' pre_number_str("a1,a3,a5,a6-a10")
pre_number_str <- function(str, split_str = ",", continuous_str = "-") {
  # 将字符串拆分为单词
  words <- strsplit(str, split_str)[[1]]

  # 提取非数字前缀
  non_numeric_prefix <- sub("\\d+", "", strsplit(str, paste0(split_str, "|", continuous_str))[[1]])

  if (!all(non_numeric_prefix == non_numeric_prefix[1])) stop("Prefix is not consistent.")

  # 提取数字部分
  numeric_parts <- gsub(non_numeric_prefix[1], "", words)

  # 将连续范围展开成完整的向量
  expanded_numbers <- unlist(lapply(strsplit(numeric_parts, continuous_str), function(x) {
    if (length(x) == 2) {
      seq(as.numeric(x[1]), as.numeric(x[2]))
    } else {
      as.numeric(x)
    }
  }))

  # 将数字转换回字符串，并加上前缀
  result <- paste0(non_numeric_prefix[1], expanded_numbers)
  result
}


#' df to link table
#'
#' @param test df with at least 3 columns
#' @param fun function to summary the elements number, defalut: `sum`, you can choose `mean`.
#' @return data.frame
#' @export
#' @examples
#' data(otutab)
#' cbind(taxonomy, num = rowSums(otutab))[1:10, ] -> test
#' df2link(test)
df2link <- function(test, fun = sum) {
  from <- to <- weight <- NULL

  if (!is.numeric(test[, ncol(test)])) test$weight <- 1
  nc <- ncol(test)
  colnames(test)[nc] <- "weight"
  if (nc < 3) stop("as least 3-columns dataframe")
  # change duplicated data
  # if need tree, use `before_tree()`

  # merge to two columns
  links <- data.frame()
  tmp_fun_df <- stats::aggregate(test$weight, by = list(test[, 1, drop = TRUE]), fun)
  nodes <- data.frame(
    name = tmp_fun_df[["Group.1"]], level = colnames(test)[1],
    weight = tmp_fun_df[["x"]]
  )

  for (i in 1:(nc - 2)) {
    test[, c(i, i + 1, nc)] -> tmp
    colnames(tmp) <- c("from", "to", "weight")
    tmp <- dplyr::group_by(tmp, from, to) %>% dplyr::summarise(weight = fun(weight), .groups = "keep")
    tmp <- na.omit(tmp)
    links <- rbind(links, tmp)

    tmp_fun_df <- stats::aggregate(tmp$weight, by = list(tmp$to), fun)
    nodes <- rbind(nodes, data.frame(
      name = tmp_fun_df[["Group.1"]], level = colnames(test)[i + 1],
      weight = tmp_fun_df[["x"]]
    ))
  }
  return(list(links = data.frame(links), nodes = nodes))
}

# ======= Statistical test======

#' Two-group test
#'
#' @param var numeric vector
#' @param group two-levels group vector
#'
#' @export
#' @return No return value
#' @examples
#' twotest(runif(20), rep(c("a", "b"), each = 10))
twotest <- function(var, group) {
  group <- factor(group)
  print(stats::t.test(var ~ group)) # parameter
  dabiao("")
  print(stats::wilcox.test(var ~ group)) # non-parameter
  dabiao("")
  print(stats::ks.test(var ~ group))
}


#' Multi-groups test
#'
#' @param var numeric vector
#' @param group more than two-levels group vector
#' @param return return which method result (tukeyHSD or LSD or wilcox?)
#' @param print whether print the result
#'
#' @return No value or a dataframe.
#' @description
#' anova (parametric) and kruskal.test (non-parametric). Perform one-way ANOVA test comparing multiple groups.
#' LSD and TukeyHSD are post hoc test of anova.
#' dunn and nemenyi are post hoc test of kruskal.test.
#' t.test or wilcox is just perform t.test or wilcox.test in each two group (no p.adjust).
#'
#' @export
#'
#' @examples
#' if (requireNamespace("multcompView")) {
#'   multitest(runif(30), rep(c("A", "B", "C"), each = 10), return = "wilcox")
#' }
multitest <- function(var, group, print = TRUE, return = FALSE) {
  methods <- c("LSD", "TukeyHSD", "dunn", "nemenyi", "wilcox.test", "t.test")
  if (is.character(return)) {
    return <- match.arg(return, methods)
    print <- FALSE
  }
  if (print) {
    return <- methods
  }

  lib_ps("multcompView", library = FALSE)
  group <- factor(group)
  means <- stats::aggregate(var, by = list(group), mean)$x
  names(means) <- levels(group)
  ano <- stats::aov(var ~ group)
  kw <- stats::kruskal.test(var ~ group)
  ntr <- nlevels(group)

  p_res <- rep(1, choose(ntr, 2))
  names(p_res) <- combn(levels(group), 2) %>%
    split(col(.)) %>%
    sapply(paste, collapse = "-")

  # LSD
  if ("LSD" %in% return) {
    lib_ps("agricolae", library = FALSE)
    lsdres <- agricolae::LSD.test(ano, "group", p.adj = "bonferroni")
  }
  if (identical(return, "LSD")) {
    return(data.frame(lsdres$groups, variable = rownames(lsdres$groups)))
  }

  # TukeyHSD
  if ("TukeyHSD" %in% return) tukeyres <- stats::TukeyHSD(ano)

  # 2023.11.23发现orderPvalue有问题，它默认评估均值最大组和其他组的差异，认为没差异的都是相同字母，但实际上可能最大组和最小组没差异，另外两组有差异。
  # 换成multcompView::multcompLetters
  if (identical(return, "tukeyHSD")) {
    p_ord_res <- multcompView::multcompLetters(tukeyres$group[, 4])$Letters
    return(data.frame(means = means, groups = p_ord_res[names(means)], variable = names(means), row.names = names(means)))
  }

  # dunn
  if ("dunn" %in% return) {
    lib_ps("PMCMRplus", library = FALSE)
    dunnres <- PMCMRplus::kwAllPairsDunnTest(var ~ group)
  }
  if (identical(return, "dunn")) {
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        p_res[paste(gi, gj, sep = "-")] <- dunnres$p.value[gj, gi]
      }
    }
    p_ord_res <- multcompView::multcompLetters(p_res)$Letters
    return(data.frame(means = means, groups = p_ord_res[names(means)], variable = names(means), row.names = names(means)))
  }

  # nemenyi
  if ("nemenyi" %in% return) {
    lib_ps("PMCMRplus", library = FALSE)
    nemenyires <- PMCMRplus::kwAllPairsNemenyiTest(var ~ group)
  }
  if (identical(return, "nemenyi")) {
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        p_res[paste(gi, gj, sep = "-")] <- nemenyires$p.value[gj, gi]
      }
    }
    p_ord_res <- multcompView::multcompLetters(p_res)$Letters
    return(data.frame(means = means, groups = p_ord_res[names(means)], variable = names(means), row.names = names(means)))
  }

  # each t-test
  if ("t.test" %in% return) {
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        p_res[paste(gi, gj, sep = "-")] <- stats::t.test(var[which(group %in% c(gi, gj))] ~ group[which(group %in% c(gi, gj))])$p.value
      }
    }
    p_res[is.nan(p_res)] <- 1
    ttest_p <- p_res
    if (identical(return, "t.test")) {
      p_ord_res <- multcompView::multcompLetters(p_res)$Letters
      return(data.frame(means = means, groups = p_ord_res[names(means)], variable = names(means), row.names = names(means)))
    }
  }
  # each wilcox.test
  if ("wilcox.test" %in% return) {
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        p_res[paste(gi, gj, sep = "-")] <- stats::wilcox.test(var[which(group %in% c(gi, gj))] ~ group[which(group %in% c(gi, gj))])$p.value
      }
    }
    p_res[is.nan(p_res)] <- 1
    wilcox_p <- p_res
    if (identical(return, "wilcox.test")) {
      p_ord_res <- multcompView::multcompLetters(p_res)$Letters
      return(data.frame(means = means, groups = p_ord_res[names(means)], variable = names(means), row.names = names(means)))
    }
  }

  if (print) {
    dabiao("1.ANOVA:")
    print(summary(ano))
    dabiao("1-1.LSDtest, bonferroni p-adj:")
    print(lsdres$groups)
    dabiao("1-2.tukeyHSD:")
    print(tukeyres)
    dabiao("2.Kruskal.test:")
    print(kw)
    dabiao("2-1.Dunn.test:")
    print(dunnres)
    dabiao("2-2.Nemenyi.test:")
    print(nemenyires)
    dabiao("3.Wilcox-test:")
    print(wilcox_p)
    dabiao("4.t.test:")
    print(ttest_p)
  }
}


#' Performs multiple mean comparisons for a data.frame
#'
#' @param df a data.frame
#' @param group The compare group (categories) in your data, one column name of metadata when metadata exist or a vector whose length equal to columns number of df.
#' @param metadata sample information dataframe contains group
#' @param method 	the type of test. Default is wilcox.test. Allowed values include:
#' \itemize{\item \code{\link[stats]{t.test}} (parametric) and \code{\link[stats]{wilcox.test}} (non-parametric). Perform comparison between two groups of samples. If the grouping variable contains more than two levels, then a pairwise comparison is performed.
#' \item \code{\link[stats]{anova}} (parametric) and \code{\link[stats]{kruskal.test}} (non-parametric). Perform one-way ANOVA test comparing multiple groups.
#' \item \code{\link[stats]{chisq.test}},  performs chi-squared contingency table tests and goodness-of-fit tests.
#' \item 'pearson', 'kendall', or 'spearman' (correlation), see \code{\link[stats]{cor}}.}
#' @param pattern a named vector matching the group, e.g. c('G1'=1,'G2'=3,'G3'=2), use the correlation analysis with specific pattern to calculate p-value.
#' @param threads default 1
#' @param p.adjust.method p.adjust.method, see \code{\link[stats]{p.adjust}}, default BH.
#' @param verbose logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' data(otutab)
#' group_test(otutab, metadata$Group, method = "kruskal.test")
#' group_test(otutab[, 1:12], metadata$Group[1:12], method = "wilcox.test")
group_test <- function(df, group, metadata = NULL, method = "wilcox.test", pattern = NULL,
                       p.adjust.method = "none", threads = 1, verbose = TRUE) {
  i <- NULL
  t1 <- Sys.time()

  method <- match.arg(method, c("t.test", "wilcox.test", "kruskal.test", "anova", "pearson", "kendall", "spearman", "lm", "chisq.test", "none"))
  if (!is.null(pattern)) {
    if (!method %in% c("pearson", "kendall", "spearman")) {
      stop("method should be one of \"pearson\", \"kendall\", \"spearman\" when you specify a pattern")
    }
  }

  if (verbose) {
    pcutils::dabiao("Checking group")
  }
  if (!is.null(metadata)) {
    if (length(group) != 1) {
      stop("'group' should be one column name of metadata when metadata exsit!")
    }
    idx <- rownames(metadata) %in% colnames(df)
    metadata <- metadata[idx, , drop = FALSE]
    df <- df[, rownames(metadata), drop = FALSE]
    if (verbose) {
      message(nrow(metadata), " samples are matched for next step.")
    }
    if (length(idx) < 2) {
      stop("too less common samples")
    }
    sampFile <- data.frame(group = metadata[, group], row.names = row.names(metadata))
  } else {
    if (length(group) != ncol(df)) {
      stop("'group' length should equal to columns number of df when metadata is NULL!")
    }
    sampFile <- data.frame(row.names = colnames(df), group = group)
  }

  if (method %in% c("chisq.test")) {
    if (verbose) dabiao("Using chisq.test, transforming data to 0-1 table")
    df <- trans(df, method = "pa")
  }

  if (verbose) {
    pcutils::dabiao("Removing all-zero rows: ", sum(rowSums(abs(df)) == 0))
  }
  df <- df[rowSums(abs(df)) > 0, ]
  # calculate each
  if (verbose) {
    pcutils::dabiao("Calculating each feature")
  }
  if (verbose) {
    pcutils::dabiao("Using method: ", method)
  }
  tkodf <- t(df) %>%
    as.data.frame()
  group <- sampFile$group
  group2 <- NULL
  if (method %in% c("pearson", "kendall", "spearman", "lm")) {
    if (is.null(pattern)) {
      if (verbose) {
        message("Using correlation analysis: ", method, ", the groups will be transform to numeric, note the factor levels of group.")
      }
      if (is.numeric(group)) {
        group2 <- group
      } else {
        group2 <- as.numeric(factor(group))
      }
    } else {
      if (is.numeric(group)) {
        # stop('Can not use 'pattern' when group is a numeric variable.')
        if ((!is.numeric(pattern)) | length(group) != length(pattern)) {
          stop("pattern should be a numeric vector whose length equal to the group, e.g. c(1,2,3,5,3)")
        }
      } else {
        if ((!is.numeric(pattern)) | (is.null(names(pattern))) | (length(pattern) != nlevels(factor(group)))) {
          stop("pattern should be a named numeric vector matching the group, e.g. c(\"G1\"=1.5,\"G2\"=0.5,\"G3\"=2)")
        }
        group2 <- pattern[group]
      }
      if (verbose) {
        message("Using pattern")
      }
    }
  }

  res.dt <- data.frame(ID = rownames(df), row.names = rownames(df))

  if (is.numeric(sampFile$group)) {
    # stop('group should be a category variable.')
    vs_group <- "Numeric variable"
    if (!method %in% c("pearson", "kendall", "spearman", "lm")) {
      stop("group is a numeric variable, try \"pearson\", \"kendall\", \"spearman\" method")
      res.dt$cor <- cor(tkodf, group2, method = method)[, 1]
    }
  } else {
    vs_group <- levels(factor(sampFile$group))
    if (length(vs_group) == 1) {
      stop("'group' should be at least two elements factor")
    }
    if (length(vs_group) > 2) {
      if (method %in% c("t.test", "wilcox.test")) {
        stop("group\" more than two elements, try \"kruskal.test\", \"anova\" or \"pearson\", \"kendall\", \"spearman\"")
      }
    }

    for (i in vs_group) {
      tmpdf <- data.frame(
        average = apply(df[, which(group == i), drop = FALSE], 1, mean),
        sd = apply(df[, which(group == i), drop = FALSE], 1, sd)
      )
      colnames(tmpdf) <- paste0(colnames(tmpdf), "_", i)
      res.dt <- cbind(res.dt, tmpdf)
    }
    if (length(vs_group) == 2) {
      # update, make sure the control group is first one.
      res.dt$diff_mean <- res.dt[, paste0("average_", vs_group[2])] - res.dt[, paste0("average_", vs_group[1])]
    }
    high_group <- apply(res.dt[, paste0("average_", vs_group)], 1, function(a) which(a == max(a))[[1]])
    res.dt$Highest <- vs_group[high_group]
  }
  if (method %in% c("pearson", "kendall", "spearman")) {
    res.dt$cor <- cor(tkodf, group2, method = method)[, 1]
  }
  # parallel
  reps <- nrow(df)

  tkodf <- t(df)
  # main function
  loop <- function(i) {
    val <- tkodf[, i]
    if (method == "none") {
      return(NA)
    }
    if (method == "wilcox.test") {
      pval <- stats::wilcox.test(val ~ group)$p.value
    }
    if (method == "t.test") {
      if (sd(val) == 0) {
        pval <- 1
      } else {
        pval <- stats::t.test(val ~ group)$p.value
      }
    }
    if (method == "chisq.test") {
      if (length(unique(val)) < 2) {
        pval <- 1
      } else {
        pval <- stats::chisq.test(val, group)$p.value
      }
    }
    if (method == "kruskal.test") {
      pval <- stats::kruskal.test(val ~ group)$p.value
    }
    if (method == "anova") {
      pval <- stats::lm(val ~ group) %>%
        stats::anova(.) %>%
        .$`Pr(>F)` %>%
        .[1]
    }
    if (method == "lm") {
      # pval <- stats::lm(val~group) %>% stats::anova(.) %>%.$`Pr(>F)` %>% .[1]
      lm_res <- stats::lm(val ~ group)
      lm_res_summ <- summary(lm_res)
      lm_res_anova <- stats::anova(lm_res)
      r2 <- lm_res_summ %>%
        .$adj.r.squared %>%
        .[1]
      pval <- c(lm_res$coefficients[2], r2, as.numeric(lm_res_anova[1, ]))
    }
    if (method %in% c("pearson", "kendall", "spearman")) {
      # 用p-value还是直接效应量呢？?  Pval <- 1-abs(stats::cor(val,group2,method = method))
      pval <- stats::cor.test(val, group2, method = method)$p.value
    }
    if (verbose & (i %% 1000 == 0)) {
      message(i, " features done.")
    }
    if (is.na(pval)) {
      pval <- 1
    }
    pval
  }

  {
    if (threads > 1) {
      pcutils::lib_ps("foreach", "doSNOW", "snow", library = FALSE)
      if (verbose) {
        pb <- utils::txtProgressBar(max = reps, style = 3)
        opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
      } else {
        opts <- NULL
      }
      cl <- snow::makeCluster(threads)
      doSNOW::registerDoSNOW(cl)
      res <- foreach::`%dopar%`(
        foreach::foreach(
          i = seq_len(reps), .options.snow = opts,
          .export = c("group", "group2")
        ),
        suppressWarnings(loop(i))
      )
      snow::stopCluster(cl)
      gc()
    } else {
      res <- suppressWarnings(lapply(seq_len(reps), loop))
    }
  }
  # simplify method
  if (method == "lm") {
    res <- do.call(rbind, res)
    colnames(res) <- c("b-coefficient", "adj-r2", "df", "sum_sq", "mean_sq", "F-value", "p.value")
    res.dt <- cbind(res.dt, res)
  } else {
    res <- do.call(c, res)
    res.dt$p.value <- res
  }

  t2 <- Sys.time()
  stime <- sprintf("%.3f", t2 - t1)

  res.dt$p.adjust <- stats::p.adjust(res.dt$p.value, method = p.adjust.method)

  resinfo <- paste0(
    "\nCompared groups: ", paste(vs_group, collapse = ", "), "\n", "Total KO number: ", reps, "\n", "Compare method: ", method, "\n", "Time use: ",
    stime, attr(stime, "units"), "\n"
  )
  if (verbose) message(resinfo)

  attributes(res.dt)$vs_group <- vs_group
  attributes(res.dt)$method <- method
  attributes(res.dt)$p.adjust.method <- p.adjust.method
  attributes(res.dt)$pattern <- pattern
  return(res.dt)
}

#' Fit a distribution
#'
#' @param a a numeric vector
#'
#' @export
#' @return distribution
fittest <- function(a) {
  lib_ps("fitdistrplus", "nortest", library = FALSE)
  if (is.vector(a)) {
    dabiao("1.Basic plot")
    plot(a)
    dabiao("2.QQ plot")
    stats::qqnorm(a, col = "red", main = "a")
    stats::qqline(a, col = "blue")
    # fitdistrplus package multiple distribution judgment package multiple distribution judgment
    dabiao("3.fitdistrplus plot")
    fitdistrplus::descdist(a)
    message("use fitdis(a) to test which distribution. e.g:fitdis(a,'norm')")
    # Whether the statistical test is normal (goodness of fit test)
    # （1）Shapiro-Wilks test：
    stats::shapiro.test(a) |> print()
    # （2）Kolmogorov-Smirnov(K-S test)
    stats::ks.test(a, "pnorm", mean = mean(a), sd = sqrt(stats::var(a))) |> print()
    # （3）Cramer-Von Mises test（cvm.test）
    nortest::cvm.test(a) |> print()
    # （4）Anderson Darling test
    nortest::ad.test(a) |> print()
  }
}


#' Get coefficients of linear regression model
#'
#' This function fits a linear regression model using the given data and formula, and returns the coefficients.
#'
#' @param data A data frame containing the response variable and predictors.
#' @param each each variable do a lm or whole multi-lm
#' @param formula A formula specifying the structure of the linear regression model.
#' @param standardize Whether to standardize the data before fitting the model.
#'
#' @return coefficients The coefficients of the linear regression model.
#' @export
#' @examples
#' data <- data.frame(
#'   response = c(2, 4, 6, 7, 9),
#'   x1 = c(1, 2, 3, 4, 5),
#'   x2 = c(2, 3, 6, 8, 9),
#'   x3 = c(3, 6, 5, 12, 12)
#' )
#' coefficients_df <- lm_coefficients(data, response ~ x1 + x2 + x3)
#' print(coefficients_df)
#' plot(coefficients_df)
lm_coefficients <- function(data, formula, standardize = FALSE, each = TRUE) {
  if (each) {
    # Get the response variable name
    response_name <- as.character(formula[[2]])
    # Get the predictor variable names
    data <- model.frame(formula, data = data)
    if (standardize) data <- trans(data, "standardize")
    predictor_names <- setdiff(colnames(data), response_name)
    coff <- c()
    r2 <- adj_r2 <- c()
    for (i in predictor_names) {
      tmplm <- lm(as.formula(paste0("`", response_name, "`~", "`", i, "`")), data)
      tmpsumm <- summary(tmplm)
      coff <- rbind(coff, (tmpsumm$coefficients[2, ]))
      r2 <- c(tmpsumm$r.squared, r2)
      adj_r2 <- c(tmpsumm$adj.r.squared, adj_r2)
    }
    coefficients_df <- cbind(variable = predictor_names, as.data.frame(coff), r2, adj_r2)
    colnames(coefficients_df)[2] <- "coefficient"
    coefficients_df$type <- ifelse(coefficients_df$coefficient > 0, "positive", "negative")
  } else {
    # multi-lm-reg
    # Fit linear regression model
    lm_model <- lm(formula, data = data)
    # Extract coefficients
    coefficients <- coef(lm_model)
    # Create a data frame
    coefficients_df <- data.frame(
      variable = names(coefficients)[-1],
      coefficient = coefficients[-1],
      type = ifelse(coefficients[-1] > 0, "positive", "negative")
    )
    sum_lm <- summary(lm_model)
    attributes(coefficients_df)$df <- sum_lm$df
    attributes(coefficients_df)$r2 <- sum_lm$r.squared
    attributes(coefficients_df)$adj_r2 <- sum_lm$adj.r.squared
    # Set the class of coefficients to "coefficients"
  }

  class(coefficients_df) <- c("coefficients", "data.frame")
  # Return the coefficients
  return(coefficients_df)
}

#' Plot coefficients as a bar chart or lollipop chart
#'
#' @description
#' This function takes the coefficients and generates a plot to visualize their magnitudes.
#'
#' @param x The coefficients to be plotted.
#' @param number show number
#' @param mode The mode of the plot: 1 for bar chart, 2 for lollipop chart.
#' @param x_order order of variables
#' @param ... add
#'
#' @return ggplot
#' @exportS3Method
#' @method plot coefficients
#' @seealso [lm_coefficients]
plot.coefficients <- function(x, mode = 1, number = FALSE, x_order = NULL, ...) {
  variable <- coefficient <- type <- adj_r2 <- NULL

  coefficients_df <- x
  coefficients_df$variable <- change_fac_lev(coefficients_df$variable, x_order)
  if (mode == 1) {
    # Bar chart mode
    p <- ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
      geom_col(aes(fill = type)) +
      scale_fill_manual(
        values = c("negative" = "red2", "positive" = "green4"),
        guide = guide_none()
      ) +
      geom_hline(yintercept = 0, linetype = 2, color = "grey") +
      labs(x = "Variable", y = "Coefficient") +
      pcutils_theme
  } else if (mode == 2) {
    # Lollipop chart mode
    p <- ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
      geom_segment(aes(xend = variable, yend = 0), color = "grey", size = 1) +
      geom_point(aes(color = type), size = 5) +
      scale_color_manual(
        values = c("negative" = "red2", "positive" = "green4"),
        guide = guide_none()
      ) +
      geom_hline(yintercept = 0, linetype = 2, color = "grey") +
      labs(x = "Variable", y = "Coefficient") +
      pcutils_theme
    if ("adj_r2" %in% colnames(coefficients_df)) {
      p <- ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
        geom_segment(aes(xend = variable, yend = 0), color = "grey", size = 1) +
        geom_point(aes(color = type, size = adj_r2)) +
        scale_color_manual(
          values = c("negative" = "red2", "positive" = "green4"),
          guide = guide_none()
        ) +
        geom_hline(yintercept = 0, linetype = 2, color = "grey") +
        labs(x = "Variable", y = "Coefficient") +
        pcutils_theme
    }
  } else {
    stop("Invalid mode. Please specify mode = 1 for bar chart or mode = 2 for lollipop chart.")
  }
  if (number) {
    p <- p + geom_text(aes(y = coefficient + ifelse(coefficient > 0, 0.15, -0.15), label = round(coefficient, 3)))
  }
  return(p)
}


#' Multiple regression/ variance decomposition analysis
#'
#' @param data dataframe
#' @param TopN give top variable importance
#' @param formula formula
#' @return ggplot
#' @export
#' @examples
#' if (requireNamespace("relaimpo") && requireNamespace("aplot")) {
#'   data(otutab)
#'   multireg(env1 ~ Group * ., data = metadata[, 2:7])
#' }
multireg <- function(formula, data, TopN = 3) {
  xGroup <- value <- explained <- variable <- NULL
  model.frame(formula, data = data) -> metatbl
  colnames(metatbl)[1:2] <- c("test_v", "xGroup")
  metatbl$xGroup <- factor(metatbl$xGroup)

  lib_ps("relaimpo", "aplot", library = FALSE)
  # multi-lm and correlation
  n_env_cor <- list()
  n_env_lm <- list()
  for (i in levels(metatbl$xGroup)) {
    print(i)
    metadata <- metatbl %>% dplyr::filter(xGroup == i)
    n_env_cor[[i]] <- cor(metadata[, "test_v"], metadata[, -1:-2])
    mlm <- lm(test_v ~ ., cbind(metadata[, "test_v", drop = FALSE], metadata[, -1:-2]))
    relaimpo::calc.relimp(mlm) -> mlm_r
    n_env_lm[[i]] <- mlm_r@lmg
  }

  do.call(rbind, n_env_cor) %>%
    as.data.frame() %>%
    dplyr::mutate(xGroup = names(n_env_cor)) -> n_env_cor
  do.call(rbind, n_env_lm) %>%
    as.data.frame() %>%
    dplyr::mutate(xGroup = names(n_env_lm)) -> n_env_lm

  reshape2::melt(n_env_cor, id.vars = "xGroup") -> n_env_cor
  reshape2::melt(n_env_lm, id.vars = "xGroup") -> n_env_lm
  n_env_lm %>%
    dplyr::group_by(xGroup) %>%
    dplyr::summarise(explained = sum(value)) -> n_explained
  n_env_lm %>%
    dplyr::group_by(xGroup) %>%
    dplyr::top_n(TopN) -> n_env_lm1

  p1 <- ggplot(data = n_env_cor, aes(y = variable, x = xGroup)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradientn(name = "Correlation", colours = get_cols(pal = "bluered")) +
    geom_point(data = n_env_lm1, aes(size = value), shape = 21, fill = NA) +
    guides(size = guide_legend("Importance")) +
    labs(x = NULL, y = NULL) +
    pcutils_theme +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )

  p2 <- ggplot(n_explained, aes(x = xGroup, y = explained)) +
    geom_bar(stat = "identity", fill = "#4EA9E6", color = "black", width = 0.7) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = NULL, y = NULL, subtitle = "Explained variation") +
    pcutils_theme +
    theme(
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 11)
    )

  p1 %>% aplot::insert_top(p2, height = 0.3)
}
