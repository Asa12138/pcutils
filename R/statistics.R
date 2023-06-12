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
  if ((max(x) - min(x)) == 0) {
    return(rep((min_s + max_s) / 2, length(x)))
  }
  x2 <- ((x - min(x))^n)
  y <- min_s + (x2) / (max(x2)) * (max_s - min_s)
  if (plot) plot(y ~ x)
  y
}


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
#' ttest or wilcox is just perform wilcox-test in each two group (no p.adjust).
#'
#' @export
#'
#' @examples
#' multitest(runif(30), rep(c("a", "b", "c"), each = 10), print = FALSE, return = "wilcox") -> aa
multitest <- function(var, group, print = TRUE, return = FALSE) {
  methods <- c("LSD", "TukeyHSD", "dunn", "nemenyi", "wilcox", "ttest")
  if (is.character(return)) {
    return <- match.arg(return, methods)
    print <- FALSE
  }
  if (print) {
    return <- methods
  }

  lib_ps("agricolae", library = FALSE)
  group <- factor(group)
  means <- stats::aggregate(var, by = list(group), mean)$x

  ano <- stats::aov(var ~ group)
  kw <- stats::kruskal.test(var ~ group)
  ntr <- nlevels(group)

  # LSD
  if ("LSD" %in% return) lsdres <- agricolae::LSD.test(ano, "group", p.adj = "bonferroni")
  if (identical(return, "LSD")) {
    return(data.frame(lsdres$groups, variable = rownames(lsdres$groups)))
  }

  # TukeyHSD
  if ("TukeyHSD" %in% return) tukeyres <- stats::TukeyHSD(ano)

  if (identical(return, "tukeyHSD")) {
    p_mat <- matrix(1, ncol = ntr, nrow = ntr)
    p_mat[lower.tri(p_mat)] <- p_mat[upper.tri(p_mat)] <- tukeyres$group[, 4]
    tukeyHSD_out <- agricolae::orderPvalue(levels(group), means, 0.05, p_mat)
    return(data.frame(tukeyHSD_out, variable = rownames(tukeyHSD_out)))
  }

  # dunn
  if ("dunn" %in% return){
    lib_ps("PMCMRplus", library = FALSE)
    dunnres <- PMCMRplus::kwAllPairsDunnTest(var ~ group)
  }
  if (identical(return, "dunn")) {
    p_mat <- matrix(1, ncol = ntr, nrow = ntr)
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        p_mat[j, i] <- p_mat[i, j] <- dunnres$p.value[gj, gi]
      }
    }
    rownames(p_mat) <- colnames(p_mat) <- levels(group)
    p_mat[is.nan(p_mat)] <- 1
    dunn_out <- agricolae::orderPvalue(levels(group), means, 0.05, p_mat)
    return(data.frame(dunn_out, variable = rownames(dunn_out)))
  }

  # nemenyi
  if ("nemenyi" %in% return){
    lib_ps("PMCMRplus", library = FALSE)
    nemenyires <- PMCMRplus::kwAllPairsNemenyiTest(var ~ group)
  }
  if (identical(return, "nemenyi")) {
    p_mat <- matrix(1, ncol = ntr, nrow = ntr)
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        p_mat[j, i] <- p_mat[i, j] <- nemenyires$p.value[gj, gi]
      }
    }
    rownames(p_mat) <- colnames(p_mat) <- levels(group)
    p_mat[is.nan(p_mat)] <- 1
    nemenyi_out <- agricolae::orderPvalue(levels(group), means, 0.05, p_mat)
    return(data.frame(nemenyi_out, variable = rownames(nemenyi_out)))
  }

  # each t-test
  if ("ttest" %in% return) {
    p_mat <- matrix(1, ncol = ntr, nrow = ntr)
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        w <- stats::t.test(var[which(group %in% c(gi, gj))] ~ group[which(group %in% c(gi, gj))])
        p_mat[j, i] <- p_mat[i, j] <- w$p.value
      }
    }
    rownames(p_mat) <- colnames(p_mat) <- levels(group)
    p_mat[is.nan(p_mat)] <- 1
    ttest_p <- p_mat
    ttest_out <- agricolae::orderPvalue(levels(group), means, 0.05, p_mat)
    if (identical(return, "ttest")) {
      return(data.frame(ttest_out, variable = rownames(ttest_out)))
    }
  }
  # each wilcox.test
  if ("wilcox" %in% return) {
    p_mat <- matrix(1, ncol = ntr, nrow = ntr)
    for (i in 1:(ntr - 1)) {
      for (j in (i + 1):ntr) {
        gi <- levels(group)[i]
        gj <- levels(group)[j]
        w <- stats::wilcox.test(var[which(group %in% c(gi, gj))] ~ group[which(group %in% c(gi, gj))])
        p_mat[j, i] <- p_mat[i, j] <- w$p.value
      }
    }
    rownames(p_mat) <- colnames(p_mat) <- levels(group)
    p_mat[is.nan(p_mat)] <- 1
    wilcox_p <- p_mat
    wilcox_out <- agricolae::orderPvalue(levels(group), means, 0.05, p_mat)
    if (identical(return, "wilcox")) {
      return(data.frame(wilcox_out, variable = rownames(wilcox_out)))
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
