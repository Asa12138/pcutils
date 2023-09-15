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


#' Performs multiple mean comparisons for a data.frame
#'
#' @param df a data.frame
#' @param group The compare group (categories) in your data, one column name of metadata when metadata exist or a vector whose length equal to columns number of df.
#' @param metadata sample information dataframe contains group
#' @param method 	the type of test. Default is wilcox.test. Allowed values include:
#' \itemize{\item \code{\link[stats]{t.test}} (parametric) and \code{\link[stats]{wilcox.test}} (non-parametric). Perform comparison between two groups of samples. If the grouping variable contains more than two levels, then a pairwise comparison is performed.
#' \item \code{\link[stats]{anova}} (parametric) and \code{\link[stats]{kruskal.test}} (non-parametric). Perform one-way ANOVA test comparing multiple groups.}
#' @param threads default 1
#' @param p.adjust.method p.adjust.method, see \code{\link[stats]{p.adjust}}, default BH.
#' @param verbose logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' data(otutab)
#' group_test(otutab,metadata$Group,method="kruskal.test")
#' group_test(otutab[,1:12],metadata$Group[1:12],method="wilcox.test")
group_test=function(df,group,metadata=NULL,method="wilcox.test",
                    threads=1,p.adjust.method='BH',verbose=TRUE){
  i=NULL
  t1 <- Sys.time()
  if(verbose)pcutils::dabiao("Checking group")
  if(!is.null(metadata)){
    if(length(group)!=1)stop("'group' should be one column name of metadata when metadata exsit!")
    idx = rownames(metadata) %in% colnames(df)
    metadata = metadata[idx, , drop = FALSE]
    df = df[, rownames(metadata),drop=FALSE]
    if(verbose)message(nrow(metadata)," samples are matched for next step.")
    if(length(idx)<2)stop("too less common samples")
    sampFile = data.frame(group=metadata[, group], row.names = row.names(metadata))
  }
  else {
    if(length(group)!=ncol(df))stop("'group' length should equal to columns number of df when metadata is NULL!")
    sampFile =data.frame(row.names =colnames(df),group=group)
  }

  #df=df[rowSums(df)>0,]

  vs_group=levels(factor(sampFile$group))
  if(length(vs_group)==1)stop("'group' should be at least two elements factor")
  if(length(vs_group)>2){
    if(method%in%c("t.test","wilcox.test"))stop("'group' more than two elements, try 'kruskal.test' or 'anova'")}
  #calculate each
  tkodf=t(df)%>%as.data.frame()
  group=sampFile$group

  res.dt=data.frame("variable"=rownames(df))
  if(verbose)pcutils::dabiao("Calculating each variable")
  if(verbose)pcutils::dabiao("Using method: ",method)

  for (i in vs_group) {
    tmpdf=data.frame(average=apply(df[,which(group==i)],1,mean),sd=apply(df[,which(group==i)],1,sd))
    colnames(tmpdf)=paste0(colnames(tmpdf),"_",i)
    res.dt=cbind(res.dt,tmpdf)
  }
  if(length(vs_group)==2){
    res.dt$diff_mean=res.dt[,paste0("average_",vs_group[1])]-res.dt[,paste0("average_",vs_group[2])]
  }
  high_group <- apply(res.dt[,paste0("average_",vs_group)], 1, function(a) which(a == max(a))[[1]])
  res.dt$Highest=vs_group[high_group]

  #parallel
  reps=nrow(df)

  #main function
  loop=function(i){
    val <- tkodf[,i]
    if(method=="wilcox.test"){
      pval <- stats::wilcox.test(val~group)$p.value
    }
    if(method=="t.test"){
      pval <- stats::t.test(val~group)$p.value
    }
    if(method=="kruskal.test"){
      pval <- stats::kruskal.test(val~group)$p.value
    }
    if(method=="anova"){
      pval <- stats::lm(val~group) %>% stats::anova(.) %>%.$`Pr(>F)` %>% .[1]
    }
    if(verbose&(i%%100==0))message(paste(i,"done."))
    pval
  }

  {
    if(threads>1){
      pcutils::lib_ps("foreach","doSNOW","snow")
      pb <- utils::txtProgressBar(max =reps, style = 3)
      opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
      cl <- snow::makeCluster(threads)
      doSNOW::registerDoSNOW(cl)
      res <- foreach::foreach(i = 1:reps,.options.snow = opts
      ) %dopar% {
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
  res.dt$p.value=res

  t2 <- Sys.time()
  stime <- sprintf("%.3f", t2 - t1)
  resinfo <- paste0('Compared groups: ', paste(vs_group,collapse = ' and '), "\n",
                    'Total variable number: ', reps, "\n",
                    'Time use: ', stime, attr(stime, 'units'), "\n")

  message(resinfo)
  res.dt$p.adjust <- stats::p.adjust(res.dt$p.value, method = p.adjust.method)
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

#=====GAM=====
if(F){
  #使用 mgcv 包拟合广义加性模型
  library(mgcv)
  dat <- gamSim(1,n=400,dist="normal",scale=2)
  b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
  summary(b)

}


#' Get coefficients of linear regression model
#'
#' This function fits a linear regression model using the given data and formula, and returns the coefficients.
#'
#' @param data A data frame containing the response variable and predictors.
#' @param each each variable do a lm or whole multi-lm
#' @param formula A formula specifying the structure of the linear regression model.
#'
#' @return coefficients The coefficients of the linear regression model.
#' @export
#' @examples
#' data <- data.frame(response=c(2, 4, 6, 7, 9),
#'                    x1=c(1, 2, 3, 4, 5),
#'                    x2=c(2, 3, 6, 8, 9),
#'                    x3=c(3, 6, 5, 12, 12))
#' coefficients_df <- lm_coefficients(data, response ~ x1 + x2 + x3)
#' print(coefficients_df)
#' plot(coefficients_df)
lm_coefficients <- function(data, formula, each=TRUE) {
  if(each){
    # Get the response variable name
    response_name <- as.character(formula[[2]])
    # Get the predictor variable names
    data=model.frame(formula,data=data)
    data=trans(data,"standardize")
    predictor_names <- setdiff(colnames(data),response_name)
    coff=c()
    r2=adj_r2=c()
    for (i in predictor_names) {
      tmplm=lm(as.formula(paste0("`",response_name,"`~","`",i,"`")),data)
      tmpsumm=summary(tmplm)
      coff=rbind(coff,(tmpsumm$coefficients[2,]))
      r2=c(tmpsumm$r.squared,r2)
      adj_r2=c(tmpsumm$adj.r.squared,adj_r2)
    }
    coefficients_df=cbind(variable=predictor_names,as.data.frame(coff),r2,adj_r2)
    colnames(coefficients_df)[2]="coefficient"
    coefficients_df$type=ifelse(coefficients_df$coefficient > 0,"positive","negative")
  }
  else{
    #multi-lm-reg
    # Fit linear regression model
    lm_model <- lm(formula, data = data)
    # Extract coefficients
    coefficients <- coef(lm_model)
    # Create a data frame
    coefficients_df <- data.frame(variable = names(coefficients)[-1],
                                  coefficient = coefficients[-1],
                                  type=ifelse(coefficients[-1] > 0,"positive","negative"))
    sum_lm=summary(lm_model)
    attributes(coefficients_df)$df=sum_lm$df
    attributes(coefficients_df)$r2=sum_lm$r.squared
    attributes(coefficients_df)$adj_r2=sum_lm$adj.r.squared
    # Set the class of coefficients to "coefficients"
  }

  class(coefficients_df) <- c("coefficients","data.frame")
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
#' @return NULL
#' @exportS3Method
#' @method plot coefficients
plot.coefficients <- function(x, mode = 1,number=FALSE,x_order=NULL,...) {
  coefficients_df=x
  coefficients_df$variable=change_fac_lev(coefficients_df$variable,x_order)
  if (mode == 1) {
    # Bar chart mode
    p=ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
      geom_col(aes(fill = type)) +
      scale_fill_manual(values = c("negative" = "red2", "positive" = "green4"),
                        guide = guide_none()) +
      geom_hline(yintercept = 0,linetype=2,color="grey")+
      labs(x = "Variable", y = "Coefficient")+pcutils_theme
  } else if (mode == 2) {
    # Lollipop chart mode
    p=ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
      geom_segment(aes(xend = variable, yend = 0),color = "grey", size = 1) +
      geom_point(aes(color = type),size = 5) +
      scale_color_manual(values = c("negative" = "red2", "positive" = "green4"),
                         guide = guide_none()) +
      geom_hline(yintercept = 0,linetype=2,color="grey")+
      labs(x = "Variable", y = "Coefficient")+pcutils_theme
    if("adj_r2"%in%colnames(coefficients_df)){
      p=ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
        geom_segment(aes(xend = variable, yend = 0),color = "grey", size = 1) +
        geom_point(aes(color = type,size = adj_r2)) +
        scale_color_manual(values = c("negative" = "red2", "positive" = "green4"),
                           guide = guide_none()) +
        geom_hline(yintercept = 0,linetype=2,color="grey")+
        labs(x = "Variable", y = "Coefficient")+pcutils_theme
    }
  } else {
    stop("Invalid mode. Please specify mode = 1 for bar chart or mode = 2 for lollipop chart.")
  }
  if(number){
    p=p+geom_text(aes(y = coefficient+ifelse(coefficient>0,0.15,-0.15),label=round(coefficient,3)))
  }
  return(p)
}


#' Multiple regression/ variance decomposition analysis
#'
#' @param data dataframe
#' @param TopN give top variable importance
#' @param formula formula
#'
#' @export
#' @examples
#' data(otutab)
#' multireg(env1~Group*.,data = metadata[,2:7])
multireg<-function(formula,data,TopN=3){
  model.frame(formula,data=data)->metatbl
  colnames(metatbl)[1:2]<-c("test_v","xGroup")
  metatbl$xGroup<-factor(metatbl$xGroup)

  lib_ps("relaimpo","aplot",library = F)
  #multi-lm and correlation
  n_env_cor=list()
  n_env_lm=list()
  for (i in levels(metatbl$xGroup)){
    print(i)
    metadata=metatbl%>%dplyr::filter(xGroup==i)
    n_env_cor[[i]]=cor(metadata[,"test_v"],metadata[,-1:-2])
    mlm<-lm(test_v~.,cbind(metadata[,"test_v",drop=F],metadata[,-1:-2]))
    relaimpo::calc.relimp(mlm)->mlm_r
    n_env_lm[[i]]=mlm_r@lmg
  }

  do.call(rbind,n_env_cor)%>%as.data.frame%>%dplyr::mutate(xGroup=names(n_env_cor))->n_env_cor
  do.call(rbind,n_env_lm)%>%as.data.frame%>%dplyr::mutate(xGroup=names(n_env_lm))->n_env_lm

  reshape2::melt(n_env_cor,id.vars = "xGroup")->n_env_cor
  reshape2::melt(n_env_lm,id.vars = "xGroup")->n_env_lm
  n_env_lm%>%dplyr::group_by(xGroup)%>%dplyr::summarise(explained=sum(value))->n_explained
  n_env_lm%>%dplyr::group_by(xGroup)%>%dplyr::top_n(TopN)->n_env_lm1

  p1=ggplot(data =n_env_cor,aes(y=variable,x=xGroup))+
    geom_tile(aes(fill=value))+
    scale_fill_gradientn(name="Correlation",colours = bluered)+
    geom_point(data = n_env_lm1,aes(size=value),shape=21,fill=NA)+
    guides(size=guide_legend("Importance"))+labs(x=NULL,y=NULL)+pcutils_theme+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA))

  p2=ggpubr::ggbarplot(n_explained,x = "xGroup",y="explained",fill = "#4EA9E6")+
    scale_y_continuous(expand = c(0,0))+labs(x=NULL,y=NULL,subtitle ="Explained variation")+
    theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=11))

  p1%>%aplot::insert_top(p2,height = 0.3)
}
