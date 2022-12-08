# Some useful tools
# pls=c("ggplot2","dplyr","magrittr","fitdistrplus","ggpmisc","ggpubr","nortest","agricolae")
#   for (p in pls) {
#     if (!requireNamespace(p)) {
#            install.packages(p)}}

#' Print with =
#' @param str output strings
#' @param n the number of = side chars
#'
dabiao<-function(str,n=80){
  if(n<nchar(str))n=nchar(str)+2
  x=(n-nchar(str))%/%2
  xx=paste0(paste(rep('=',x),collapse = ""),str,paste(rep('=',x),collapse = ""))
  cat(xx,"\n")
}


#' Library packages or install
#'
#' @param p_list a vector of packages list
#'
#' @return NULL
#' @export
#'
#' @examples
#' lib_ps("ggplot2","dplyr")
lib_ps<-function(p_list,...){
  p_list=c(p_list,...)
  for (p in p_list) {
    if (!requireNamespace(p)) {
      print(paste0(p,": this package haven't install, should install?"))
      flag=readline("yes/no(y/n)?")
      if(tolower(flag)%in%c("yes","y")){
        install.packages(p)
      }
      else stop(paste0("exit, because '",p,"' need to install"))

      if (!requireNamespace(p)){
        print(paste0(p," is not available, try Bioconductor?"))
        flag=readline("yes/no(y/n)?")
        if(tolower(flag)%in%c("yes","y")){
          if (!requireNamespace("BiocManager"))install.packages("BiocManager")
          BiocManager::install(p)
        }
        else stop(paste0("exit, because '",p,"' need to install"))
      }

      if (!requireNamespace(p))stop("please try other way (github...) to install ",p)
    }
    suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
}


#' @title Min_Max scale
#' @param x a numeric vector
#' @param min_s scale min
#' @param max_s scale max
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' x=runif(10)
#' mmscale(x,5,10)
mmscale=function(x,min_s=0,max_s=1){
  min_s+(x-min(x))/(max(x)-min(x))*(max_s-min_s)
}

#' @title Three-line table
#'
#' @param aa a dataframe
#' @param digits how many digits should remain
#' @param nrow show how many rows
#' @param ncol show how many columns
#' @param ... additional arguments e.g.(rowname=NULL)
#'
#' @import ggpubr dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#' data("otutab")
#' sanxian(otutab)
sanxian<-function(aa,digits = 3,nrow=10,ncol=10,...){
  lib_ps("ggpubr","dplyr")
  if(nrow(aa)>nrow)aa<-aa[1:nrow,,drop=F]
  if(ncol(aa)>ncol)aa<-aa[,1:ncol,drop=F]
  aa%>%mutate_if(is.numeric,\(x)round(x,digits = digits))%>%
    ggtexttable(...,theme = ttheme("blank")) %>%
    tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 3)%>%
    tab_add_hline(at.row = nrow(aa)+1, row.side = "bottom", linewidth = 3)->p
  return(p)
}


#' Plot a multi pages pdf
#' @param plist plot list
#'
#' @param file prefix of your .pdf file
#' @param width width
#' @param height height
#' @param ... additional arguments
#' @export
plotpdf<-function(plist,file='new',width=8,height=7,...){
  pdf(paste0(file,'.pdf'),width,height,...)
  for (i in plist){
    print(i)
  }
  dev.off()
}


#' Get n colors
#'
#' @param n how many colors you need
#' @param pal a vector of colors, you can get from here too.{RColorBrewer::brewer.pal(5,"Set2")} {ggsci::pal_aaas()(5)}
#'
#' @return n colors
#' @export
#'
#' @examples
#' get_cols(10)->my_cols
#' scales::show_col(my_cols)
get_cols <- function (n,pal=NULL){
  col <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
                    "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
                    "#ccebc5", "#ffed6f")
  col2 <- c("#1f78b4", "#ffff33", "#c2a5cf", "#ff7f00", "#810f7c",
                     "#a6cee3", "#006d2c", "#4d4d4d", "#8c510a", "#d73027",
                     "#78c679", "#7f0000", "#41b6c4", "#e7298a", "#54278f")
  col3 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                    "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
                    "#ffff99", "#b15928")

   if(is.null(pal))pal=col3
   return(colorRampPalette(pal)(n))
}

#' Add a global gg_theme and colors for plots
#'
#' @param set_theme your theme
#'
#' @return NULL
#' @export
#'
#' @examples
#' add_theme()
add_theme<-function(set_theme=NULL){
  if(is.null(set_theme)){
    mytheme<- {theme(text = element_text(family = "sans", size = 14))+
        theme(plot.margin=unit(rep(0.75,4),'lines'))}
    if (requireNamespace("ggpubr")){
      library(ggpubr)
      mytheme<- {theme_pubr(base_size = 14,legend = "right")+
          theme(plot.margin=unit(rep(0.75,4),'lines'))}
    }
  }
  else {
    stopifnot(inherits(set_theme,c("theme","gg")))
    mytheme<-set_theme
  }
  mytheme<<-mytheme
}


#' @title Plot a boxplot
#'
#' @param tab your dataframe
#' @param col which column choose for value
#' @param metadata the dataframe contains the group
#' @param group which colname choose for group or a vector
#' @param alpha whether plot a group alphabeta
#' @param method test method:tukeyHSD,LSD, (default: tukeyHSD)
#' @param rain plot a raincloud?
#' @param p_value show the p-value between each group?(when group number is 2~4)
#'
#' @return a 'ggplot' plot object,
#' @export
#'
#' @examples
#' a=data.frame(a=1:18,b=runif(18,0,5))
#' group_box(a,col = 2,group = rep(c("a","b","c"),each=6))
#' group_box(a,col = 1,group = rep(c("a","b","c"),each=6),alpha=F,rain=T,each=T)
#'
group_box<-function(tab,col=NULL,metadata=NULL,group=NULL,alpha=F,method="tukeyHSD",rain=F,p_value=F){
  lib_ps("ggplot2","dplyr","ggpubr")
  if(is.vector(tab))tab=data.frame(value=tab)
  if(is.null(col))col=1
  if(is.character(col)){
    col=which(names(tab)==col)
    if(length(col)==0)stop("col is not in colnames")
  }
  if(is.null(metadata)&&is.null(group)){
    #a single boxplot
    md<-data.frame(value=tab[,col],group=names(tab)[col])
  }
  else{
    if(is.null(metadata)&&!is.null(group)){
      md<-data.frame(value=tab[,col],group=group)
    }
    else if ((!is.null(metadata)&&!is.null(group))){
      tab<-tab[rownames(metadata),]
      md<-data.frame(value=tab[,col],group=metadata[,group])
      g_name=group
    }
  }
  md$group<-factor(md$group)
  max(md$value)->high;min(md$value)->low
  p<-ggplot(md,aes(group,value,color=group,group=group))+
    stat_boxplot(geom = "errorbar",width=0.15)+
    geom_boxplot(outlier.shape = NA)+
    geom_jitter(width = 0.15,alpha=0.8,size=0.5)+
    ylab(label = NULL)+xlab(label = NULL)
  if(rain){
    lib_ps("gghalves")
    p<-ggplot(md,aes(group,value,color=group,group=group))+
      gghalves::geom_half_violin(aes(fill=group),color=NA, side = "l", trim=FALSE)+
      gghalves::geom_half_point(side="r", size=0.5, alpha=0.8) +
      geom_boxplot(fill = NA,
                   position=position_nudge(x=.22),
                   linewidth = 0.6,
                   width = 0.2,
                   outlier.shape = NA
      )+ylab(label = NULL)+xlab(label = NULL)
  }

  if(exists("g_name"))p=p+guides(color=guide_legend(g_name),
                                 fill=guide_legend(g_name))

  if(p_value){
    if(between(nlevels(md$group),2,4)){
    p=p+stat_compare_means(show.legend = FALSE,
                           comparisons = combn(levels(md$group),2)%>%split(col(.)))
  }
    else p=p+stat_compare_means(show.legend = FALSE,label.x = 1,label.y = (high+0.2*(high-low)))
    }

  if(alpha){
    multitest(md$value,md$group,print = F,return = method)->a
    if(rain){
      p=p+ geom_text(data = a,aes(x=variable,y=(high+0.15*(high-low)),label=groups),
                     inherit.aes = FALSE,color='red',size=5,position=position_nudge(x=.1))
    }
    else {p=p+ geom_text(data = a,aes(x=variable,y=(high+0.05*(high-low)),label=groups),
                         inherit.aes = FALSE,color='red',size=5)
    }
  }
  if(exists("mytheme"))p=p+mytheme
  return(p)
}

#' Two-group test
#'
#' @param var numeric vector
#' @param group two-levels group vector
#'
#' @export
#'
#' @examples
#'twotest(runif(20),rep(c("a","b"),each=10))
twotest<-function(var,group){
  group<-factor(group)
  print(t.test(var~group))#parameter
  print('===================================================')
  print(wilcox.test(var~group))#non-parameter
  print('===================================================')
  print(ks.test(var~group))
}

#' Multi-group test
#'
#' @param var numeric vector
#' @param group more than two-levels group vector
#' @param return return which method result (tukeyHSD or LSD or wilcox?)
#'
#' @importFrom agricolae LSD.test
#' @export
#'
#' @examples
#' multitest(c(runif(20),runif(10,2,3)),rep(c("a","b","c"),each=10))
#' invisible(multitest(runif(30),rep(c("a","b","c"),each=10))->aa)
multitest<-function(var,group,print=T,return=F){
  lib_ps("agricolae")
  group<-factor(group)
  ano<-aov(var~group)
  #LSD
  lsdres <- LSD.test(ano, 'group', p.adj = 'bonferroni')
  #TukeyHSD
  tukeyres<-TukeyHSD(ano)
  means=aggregate(var,by=list(group),mean)$x
  ntr=nlevels(group)
  Q <- matrix(1, ncol = ntr, nrow = ntr)
  Q[lower.tri(Q)]<-Q[upper.tri(Q)] <- tukeyres$group[,4]
  out<-agricolae::orderPvalue(levels(group),means,0.05,Q)
  #each wilcox.test
  Q <- matrix(1, ncol = ntr, nrow = ntr)
  for(i in 1:(ntr-1)){
    for(j in (i+1):ntr){
      gi=levels(group)[i];gj=levels(group)[j]
      w<-wilcox.test(var[which(group%in%c(gi,gj))]~group[which(group%in%c(gi,gj))])
      Q[j,i]<-Q[i,j]<-w$p.value
    }
  }
  rownames(Q)<-colnames(Q)<-levels(group)
  out1<-agricolae::orderPvalue(levels(group),means,0.05,Q)

  if(print){
    print("ANOVA:")
    print(summary(ano))
    print('=================================================')
    print(kruskal.test(var~group))
    print('=================================================')
    print('LSDtest分组,bonferroni p-adj:')
    print(lsdres$groups)
    print('=================================================')
    print(tukeyres)
    print('===Wilcox-test====================================')
    print(Q)
  }
  if(return=="LSD")return(data.frame(lsdres$groups,variable=rownames(lsdres$groups)))
  if(return=="tukeyHSD")return(data.frame(out,variable=rownames(out)))
  if(return=="wilcox")return(data.frame(out1,variable=rownames(out1)))
}

#' Fit a distribution
#'
#' @param a a numeric vector
#'
#' @export
#' @examples
#'a=runif(50)
#'fittest(a)
fittest<-function(a){
  if(is.vector(a)){
    #肉眼看
    plot(a)
    #QQ图
    qqnorm(a,col="red",main = "a");qqline(a,col="blue")
    #fitdistrplus包多个分布判断包多个分布判断
    fitdistrplus::descdist(a)
    print("use fitdis(a) to test which distribution. e.g:fitdis(a,'norm')")
    #统计学检验是否正态（拟合优度检验）
    #（1）Shapiro-Wilks检验：
    shapiro.test(a)|>print()
    #（2）Kolmogorov-Smirnov(K-S检验)
    ks.test(a,"pnorm",mean=mean(a),sd=sqrt(var(a)))|>print()
    #（3）Cramer-Von Mises检验（cvm.test）
    nortest::cvm.test(a)|>print()
    #（4）Anderson Darling检验
    nortest::ad.test(a)|>print()
  }

}

#' Plot a donut chart
#'
#' @param ad a two-columns dataframe, first is type, second is number
#'
#' @import ggplot2 dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#'a=data.frame(type=letters[1:6],num=c(1,3,3,4,5,10))
#'gghuan(a)
#'b=data.frame(type=letters[1:12],num=c(1,3,3,4,15,10,35,2:6))
#'gghuan(b)

gghuan<-function(tab,reorder=T,mode="1"){
  if(ncol(tab)>2)stop("need two columns: first is type, second is number")
  colnames(tab)[1]->g_name
  colnames(tab)<-c("type","n")
  plot_df<-tab%>%group_by(type)%>%
    summarise(mean=mean(n))%>%mutate(fraction=mean/sum(mean))

  if(reorder){
    plot_df$type=reorder(plot_df$type,plot_df$mean)
    plot_df<-arrange(plot_df,mean)
  }
  plot_df$ymax = cumsum(plot_df$fraction)
  plot_df$ymin = c(0, head(plot_df$ymax, n = -1))
  plot_df$rate_per<-paste(as.character(round(100*plot_df$fraction,1)),'%',sep='')
  if(mode=="1"){plt<-ggplot(data = plot_df, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 3.2, xmin = 1.7)) +
    geom_rect(alpha=0.8) +xlim(c(0, 5)) +
    coord_polar(theta = "y") +
    geom_text(aes(x = 3.6, y = ((ymin+ymax)/2),label = type) ,size=4)+
    geom_text(aes(x = 2.5, y = ((ymin+ymax)/2),label = rate_per) ,size=3.6,col="white")
  }
  if(mode=="2"){plt <- ggplot(plot_df,aes(x = type,y = fraction,fill = type)) +
    geom_col(position = "dodge2",show.legend = TRUE,alpha = .9
    ) + coord_polar()+ylim(-min(plot_df$fraction),max(plot_df$fraction)+0.2)+
    geom_text(aes(x=type,y=fraction+0.1,label = paste0(type,"\n",rate_per)) ,size=4)
  }
  plt+theme_light() +
    labs(x = "", y = "",fill=g_name) +
    theme(panel.grid=element_blank()) + ## 去掉白色外框
    theme(axis.text=element_blank()) + ## 把图旁边的标签去掉
    theme(axis.ticks=element_blank()) + ## 去掉左上角的坐标刻度线
    theme(panel.border=element_blank(),legend.position = "none")## 去掉最外层的正方形边框
}

#' Fit a lm and plot
#'
#' @param y a numeric vector or a two-columns dataframe
#' @param x a numeric vector or NULL
#' @import ggpmisc
#' @return a ggplot
#' @export
#'
#' @examples
#'my_lm(runif(50),1:50)
#'my_lm(c(1:50)+runif(50,0,5),1:50)
my_lm<-function(y,x=NULL,...){
  lib_ps("ggpmisc")
  if(!is.null(x))y=data.frame(y=y,x=x)
  y1=y
  colnames(y)[1:2]=c("y","x")
  if(!is.numeric(y[,1])|!is.numeric(y[,2]))stop("need numeric")
  #model <- lm(y ~ x, data = y)
  ggplot(y,aes(x,y))+geom_point(...)+
    geom_smooth(method = "lm",color="red",se = F,formula = "y~x")+
    ggpmisc::stat_poly_eq(
      aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = '~~~~~')),
      formula = y ~ x,  parse = TRUE,color="red",
      size = 3, #公式字体大小
      label.x = 0.05, label.y = 1.05) +   #位置 ，0-1之间的比例
      labs(x=colnames(y1)[2],y=colnames(y1)[1])
}

#' transfer Geographical latitude and longitude to XY(m)
#'
#' @param dat a two-columns dataframe, first is latitude, second is longitude
#'
#' @export
#'
#' @examples
#'data.frame(row.names = letter[1:18],x=runif(18,30,35),y=runif(18,40,45))->geo
#'toXY(geo)
toXY <- function(dat){
  lib_ps("SoDA")
  XY <- geoXY(dat[,1], dat[,2])
  return(as.data.frame(row.names = rownames(dat),XY))
}

#https://cloud.tencent.com/developer/article/1751856
#' Plot china map
#'
#' @param dir where to put the china.json file
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' china_map()
china_map<-function(dir="~/Downloads/"){
  lib_ps("ggspatial","ggplot2","sf")
  china_shp=paste0(dir,"china.json")
  if(!file.exists(china_shp))download.file("https://gitcode.net/mirrors/lyhmyd1211/geomapdata_cn/-/raw/master/china.json?inline=false",china_shp)
  china <- sf::read_sf(china_shp)

  ggplot()+
    #geom_point(aes(x=long,y=lat,col=env1))+
    geom_sf(data = china,fill=pcutils::get_cols(35),linewidth=1,color="black")+
    #coord_sf(xlim = c(110,125),ylim = c(30,40))+
    annotation_scale(location = "bl") +
    #geom_text_repel(aes(x=long,y=lat,label=Id),size=3)+
    # spatial-aware automagic north arrow
    annotation_north_arrow(location = "tl", which_north = "false",
                           style = north_arrow_fancy_orienteering)+
    theme(
      #aspect.ratio = 1.25, #调节长宽比
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill=NA,color="grey10",linetype=1,size=1.),
      plot.margin=unit(c(0,0,0,0),"mm"))
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
#' matrix(letters[1:6],2,3)|>as.data.frame()->a
#' grepl.data.frame("c",a)
#' grepl.data.frame("\\w",a)

grepl.data.frame<-function(pattern, x, ...) {
  y <- if (length(x)) {
    do.call("cbind", lapply(x, "grepl", pattern = pattern,...))
  }
  else {
    matrix(FALSE, length(row.names(x)), 0)
  }
  if (.row_names_info(x) > 0L)
    rownames(y) <- row.names(x)
  y
}

