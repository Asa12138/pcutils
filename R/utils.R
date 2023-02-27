# Some useful tools
# pls=c("ggplot2","dplyr","magrittr","fitdistrplus","ggpmisc","ggpubr","nortest","agricolae")
#   for (p in pls) {
#     if (!requireNamespace(p)) {
#            install.packages(p)}}



#=========little tools=========
#' Print with =
#'
#' @param str output strings
#' @param char side chars default:=
#' @param n the number of output length
#' @param mode "middle", "left" or "right"
#'
#' @export
dabiao<-function(str,n=80,char="=",mode=c("middle", "left", "right")){
  mode=match.arg(mode,c("middle", "left", "right"))
  if(n<nchar(str))n=nchar(str)+2
  x=(n-nchar(str))%/%2
  switch (mode,
          "left" = {xx=paste0(str,strrep(char,2*x))},
          "middle" = {xx=paste0(strrep(char,x),str,strrep(char,x))},
          "right" = {xx=paste0(strrep(char,2*x),str)}
  )
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
lib_ps<-function(p_list,...,all_yes=F){
  some_packages=c(
    "ggsankey"="davidsjoberg/ggsankey",
    "sankeyD3"="fbreitwieser/sankeyD3",
    "pctax"="Asa12138/pctax",
    "MetaNet"="Asa12138/MetaNet",
    "ggcor"="Github-Yilei/ggcor",
    "chorddiag"="mattflor/chorddiag"
  )
  p_list=c(p_list,...)
  for (p in p_list) {
    if (!requireNamespace(p)) {
      if(!all_yes){
      print(paste0(p,": this package haven't install, should install?"))
      flag=readline("yes/no(y/n)?")}
      else flag="y"

      if(tolower(flag)%in%c("yes","y")){
        if(p%in%names(some_packages))remotes::install_github(some_packages[p])
        else install.packages(p)
      }
      else stop(paste0("exit, because '",p,"' need to install"))

      if (!requireNamespace(p)){

        if(!all_yes){print(paste0(p," is not available, try Bioconductor?"))
        flag=readline("yes/no(y/n)?")}

        if(tolower(flag)%in%c("yes","y")){
          if (!requireNamespace("BiocManager"))install.packages("BiocManager")
          BiocManager::install(p)
        }
        else stop(paste0("exit, because '",p,"' need to install"))
      }

      if (!requireNamespace(p)){cat("\n");stop("please try other way (github...) to install ",p)}
    }

    suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
}

#' Detach packages
#'
#' @param p_list a vector of packages list
#' @export
del_ps<-function(p_list,...){
  p_list=c(p_list,...)
  p_list=paste0("package:",p_list)
  all=search()
  p_list=p_list[p_list%in%all]
  for (p in p_list){
    detach(p,character.only = T)
  }
}

#' @title Min_Max scale
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
#' x=runif(10)
#' mmscale(x,5,10)
mmscale<-function(x,min_s=0,max_s=1,n=1,plot=F){
  if(n<=0)stop("n should bigger than 0")
  if((max(x)-min(x))==0)return(rep((min_s+max_s)/2,length(x)))
  x2=((x-min(x))^n)
  y=min_s+(x2)/(max(x2))*(max_s-min_s)
  if(plot)plot(y~x)
  y
}

#' @title Three-line table
#'
#' @param aa a dataframe
#' @param digits how many digits should remain
#' @param nrow show how many rows
#' @param ncol show how many columns
#' @param ... additional arguments e.g.(rows=NULL)
#'
#' @import ggpubr dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#' data("otutab",package = "MetaNet")
#' sanxian(otutab)
#' sanxian(otutab,ncol=4,rows=NULL)
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

#' Transform a rgb vector to a Rcolor code
#'
#' @param x vector or three columns data.frame
#'
#' @return Rcolor code like "#69C404"
#' @export
#'
#' @examples
#' rgb2code(c(12,23,34))
#' rgb2code("#69C404",rev=T)
rgb2code<-function(x,rev=F){
  lib_ps("dplyr")
  if(rev){
    if(is.vector(x))col2rgb(x)%>%t()%>%as.vector()->A;names(A)=c("r","g","b");return(A)
    if(is.data.frame(x))apply(x, 1,col2rgb)%>%t()->A;colnames(A)=c("r","g","b");rownames(A)=rownames(x);return(A)
  }
  else{
    if(length(x)!=3)stop("need r,g,b!")
    names(x)=c("r","g","b")
    if(is.vector(x))return(rgb(x[1],x[2],x[3],maxColorValue = 255))
    if(is.data.frame(x))return(transmute(x,code=rgb(r,g,b,maxColorValue = 255)))
    }
}

#' Plot a multi-pages pdf
#'
#' @param plist plot list
#' @param file prefix of your .pdf file
#' @param width width
#' @param height height
#' @param brower the path of Google Chrome, Microsoft Edge or Chromium in your computer.
#' @param ... additional arguments
#'
#' @export
plotpdf<-function(plist,file='new',width=8,height=7,brower="/Applications/Microsoft\ Edge.app/Contents/MacOS/Microsoft\ Edge",...){
  if(inherits(plist,"htmlwidget")){
    lib_ps("pagedown")
    if(!file.exists(brower))stop(brower,"is not found in your computer, please give a right path for Google Chrome, Microsoft Edge or Chromium.")
    suppressMessages(htmlwidgets::saveWidget(plist,"tmppp.html"))
    pagedown::chrome_print("tmppp.html",paste0(file,'.pdf'),wait = 0,browser =brower,
                           options = list(pageRanges="1",paperWidth=width,paperHeight=height,...))
    file.remove("tmppp.html")
    message("pdf saved sucessfully")
  }
  else{
    pdf(paste0(file,'.pdf'),width,height,...)
    for (i in plist){
      print(i)
    }
    dev.off()}
}

#' Plot a gif
#'
#' @param plist plot list
#' @param file prefix of your .gif file
#'
#' @export
plotgif<-function(plist,file='new',mode="gif"){
  if(mode=="gif"){ animation::saveGIF(
      for (i in plist){
        print(i)
      },movie.name = paste0(file,".gif")
    )}
  #transfer pngs to a gif use gifski::gifski()
  if(mode=="html"){
    nwd=getwd()
    dir.create(paste0(file,"_html"))
    setwd(paste0(file,"_html"))
    animation::saveHTML(
    for (i in plist){
      print(i)
    },movie.name = paste0(file,".html")
    )
    setwd(nwd)
    }
}

#' Get n colors
#'
#' @param n how many colors you need
#' @param pal col1~3; or a vector of colors, you can get from here too.{RColorBrewer::brewer.pal(5,"Set2")} {ggsci::pal_aaas()(5)}
#' @param picture a picture file, colors will be extracted from the picture
#'
#' @return n colors
#' @export
#'
#' @examples
#' get_cols(10,"col2")->my_cols
#' scales::show_col(my_cols)
#' scales::show_col(get_cols(15,RColorBrewer::brewer.pal(5,"Set2")))
#' scales::show_col(get_cols(15,ggsci::pal_aaas()(5)))
#' #scales::show_col(get_cols(4,picture="~/Desktop/Screenshot 2023-02-14 at 16.17.28.png"))
get_cols <- function (n,pal="col1",picture=NULL){
  col1 <- c("#8dd3c7", "#ffed6f", "#bebada", "#fb8072", "#80b1d3",
                    "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
                    "#ccebc5")
  col2 <- c("#a6cee3", "#78c679","#c2a5cf", "#ff7f00","#1f78b4",  "#810f7c", "#ffff33",
                     "#006d2c", "#4d4d4d", "#8c510a", "#d73027",
                      "#7f0000", "#41b6c4", "#e7298a", "#54278f")
  col3 <- c("#a6bce3", "#fb9a99", "#fdbf6f", "#1f78b4", "#b2df8a", "#cab2d6", "#33a02c",
                    "#e31a1c", "#ff7f00", "#6a3d9a",
                    "#ffef00", "#b15928")

  if(length(pal)==1)pal=get(pal)
  if(!is.null(picture)){
    lib_ps("RImagePalette")
    type=tools::file_ext(picture)
    switch (type,
      "jpg" = {p1=jpeg::readJPEG(picture)},
      "png" = {p1=png::readPNG(picture)}
    )
    pal=RImagePalette::image_palette(p1,n = n)
  }
  if(length(pal)<n)return(colorRampPalette(pal)(n))
  return(pal[seq_len(n)])
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
        theme(plot.margin=unit(rep(0.5,4),'lines'),
              strip.background = element_rect(fill=NA))}
    if (requireNamespace("ggpubr")){
      library(ggpubr)
      mytheme<- {theme_pubr(base_size = 14,legend = "right")+
          theme(plot.margin=unit(rep(0.5,4),'lines'),
                strip.background = element_rect(fill=NA))}
    }
  }
  else {
    stopifnot(inherits(set_theme,c("theme","gg")))
    mytheme<-set_theme
  }
  mytheme<<-mytheme
}

#' Remove outliers
#'
#' @param x a numeric vector
#' @param factor default 1.5
#'
#' @export
#'
#' @examples
#' remove.outliers(c(1,10:15))
remove.outliers <- function(x, factor = 1.5) {
  q25 = quantile(x, probs = 0.25)
  q75 = quantile(x, probs = 0.75)
  iqr = unname(q75 - q25)
  lower.threshold = q25 - (iqr * factor)
  upper.threshold = q75 + (iqr * factor)
  return(x[(x >= lower.threshold) & (x <= upper.threshold)])
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

#' Read some special format file
#'
#' @param file file path
#' @param format "blast","diamond"
#'
#' @return data.frame
#' @export
#'
read.file<-function(file,format=NULL,just_print=F){
  if(just_print){
    if(file.size(file)>10000){
      print(paste0(file,": this file is a little big, still open?"))
      flag=readline("yes/no(y/n)?")
      if(tolower(flag)%in%c("yes","y")){
        cat(readr::read_file(file))
      }}
    else cat(readr::read_file(file))
  }
  else{
    if(is.null(format))format=tools::file_ext(file)
    format=match.arg(format,c("blast","diamond","jpg","png","pdf","svg"))

    if(format%in%c("blast","diamond")){
      df=read.table(file,sep = "\t",
                    col.names = c("Qseqid","Sseqid","Pident","Length","Mismatch","Gapopen",
                                  "Qstart","Qend","Sstart","Send","E_value","Bitscore"))
      return(df)
    }

    if(format%in%c("jpg","png")){
      switch (format,
              "jpg" = {p1=jpeg::readJPEG(file)},
              "png" = {p1=png::readPNG(file)}
      )
      graphics::plot(1:2, type = "n", axes = F, ylab = "n", xlab = "n",ann = FALSE)
      graphics::rasterImage(p1, 1, 1, 2, 2)
    }
    if(format=="svg"){
      x <- grImport2::readPicture(file)
      g <- grImport2::pictureGrob(x)
      p=as_ggplot(g)
      p
    }
  }
}


#' Transfer the format of file
#'
#' @param file input file
#' @param to_format transfer to
#' @param format input file format
#'
#' @return file at work directory
#' @export
#'
trans_format<-function(file,to_format,format=NULL,...,brower="/Applications/Microsoft\ Edge.app/Contents/MacOS/Microsoft\ Edge"){
  if(is.null(format))format=tools::file_ext(file)
  name=tools::file_path_sans_ext(basename(file))
  out=paste0(name,".",to_format)
  if(to_format=="jpeg")to_format="jpg"
  if(format==to_format)stop("don not need transfer")
  lib_ps("ggplot2")
  if(format=="svg"){
    if(to_format=="html"){file.copy(file,out)}
    else{
      lib_ps("rsvg","grImport2")
      rsvg::rsvg_svg(file, file)
      x <- grImport2::readPicture(file)
      g <- grImport2::pictureGrob(x)
      ggplot2::ggsave(g, filename = out, device = to_format,...)
      invisible(g)
    }
  }
  if(format=="pdf"){
    lib_ps("pdftools")
    switch (to_format,
            "png" = {pdftools::pdf_convert(file,"png",filenames = out)},
            "jpg" = {pdftools::pdf_convert(file,"jpeg",filenames = out)},
            "jpeg" = {pdftools::pdf_convert(file,"jpeg",filenames = out)}
    )
  }
  #https://phantomjs.org/download.html
  #PhantomJS
  if(format=="png"){
    img=png::readPNG(file)
    g <- grid::rasterGrob(img, interpolate=TRUE)
    p=qplot() +annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +theme_void()
    ggplot2::ggsave(p, filename = out, device = to_format,...)
    invisible(g)
  }
  if(format=="jpg"){
    img=jpeg::readJPEG(file)
    g <- grid::rasterGrob(img, interpolate=TRUE)
    p=qplot() +annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +theme_void()
    ggplot2::ggsave(p, filename = out, device = to_format,...)
    invisible(g)
  }
  if(format=="html"){
    if(to_format%in%c("pdf", "png", "jpeg")){
      pagedown::chrome_print(file,out,wait = 0,browser =brower,format = to_format,
                             options = list(#paperWidth=width,
                               #pageRanges="1",
                               #paperHeight=height,
                               ...))}
    if(to_format=="svg"){file.copy(file,out)}
  }

}


#' Download supplemental materials according to a doi
#'
#' @param doi doi
#' @param dir dir
#' @param bget_path your bget_path
#'
#' @export
#'
#'
get_doi<-function(doi,dir="~/Downloads/",bget_path="~/software/bget_0.3.2_Darwin_64-bit/bget"){
  if(!file.exists(bget_path))stop("Cann't find bget! check `bget_path`")
  doi=sub("https://doi.org/","",doi)
  command=paste0(bget_path," doi ",doi," -t2 --suppl --full-text -o ",dir)
  system(command)

}

#=========statistics==========

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
#' multitest(runif(30),rep(c("a","b","c"),each=10),print=F,return="wilcox")->aa
multitest<-function(var,group,print=T,return=F){
  lib_ps("agricolae")
  group<-factor(group)
  ano<-aov(var~group)
  #LSD
  lsdres <- LSD.test(ano, 'group', p.adj = 'bonferroni')
  if(return=="LSD")return(data.frame(lsdres$groups,variable=rownames(lsdres$groups)))
  #TukeyHSD
  tukeyres<-TukeyHSD(ano)
  means=aggregate(var,by=list(group),mean)$x
  ntr=nlevels(group)
  Q <- matrix(1, ncol = ntr, nrow = ntr)
  Q[lower.tri(Q)]<-Q[upper.tri(Q)] <- tukeyres$group[,4]
  out<-agricolae::orderPvalue(levels(group),means,0.05,Q)
  if(return=="tukeyHSD")return(data.frame(out,variable=rownames(out)))
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
  Q[is.nan(Q)]=1
  out1<-agricolae::orderPvalue(levels(group),means,0.05,Q)
  if(return=="wilcox")return(data.frame(out1,variable=rownames(out1)))
  if(print){
    dabiao("1.ANOVA:")
    print(summary(ano))
    dabiao("2.Kruskal.test:")
    print(kruskal.test(var~group))
    dabiao("3.LSDtest, bonferroni p-adj:")
    print(lsdres$groups)
    dabiao("4.tukeyHSD:")
    print(tukeyres)
    dabiao("5.Wilcox-test:")
    print(Q)
  }
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
  lib_ps("fitdistrplus","nortest")
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

#' transfer Geographical latitude and longitude to XY(m)
#'
#' @param dat a two-columns dataframe, first is latitude, second is longitude
#'
#' @export
#'
#' @examples
#'data.frame(row.names = letters[1:18],x=runif(18,30,35),y=runif(18,40,45))->geo
#'toXY(geo)
toXY <- function(geo){
  lib_ps("SoDA")
  XY <- geoXY(geo[,1], geo[,2])
  #geosphere::distm
  return(as.data.frame(row.names = rownames(geo),XY))
}

#=========some plot===========
#' @title Plot a boxplot
#'
#' @param tab your dataframe
#' @param metadata the dataframe contains the group
#' @param group which colname choose for group or a vector
#' @param alpha whether plot a group alphabeta
#' @param method test method:wilcox, tukeyHSD, LSD, (default: wilcox)
#' @param p_value1 multi-test of all group
#' @param p_value2 two-test of each pair
#' @param comparisons comparison pairs, as list, e.g. list(c("a","b"))
#' @param mode 1~3
#' @param facet facet when only one var?
#'
#' @return a 'ggplot' plot object,
#' @export
#'
#' @examples
#' a=data.frame(a=1:18,b=runif(18,0,5))
#' group_box(a,group = rep(c("a","b","c"),each=6),p_value1=F,p_value2=T)
#' multitest(a$a,group = rep(c("a","b","c"),each=6))
#' group_box(a[,1,drop=F],group = rep(c("a","b","c"),each=6),p_value2=T,mode=3,comparisons=list(c("a","b")))
#'
group_box<-function(tab,group=NULL,metadata=NULL,mode=1,facet=T,
                    alpha=F,method="wilcox",p_value1=F,p_value2=F,comparisons=NULL,...){
  lib_ps("ggplot2","dplyr","ggpubr","reshape2")
#data transform
  g_name=NULL
  if(is.vector(tab))tab=data.frame(value=tab)
  else tab=select_if(tab,is.numeric)
  if(is.null(metadata)&&is.null(group)){
    #a single boxplot
    md<-data.frame(tab,group="value",check.names = F)
  }
  else{
    if(is.null(metadata)&&!is.null(group)){
      md<-data.frame(tab,group=group,check.names = F)
    }
    else if ((!is.null(metadata)&&!is.null(group))){
      if(!all(rownames(metadata)%in%rownames(tab)))message("rownames dont match in tab and metadata")
      tab<-tab[rownames(metadata),,drop=F]
      md<-data.frame(tab,group=metadata[,group],check.names = F)
      g_name=group
    }
  }
  md$group<-factor(md$group)

  md%>%reshape2::melt(id.vars="group",variable.name="indexes")->md
  md$indexes=factor(md$indexes,levels = colnames(tab))
#main plot
  if(mode==1){p<-ggplot(md,aes(group,value,color=group,group=group))+
      stat_boxplot(geom = "errorbar",width=0.15)+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(width = 0.15,alpha=0.8,size=0.5)}
  if(mode==2){
    p=ggplot(md,aes(group,value,group=group,fill=group))+
      #stat_boxplot(geom = "errorbar",width=0.15)+
      geom_boxplot(color="black",outlier.shape = NA)+
      geom_jitter(color="black",width = 0.15,alpha=0.8,size=0.5)}
  if(mode==3){
    lib_ps("gghalves")
    p<-ggplot(md,aes(group,value,color=group,group=group))+
      gghalves::geom_half_violin(aes(fill=group), side = "l", trim=FALSE)+
      gghalves::geom_half_point(side="r", size=0.5, alpha=0.8) +
      geom_boxplot(position=position_nudge(x=.22),
                   linewidth = 0.6,
                   width = 0.2,
                   outlier.shape = NA
      )
  }
  p=p+guides(color=guide_legend(g_name),fill=guide_legend(g_name))+
    ylab(label = NULL)+xlab(label = NULL)

#facet?
  flag=(ncol(tab)==1)
  if((!flag)|facet) p=p+facet_wrap(.~indexes,scales = "free_y")
  else {ylab=colnames(tab)[1];p=p+ylab(ylab)}

#p-value?
  if(p_value1) p=p+stat_compare_means(show.legend = FALSE,label.x = 1,label.y.npc = 0.95)
  if(p_value2){
    if(is.null(comparisons)){
      p=p+stat_compare_means(show.legend = FALSE,
                            comparisons = combn(levels(md$group),2)%>%split(col(.)),...)
    }
    else{
      p=p+stat_compare_means(show.legend = FALSE,
                             comparisons = comparisons,...)
    }
  }

  if(alpha){
    a<-list()
    for (i in colnames(tab)){
      filter(md,indexes==!!i)->tmp
      a[[i]]=multitest(tmp$value,tmp$group,return = method)%>%cbind(.,indexes=i)
    }
    do.call(rbind,a)->aa
    md%>%group_by(indexes)%>%summarise(low=min(value),high=max(value))%>%left_join(aa,.,"indexes")->aa
    aa$indexes=factor(aa$indexes,levels = colnames(tab))
    if(mode==3){
      p=p+ geom_text(data = aa,aes(x=variable,y=(high+0.15*(high-low)),label=groups),
                     inherit.aes = FALSE,color='red',size=5,position=position_nudge(x=.1))
    }
    else {p=p+ geom_text(data = aa,aes(x=variable,y=(high+0.05*(high-low)),label=groups),
                         inherit.aes = FALSE,color='red',size=5)
    }
  }

  if(exists("mytheme"))if(inherits(mytheme,c("theme","gg")))p=p+mytheme
  return(p)
}

#' Plot a doughnut chart
#'
#' @param ad a two-columns dataframe, first is type, second is number
#'
#' @import ggplot2 dplyr
#' @return a ggplot
#' @export
#'
#' @examples
#'a=data.frame(type=letters[1:6],num=c(1,3,3,4,5,10))
#'gghuan(a)+scale_fill_manual(values=get_cols(6,"col3"))
#'b=data.frame(type=letters[1:12],num=c(1,3,3,4,15,10,35,2:6))
#'gghuan(b)+theme(legend.position="right")
gghuan<-function(tab,reorder=T,mode="1",topN=5,name=T){
  if(ncol(tab)>2)stop("need two columns: first is type, second is number")
  colnames(tab)[1]->g_name
  colnames(tab)<-c("type","n")
  plot_df<-tab%>%group_by(type)%>%
    summarise(mean=mean(n))

  if(reorder){
    plot_df$type=reorder(plot_df$type,plot_df$mean)
    plot_df<-arrange(plot_df,-mean)
  }

  if(nrow(plot_df)>topN){
    plot_df=rbind(head(plot_df,topN),
                  data.frame(type="others",
                             mean=sum(plot_df$mean[(topN+1):nrow(plot_df)])))

    plot_df$type=relevel(factor(plot_df$type),"others")
  }
  mutate(plot_df,fraction=mean/sum(mean))->plot_df

  plot_df$ymax = cumsum(plot_df$fraction)
  plot_df$ymin = c(0, head(plot_df$ymax, n = -1))
  plot_df$rate_per<-paste(as.character(round(100*plot_df$fraction,1)),'%',sep='')
  if(mode=="1"){plt<-ggplot(data = plot_df, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 3.2, xmin = 1.7)) +
    geom_rect(alpha=0.8) +xlim(c(0, 5)) +
    coord_polar(theta = "y") +
    geom_text(aes(x = 2.5, y = ((ymin+ymax)/2),label = rate_per) ,size=3.6,col="white")
  if(name)plt=plt+geom_text(aes(x = 3.6, y = ((ymin+ymax)/2),label = type) ,size=4)
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

#' Fit a linear model and plot
#'
#' @param tab your dataframe
#' @param var which colname choose for var or a vector
#' @param metadata the dataframe contains the var
#' @param facet facet when only one var?
#' @param ...
#'
#' @import ggpmisc
#' @return a ggplot
#' @export
#'
#' @examples
#'my_lm(runif(50),var=1:50)
#'my_lm(c(1:50)+runif(50,0,5),var=1:50)
my_lm<-function(tab,var,metadata=NULL,facet=T,...){
  lib_ps("ggplot2","dplyr","ggpubr")
  #data transform
  g_name=NULL
  if(is.vector(tab))tab=data.frame(value=tab)

  if(is.null(metadata)){
    md<-data.frame(tab,var=var)
  }
  else if (!is.null(metadata)){
    if(!all(rownames(metadata)%in%rownames(tab)))message("rownames dont match in tab and metadata")
    tab<-tab[rownames(metadata),,drop=F]
    md<-data.frame(tab,var=metadata[,var])
    g_name=var
  }

  if(!all(apply(md, 2, is.numeric)))stop("need numeric")
  md%>%reshape2::melt(.,id.vars="var",variable.name="indexes")->md
  md$indexes=factor(md$indexes,levels = colnames(tab))
  #main plot
  p=ggplot(md,aes(var,value))+
    geom_point(...)+
    geom_smooth(method = "lm",color="red",se = F,formula = "y~x")+
    ggpmisc::stat_poly_eq(
      aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = '~~~~~')),
      formula = y ~ x,  parse = TRUE,color="red",
      size = 3, #公式字体大小
      label.x = 0.05, label.y = 1.05)+#位置 ，0-1之间的比例
    labs(x=NULL,y=NULL)
  #facet?
  flag=(ncol(tab)==1)
  if((!flag)|facet) p=p+facet_wrap(.~indexes,scales = "free_y")
  else {ylab=colnames(tab)[1];p=p+ylab(ylab)}

  p=p+xlab(g_name)

  if(exists("mytheme"))if(inherits(mytheme,c("theme","gg")))p=p+mytheme
  return(p)
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


#' Plot a DNA double helix
#'
#' @export
#' @references \code{https://github.com/SherryDong/create_plot_by_R_base}
dna_plot<-function(){
  lib_ps("RColorBrewer")
  col_DNA <- brewer.pal(8,'Set1')[2]
  # A-green, T-red, C-yellow, G-blue
  col_ATCG <- c(brewer.pal(8,'Accent')[1],brewer.pal(11,'Set3')[4],brewer.pal(11,'Set3')[2],brewer.pal(11,'Paired')[1])
  DNA_length <- 4 ## the code only applies when DNA_length%%2==0, if DNA_length%%2==1, need to modify
  x <- seq(-DNA_length*pi/2,DNA_length*pi/2,length.out=1000) ##
  y1 <- cos(x) ## backbone up
  y2 <- cos(x+pi) ## backbone down
  # get the position of nucleotides
  xx <- seq(DNA_length*pi/2,-DNA_length*pi/2,length.out = DNA_length*5+1);
  xx <- xx+(xx[2]-xx[1])/2
  # remove the first and the lines in the boundary region
  xx <- setdiff(xx,c(xx[c(1:DNA_length)*5-2],min(xx)))
  plot(y1~x,pch=16,type='l',xlab='',ylab='',xaxt='n',yaxt='n',main='',bty='n',col='white')
  for(i in 1:length(xx)){
    ybottom <- cos(xx[i]) # ybottom position
    ytop    <- cos(xx[i]+pi) # yup position
    rr <- sample(1:4,1) ## ATCG, random select one pair
    if(rr==1){
      segments(y0=ybottom,y1=0,x0=xx[i],x1=xx[i],col=col_ATCG[1],lwd=4) ## A-T
      segments(y0=0,y1=ytop,x0=xx[i],x1=xx[i],col=col_ATCG[2],lwd=4)
    }
    if(rr==2){
      segments(y0=ybottom,y1=0,x0=xx[i],x1=xx[i],col=col_ATCG[2],lwd=4) ## T-A
      segments(y0=0,y1=ytop,x0=xx[i],x1=xx[i],col=col_ATCG[1],lwd=4)
    }
    if(rr==3){
      segments(y0=ybottom,y1=0,x0=xx[i],x1=xx[i],col=col_ATCG[3],lwd=4) ## C-G
      segments(y0=0,y1=ytop,x0=xx[i],x1=xx[i],col=col_ATCG[4],lwd=4)
    }
    if(rr==4){
      segments(y0=ybottom,y1=0,x0=xx[i],x1=xx[i],col=col_ATCG[4],lwd=4) ## G-C
      segments(y0=0,y1=ytop,x0=xx[i],x1=xx[i],col=col_ATCG[3],lwd=4)
    }
  }
  lines(y1~x,pch=16,lwd=8,col=col_DNA)
  lines(y2~x,pch=16,lwd=8,col=col_DNA)

}

#' @export
my_cat<-function(mode=1){
  if(mode==1){  lib_ps("RImagePalette","png")
  p1=png::readPNG(system.file("data/smallguodong.ppp",package = "pcutils"))
  graphics::plot(1:2, type = "n", axes = F, ylab = "n", xlab = "n",ann = FALSE)
  graphics::rasterImage(p1, 1, 1, 2, 2)}

  if(mode==2){  lib_ps("ggimage","ggplot2")
  t<-seq(0, 2*pi, 0.08)
  d=data.frame(x=2*(sin(t)-0.5*sin(2*t)),y=2*(cos(t)-0.5*cos(2*t)))
  ggplot(d, aes(x, y)) +
    ggimage::geom_image(image = system.file("data/smallguodong.ppp",package = "pcutils"), size = .05)+theme_void()}
}


#' My Sankey plot
#'
#' @param test a dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[sankeyD3]{sankeyNetwork}}
#' @param mode "sankeyD3","ggsankey"
#'
#' @export
#'
#' @examples
#' data.frame(a=c("a","a","b","b","c"),aa=rep("a",5),b=c("a",LETTERS[2:5]),c=1:5)%>%my_sankey(.,"gg",num=T)
#' data("otutab",package = "MetaNet")
#' cbind(taxonomy,num=rowSums(otutab))[1:10,]->test
#' my_sankey(test)->p
#' plotpdf(p)
my_sankey=function(test,mode=c("sankeyD3","ggsankey"),...){
  mode=match.arg(mode,c("sankeyD3","ggsankey"))
  lib_ps("dplyr")
  nc=ncol(test)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")
  if(mode=="sankeyD3"){
    lib_ps("sankeyD3")
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

    p=sankeyNetwork(Links = as.data.frame(links), Nodes = nodes,
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
    lib_ps("ggsankey")
    df=make_long(test,1:(nc-1),value =!!nc)
    parms=list(...)

    if(!is.null(parms$num)){
      if((parms$num)){
      df%>%group_by(x,node)%>%summarise(value=sum(value))%>%mutate(label=paste0(node,"\n",value))->tmp
      df=left_join(df,tmp[,-3])}
      else df$label=df$node
      }
    else df$label=df$node

    p=ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, label = label,value=value)) +
      geom_sankey(aes(fill = factor(node)),flow.alpha = .6,node.color = "gray30",space = 1) +
      scale_fill_manual(values = get_cols(nlevels(factor(df$node))))+
      geom_sankey_text(size = 3, color = "black",space = 1) +
      theme_sankey(base_size = 18) +
      labs(x = NULL) +
      theme(legend.position = "none",plot.title = element_text(hjust = .5))
    return(p)
  }
}

#' My cicro plot
#'
#' @param df dataframe
#' @param ... \code{\link[circlize]{chordDiagram}}
#
#' @return chordDiagram
#' @export
#'
#' @examples
#' data.frame(a=c("a","a","b","b","c"),b=c("a",LETTERS[2:5]),c=1:5)%>%my_cicro()
my_cicro=function(df,reorder=T,colors=NULL,mode=c("circlize","chorddiag"),...){
  mode=match.arg(mode,c("circlize","chorddiag"))
  colnames(df)=c("from","to","count")

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

  if(is.null(colors))colors=pcutils::get_cols(length(unique(c(colnames(tab),rownames(tab)))))

  if(mode=="circlize"){
    lib_ps("circlize")
    circlize::chordDiagram(tab,grid.col = colors,...)
    del_ps("circlize")}
  if(mode=="chorddiag"){
    lib_ps("chorddiag")
    chorddiag::chorddiag(tab,groupedgeColor= colors,...)
    del_ps("chorddiag")}
}

#' my_synteny
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

  lib_ps("RIdeogram")
  colnames(modu_sum)=c("Chr","Start","End","fill","species","size","color")
  colnames(edge_sum)=c("Species_1","Start_1","End_1","Species_2","Start_2","End_2","fill")
  ideogram(karyotype = modu_sum, synteny =edge_sum)
  rsvg::rsvg_svg("chromosome.svg",file = "chromosome.svg")
  read.file("chromosome.svg")
}

#=======some tips========
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
    lib_ps("foreach","doSNOW")
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
  }
  else {
    res <-lapply(1:reps, loop)
  }}
  #simplify method
  res=do.call(c,res)
',"\n")
}
