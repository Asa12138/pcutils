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
  lib_ps("SoDA", library = F)
  XY <- SoDA::geoXY(geo[, 1], geo[, 2])
  # geosphere::distm
  return(as.data.frame(row.names = rownames(geo), XY))
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
#' data(otutab)
#' cor_plot(metadata[, 3:10])
#' cor_plot(metadata[, 3:10], mode = 2)
#' cor_plot(t(otutab)[,1:50],mode=3,heat=FALSE)
#' }
cor_plot <- function(env, env2 = NULL, mode = 1, method = "pearson", heat = T, mode3_param = NULL, ...) {
  if (ncol(env) > 30 & heat) {
    lib_ps("pheatmap", library = F)
    stats::cor(env) -> a
    pheatmap::pheatmap(a, show_rownames = F, show_colnames = F, border_color = F, ...)
  } else {
    lib_ps("ggcor", library = F)
    if(isNamespaceLoaded("linkET"))lapply(c("ggcor","linkET"),unloadNamespace)

    #ggcor::set_scale(bluered, type = "gradient2n")
    if (is.null(env2)) {
      if (mode == 1) {
        p <- ggcor::quickcor(env, method = method, cor.test = T) +
          ggcor::geom_square(data = ggcor::get_data(type = "lower", show.diag = FALSE)) +
          ggcor::geom_mark(data = ggcor::get_data(type = "upper", show.diag = FALSE), size = 2.5) +
          geom_abline(slope = -1, intercept = ncol(env) + 1)+
          scale_fill_gradientn(colours = bluered,limit=c(-1,1))
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
          ggcor::set_p_yaxis()+
          scale_fill_gradientn(colours = bluered,limit=c(-1,1))
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
        return(p+scale_fill_gradientn(colours = bluered,limit=c(-1,1)))
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
          ggcor::set_p_yaxis()+
          scale_fill_gradientn(colours = bluered,limit=c(-1,1))
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
      graphics::segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[1], lwd = 4) ## A-T
      graphics::segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[2], lwd = 4)
    }
    if (rr == 2) {
      graphics::segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[2], lwd = 4) ## T-A
      graphics::segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[1], lwd = 4)
    }
    if (rr == 3) {
      graphics::segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[3], lwd = 4) ## C-G
      graphics::segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[4], lwd = 4)
    }
    if (rr == 4) {
      graphics::segments(y0 = ybottom, y1 = 0, x0 = xx[i], x1 = xx[i], col = col_ATCG[4], lwd = 4) ## G-C
      graphics::segments(y0 = 0, y1 = ytop, x0 = xx[i], x1 = xx[i], col = col_ATCG[3], lwd = 4)
    }
  }
  graphics::lines(y1 ~ x, pch = 16, lwd = 8, col = col_DNA)
  graphics::lines(y2 ~ x, pch = 16, lwd = 8, col = col_DNA)
}

#' Radar plot
#'
#' @param otu_time otutab
#' @param ... add
#'
#' @export
#' @return ggplot
#' @examples
#' \donttest{
#' data(otutab)
#' tax_radar(otutab[1:20,1:3])
#' }
tax_radar<-function(otu_time,...){
  lib_ps("ggradar","scales",library = F)
  otu_time[1:4,]%>%
    dplyr::mutate_all(scales::rescale) %>%cbind(tax=rownames(.),.)%>%
    ggradar::ggradar(.,legend.text.size=10,...)
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
#' \donttest{
#' data(otutab)
#' triangp(otutab,metadata$Group,class=taxonomy$Phylum,scale=TRUE)
#' }
triangp<-function(otutab,group,scale=F,class=NULL){
  lib_ps("ggtern","vegan",library = F)
  group%>%as.factor()->group
  if (nlevels(group)!=3)stop("group is not 3, can't plot trip")
  KO=OE=WT=NULL

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

#' df 2 link
#'
#' @param test df
#' @param fun function to summary the elements number, defalut: `sum`, you can choose `mean`.
#'
#' @export
#' @examples
#' data(otutab)
#' cbind(taxonomy,num=rowSums(otutab))[1:10,]->test
#' df2link(test)
#'
df2link=function(test,fun=sum){
  if(!is.numeric(test[,ncol(test)]))test$weight=1
  nc=ncol(test)
  colnames(test)[nc]="weight"
  if(nc<3)stop("as least 3-columns dataframe")
  #change duplicated data
  #if need tree, use `before_tree()`

  #merge to two columns
  links=data.frame()
  tmp_fun_df=stats::aggregate(test$weight,by=list(test[,1,drop=T]),fun)
  nodes=data.frame(name=tmp_fun_df[["Group.1"]],level=colnames(test)[1],
                               weight=tmp_fun_df[["x"]])

  for (i in 1:(nc-2)){
    test[,c(i,i+1,nc)]->tmp
    colnames(tmp)=c("from","to","weight")
    tmp=dplyr::group_by(tmp,from,to)%>%dplyr::summarise(weight=fun(weight),.groups="keep")
    tmp=na.omit(tmp)
    links=rbind(links,tmp)

    tmp_fun_df=stats::aggregate(tmp$weight,by=list(tmp$to),fun)
    nodes=rbind(nodes,data.frame(name=tmp_fun_df[["Group.1"]],level=colnames(test)[i+1],
                                 weight=tmp_fun_df[["x"]]))
  }
  return(list(links=links,nodes=nodes))
}

#多余合并为others
gettop=\(a,top){
  nc=ncol(a)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(a[,nc]))stop("the last column must be numeric")
  colnames(a)->cns
  colnames(a)=c(paste0("f",seq_len(nc-1)),"n")
  top=rep(top,length.out=nc-1)
  keep=list()
  for (i in seq_len(nc-1)) {
    tmpc=colnames(a)[i]
    colnames(a)[i]="tmp"
    a%>%dplyr::group_by(tmp)%>%dplyr::summarise(count=sum(n))%>%dplyr::arrange(-count)%>%head(top[i])%>%dplyr::pull(tmp)->keep[[i]]
    a=mutate(a,tmp=ifelse(tmp%in%keep[[i]],tmp,paste0("other_",cns[i])))
    colnames(a)[i]=tmpc
  }
  a=a%>%dplyr::group_by_at(seq_len(nc-1))%>%dplyr::summarise(count=sum(n))%>%dplyr::arrange(-count)
  colnames(a)=cns
  as.data.frame(a)
}

#' My Sankey plot
#'
#' @param test a dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[sankeyD3]{sankeyNetwork}}
#' @param mode "sankeyD3","ggsankey"
#' @param space space width for ggsankey
#' @param topN "all" or numeric vector, determine how many topN shows in each column
#'
#' @export
#'
#' @import ggplot2 dplyr
#' @return ggplot or htmlwidget
#' @examples
#' \donttest{
#' data.frame(a=c("a","a","b","b","c"),aa=rep("a",5),b=c("a",LETTERS[2:5]),c=1:5)%>%
#'    my_sankey(.,"gg",num=TRUE)
#' data(otutab)
#' cbind(taxonomy,num=rowSums(otutab))[1:10,]->test
#' my_sankey(test)
#' }
my_sankey=function(test,mode=c("sankeyD3","ggsankey"),topN="all",space=1,...){
  mode=match.arg(mode,c("sankeyD3","ggsankey"))
  test=as.data.frame(test)
  nc=ncol(test)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")
  if(!identical(topN,"all")){
    test=gettop(test,topN)
  }

  target=weight=x=node=value=next_x=next_node=label=NULL
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

#' My Sunburst plot
#'
#' @param test a dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[plotly]{plot_ly}}
#' @return htmlwidget
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy,num=rowSums(otutab))[1:10,]->test
#' my_sunburst(test)
#' }
my_sunburst=function(test,...){
  test=as.data.frame(test)
  if(length(unique(test[,1]))>1){
    test=cbind("Root"=" ",test)
  }
  nc=ncol(test)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")

  lib_ps("plotly",library = F)
  target=source=weight=NULL
  #change duplicated data

  # for (i in 1:(nc-1)){
  #   test[,i]=paste0(test[,i],strrep(" ",i-1))
  # }

  #merge to two columns
  links=data.frame()
  for (i in 1:(nc-2)){
    test[,c(i,i+1,nc)]->tmp
    colnames(tmp)=c("source","target","weight")
    tmp=dplyr::group_by(tmp,source,target)%>%dplyr::summarise(weight=sum(weight),.groups="keep")
    links=rbind(links,tmp)
  }
  fig <- plotly::plot_ly(
    #定义所有级别各类的标签
    labels = links$target,
    #定义所有级别各类的父级，与上面定义的标签一一对应
    parents = links$source,
    #定义各分类的值（一一对应）
    values = links$weight,
    text = links$weight,
    #指定图表类型：sunburst
    type = 'sunburst',...
  )
  fig
}

#' My Treemap plot
#'
#' @param test a three-columns dataframe with hierarchical structure
#' @param d3 choose 3D
#' @param ... look for parameters in \code{\link[plotly]{plot_ly}}
#'
#' @return htmlwidget
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy,num=rowSums(otutab))[1:10,c(4,7,8)]->test
#' my_treemap(test)
#' }
my_treemap=function(test,d3=TRUE,...){
  test=as.data.frame(test)
  # if(length(unique(test[,1]))>1){
  #   test=cbind("Root"=" ",test)
  # }
  nc=ncol(test)
  if(nc!=3)stop("supports 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")

  lib_ps("treemap",library = F)
  target=source=weight=NULL
  #change duplicated data

  # for (i in 1:(nc-1)){
  #   test[,i]=paste0(test[,i],strrep(" ",i-1))
  # }

  #merge to two columns
  links=data.frame()
  for (i in 1:(nc-2)){
    test[,c(i,i+1,nc)]->tmp
    colnames(tmp)=c("source","target","weight")
    tmp=dplyr::group_by(tmp,source,target)%>%dplyr::summarise(weight=sum(weight),.groups="keep")
    links=rbind(links,tmp)
  }
  fig <- treemap::treemap(
    dtf = links,
    #定义所有级别各类的标签
    index=c("source","target"),
    #定义各分类的值（一一对应）
    vSize = "weight",type="index"
  )
  if(d3){
    lib_ps("d3treeR",library = F)
    fig <- d3treeR::d3tree2( fig ,rootname = colnames(test)[1])
  }
  fig
}

#' My Network plot
#'
#' @param test a dataframe with hierarchical structure
#' @param vertex_anno vertex annotation table
#' @param vertex_group vertex_group
#' @param vertex_class vertex_class
#' @param vertex_size vertex_size
#' @param ... look for parameters in \code{\link[MetaNet]{c_net_plot}}
#'
#' @return metanet
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy,num=rowSums(otutab))[1:10,]->test
#' my_network(test)
#' }
my_network=function(test,vertex_anno=NULL,
                    vertex_group = "v_group", vertex_class = "v_class",
                    vertex_size = "size",...){
  lib_ps("MetaNet",library = F)
  test=as.data.frame(test)
  nc=ncol(test)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")
  ttt=MetaNet::df2net(test)
  ttt=MetaNet::c_net_set(ttt,vertex_anno,vertex_group = vertex_group,
                                                  vertex_class = vertex_class,vertex_size = vertex_size)
  message("For more details for network visualization, please refer to MetaNet ('https://github.com/Asa12138/MetaNet').")
  plot(ttt,...)
}

#' My Circle packing plot
#'
#' @param test a dataframe with hierarchical structure
#' @param anno annotation table for color or fill.
#' @param mode 1~2
#' @param Group fill for mode2
#' @param Score color for mode1
#' @param label the labels column
#' @param show_level_name show which level name? a vector contains some column names.
#' @param show_tip_label show_tip_label, logical
#' @param str_width str_width
#'
#' @return ggplot
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' cbind(taxonomy,weight=rowSums(otutab))[1:10,]->test
#' my_circle_packing(test)
#' }
#'
my_circle_packing=function(test,anno=NULL,mode=1,
                           Group = "v_group",Score = "weight",label="label",
                           show_level_name="all",show_tip_label=TRUE,str_width=10){
  lib_ps("MetaNet","ggraph",library = F)
  test=as.data.frame(test)
  if(length(unique(test[,1]))>1){
    test=cbind("Root"=" ",test)
  }
  nc=ncol(test)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")
  if(any(test[,nc]<0))stop("the weight must be bigger than 0.")
  ttt=MetaNet::df2net(test)

  ttt=MetaNet::c_net_set(ttt,anno)

  tmp_v=MetaNet::get_v(ttt)
  tmp_v$Level=factor(tmp_v$v_class,levels = colnames(test)[-ncol(test)])
  tmp_v$label=ifelse(is.na(tmp_v[,label]),tmp_v$label,tmp_v[,label])
  if(!identical(show_level_name,"all")){
    if(show_tip_label)show_level_name=c(show_level_name,colnames(test)[ncol(test)-1])
    tmp_v$label=ifelse(tmp_v$level%in%show_level_name,tmp_v$label,NA)
  }
  tmp_v$Group=ifelse(tmp_v$Level==colnames(test)[ncol(test)-1],tmp_v[,Group],NA)
  tmp_v$Score=ifelse(tmp_v$Level==colnames(test)[ncol(test)-1],tmp_v[,Score],NA)

  as.list(tmp_v)->igraph::vertex.attributes(ttt)

  if(mode==1){
    p=ggraph::ggraph(ttt, layout = 'circlepack',weight=weight) +
      ggraph::geom_node_circle(aes(fill=Score))+
      scale_fill_continuous(na.value=NA)
  }
  if(mode==2){
    p=ggraph::ggraph(ttt, layout = 'circlepack',weight=weight) +
      ggraph::geom_node_circle(aes(fill=Group))+
      scale_fill_discrete(na.translate = F)
  }
  p=p+ggraph::geom_node_circle(aes(color=Level))+
      ggraph::geom_node_text(aes(label=stringr::str_wrap(label,width = str_width),color=Level,
                                 size = weight),show.legend = F)+
      # ggraph::geom_node_text(aes(label=stringr::str_wrap(label,width = str_width),color=Level,
      #                            filter=leaf,size = weight),show.legend = F)+
      # ggraph::geom_node_text(aes(label=stringr::str_wrap(label,width = str_width),color=Level,
      #                            filter=!leaf,size = weight),show.legend = F,nudge_y = 0.5)+
      theme_void()
  p
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
#' data(otutab)
#' cbind(taxonomy,num=rowSums(otutab))[1:10,c(3,7,8)]->test
#' my_circo(test)
#' }
my_circo=function(df,reorder=T,pal=NULL,mode=c("circlize","chorddiag")[1],...){
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
#' @return plot
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

#' Plot a area plot
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
#' @param number show the number?
#' @param format_params parameters parse to \code{\link[base]{format}}
#' @param text_params parameters parse to \code{\link[ggplot2]{geom_text}}
#'
#' @import ggplot2
#' @export
#' @return a ggplot
#' @examples
#' data(otutab)
#' areaplot(otutab, metadata, group = "Id")
#' \donttest{
#' areaplot(otutab, metadata, group = "Group", group_order = TRUE, relative = FALSE)
#' }
areaplot <- function(otutab, metadata = NULL, group = "Group", get_data = FALSE,
                      bar_params = list(position = "stack"),
                      topN = 8, others = TRUE, relative = TRUE, legend_title = "",
                      stack_order = TRUE, group_order = FALSE, facet_order = FALSE,
                      style = c("group", "sample")[1],
                      number = FALSE, format_params = list(digits = 2), text_params = list(position = position_stack())) {
  # Used to draw species stacking diagrams, suitable for processing various OTU similar data, input metatab as the basis for grouping.
  # style can choose "group" or "sample"
  # others=TRUE is used to choose whether to draw other than TopN
  # pmode can choose fill/stack/dodge
  # library(ggplot2)
  # library(dplyr)
  lib_ps("reshape2", "scales", "dplyr", library = FALSE)
  variable=Taxonomy=value=NULL
  if(is.numeric(metadata[, group,drop=T]))warning("Recommend categorical variables")
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
  bar_params <- update_param(NULL, bar_params)
  format_params <- update_param(list(digits = 2), format_params)
  text_params <- update_param(list(position = position_stack()), text_params)

  #变为数字向量
  data_all$variable2=as.numeric(data_all$variable)

  if (style == "sample") {
    if (T) {
      p <- ggplot(data_all, aes(
        x = variable2, y = value, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        # geom_bar(stat = "identity",  position = pmode) +
        do.call(geom_area, bar_params) +
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
    if (T) {
      p <- ggplot(data_all, aes(
        x = variable2, y = value, fill = Taxonomy,
        label = do.call(format, append(list(value), format_params))
      )) +
        do.call(geom_area, bar_params)
    }
  }
  #强行加上原来的label
  p=p+scale_x_continuous(breaks = 1:nlevels(data_all$variable),labels = levels(data_all$variable))

  if (relative) {
    p <- p + scale_y_continuous(labels = scales::percent) + ylab("Relative Abundance (%)")
  } else {
    p <- p + ylab("Number")
  }

  if (number) p <- p + do.call(geom_text, (text_params))

  p + guides(fill = guide_legend(title = legend_title)) + xlab(group)
}

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
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' data(otutab)
#' ggheatmap(otutab[1:30,],scale="row",row_annotation = otutab[1:30,1:2],
#'    col_annotation = metadata[,c(2,4)])
ggheatmap=function(otutab,pal=NULL,scale="none",
                   rowname=T,colname=T,
                   row_cluster=T,col_cluster=T,
                   row_annotation=NULL,col_annotation=NULL,annotation_pal=NULL){
  lib_ps("ggnewscale","aplot","reshape2","ggtree","ape",library = F)

  if(is.null(pal))pal=bluered
  else if(length(is.ggplot.color(pal))<2)stop("pal is wrong!")

  otutab->d
  if(scale=="row")d=trans(d,method = "standardize",margin = 1)
  else if (scale=="column")d=trans(d,method = "standardize",margin = 2)

  rownames(d)->d$otu

  dd=reshape2::melt(d,id.vars="otu",variable.name="sample")

  p <- ggplot(dd, aes(sample,otu, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal) +
    scale_y_discrete(position="right") +
    theme_minimal() +
    xlab(NULL) + ylab(NULL)

  if(!rowname){
    p=p+theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
  }
  if(!colname){
    p=p+theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
  }

  if(!is.null(row_annotation)){
    ca1 <- row_annotation
    rownames(ca1)->ca1$Id
    pc1=ggplot()
    for (i in 1:(ncol(ca1)-1)){
      tmp=ca1[,c(i,ncol(ca1))]
      pd1=reshape2::melt(tmp,id.vars="Id",variable.name="sample")
      if(i>1)pc1=pc1+ggnewscale::new_scale_fill()
      pc1=pc1+
        geom_tile(data = pd1, aes(y=Id,x=sample, fill=value))+
        labs(fill = colnames(ca1)[i])
      if(!is.null(annotation_pal[[colnames(ca1)[i]]])){
        if(is.numeric(pd1$value))pc1=pc1+scale_fill_gradientn(colours = annotation_pal[[colnames(ca1)[i]]])
        else pc1=pc1+scale_fill_manual(values = annotation_pal[[colnames(ca1)[i]]])
      }
    }
    pc1=pc1+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      xlab(NULL) + ylab(NULL)
    p=p %>% aplot::insert_left(pc1, width=0.05*(ncol(ca1)-1))
  }

  if(row_cluster){
    hclust(dist(otutab))%>%ape::as.phylo()->a
    p=p %>% aplot::insert_left(ggtree::ggtree(a,branch.length = "none"), width=.1)
  }

  if(!is.null(col_annotation)){
    ca <- col_annotation
    rownames(ca)->ca$Id

    pc=ggplot()
    for (i in 1:(ncol(ca)-1)){
      tmp=ca[,c(i,ncol(ca))]
      pd=reshape2::melt(tmp,id.vars="Id",variable.name="sample")

      if(i>1)pc=pc+ggnewscale::new_scale_fill()
      pc=pc+
        geom_tile(data = pd, aes(x=Id,y=sample, fill=value))+
        labs(fill = colnames(ca)[i])+
        scale_y_discrete(position="right")
      if(!is.null(annotation_pal[[colnames(ca)[i]]])){
        if(is.numeric(pd$value))pc=pc+scale_fill_gradientn(colours = annotation_pal[[colnames(ca)[i]]])
        else pc=pc+scale_fill_manual(values = annotation_pal[[colnames(ca)[i]]])
      }
    }
    pc=pc+
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      xlab(NULL) + ylab(NULL)

    p=p %>% aplot::insert_top(pc, height=0.05*(ncol(ca)-1))
  }
  if(col_cluster){
    hclust(dist(t(otutab)))%>%ape::as.phylo()->b
    p=p %>% aplot::insert_top(ggtree::ggtree(b,branch.length = "none") +
                                ggtree::layout_dendrogram(), height=.1)
  }
  return(p)
}
