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

#========Common plots=======

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
          scale_fill_gradientn(colours = get_cols(pal = "bluered"),limit=c(-1,1))
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
          scale_fill_gradientn(colours = get_cols(pal = "bluered"),limit=c(-1,1))
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
        return(p+scale_fill_gradientn(colours = get_cols(pal = "bluered"),limit=c(-1,1)))
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
          scale_fill_gradientn(colours = get_cols(pal = "bluered"),limit=c(-1,1))
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
#' tax_radar(otutab[1:6,1:4])
#' }
tax_radar<-function(group_df,...){
  lib_ps("ggradar","scales",library = F)
  if(nrow(group_df)>20|ncol(group_df)>6)stop("too many columns or rows!")
  else{
    group_df%>%
      dplyr::mutate_all(scales::rescale) %>%cbind(tax=rownames(.),.)%>%
      ggradar::ggradar(.,legend.text.size=10,...)
  }
}


#' Triangle plot
#'
#' @param group_df group_df
#' @param class point color
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(otutab)
#' hebing(otutab,metadata$Group,act = 'mean')->tmp
#' triangp(tmp,class=taxonomy$Phylum)
#' }
triangp<-function(group_df,class=NULL){
  lib_ps("ggtern",library = F)
  if (ncol(group_df)!=3)stop("ncol of group_df is not 3, can't plot trip")
  KO=OE=WT=NULL

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

#多余合并为others
gettop=\(a,top){
  nc=ncol(a)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(a[,nc]))stop("the last column must be numeric")
  colnames(a)->cns
  colnames(a)=c(paste0("f",seq_len(nc-1)),"n")
  a=mutate_at(a,seq_len(nc-1),as.character)
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
#' @param width width
#' @param str_width str_width
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
my_sankey=function(test,mode=c("sankeyD3","ggsankey"),topN="all",space=1,width=0.1,str_width=20,...){
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

    p=ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                     label = stringr::str_wrap(label, width = str_width),fill = factor(node),value=value)) +
      ggsankey::geom_sankey(flow.alpha = .6,node.color = "gray30",space = space,width=width) +
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
#' data.frame(a=c("a","a","b","b","c"),b=c("a",LETTERS[2:5]),c=1:5)%>%my_circo(mode="circlize")
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

  if(is.null(pal))pal=get_cols(pal = "bluered")
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
