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
    ggcor::set_scale(c("#6D9EC1", "white", "#E46726"), type = "gradient2n")
    if (is.null(env2)) {
      if (mode == 1) {
        p <- ggcor::quickcor(env, method = method, cor.test = T) +
          ggcor::geom_square(data = ggcor::get_data(type = "lower", show.diag = FALSE)) +
          ggcor::geom_mark(data = ggcor::get_data(type = "upper", show.diag = FALSE), size = 2.5) +
          geom_abline(slope = -1, intercept = ncol(env) + 1)
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
          ggcor::set_p_yaxis()
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
        return(p)
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
          ggcor::set_p_yaxis()
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

#' My Sankey plot
#'
#' @param test a dataframe with hierarchical structure
#' @param ... look for parameters in \code{\link[sankeyD3]{sankeyNetwork}}
#' @param mode "sankeyD3","ggsankey"
#' @param space space width for ggsankey
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
#' my_sankey(test)->p
#' }
my_sankey=function(test,mode=c("sankeyD3","ggsankey"),space=1,...){
  mode=match.arg(mode,c("sankeyD3","ggsankey"))
  test=as.data.frame(test)
  nc=ncol(test)
  if(nc<3)stop("as least 3-columns dataframe")
  if(!is.numeric(test[,nc]))stop("the last column must be numeric")

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
#' data.frame(a=c("a","a","b","b","c"),b=c("a",LETTERS[2:5]),c=1:5)%>%my_circo(mode="chorddiag")
#' }
my_circo=function(df,reorder=T,pal=NULL,mode=c("circlize","chorddiag"),...){
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
