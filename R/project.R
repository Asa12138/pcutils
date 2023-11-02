
#' Make a R-analysis project
#'
#' @param pro_n project name
#' @param root_dir root directory
#' @param bib cite papers bib, from Zotero
#' @param csl cite papers format, default science.csl
#'
#' @export
#' @return No return value
make_project <- function(pro_n, root_dir = "~/Documents/R/",
                         bib = "~/Documents/R/pc_blog/content/bib/My Library.bib",
                         csl = "~/Documents/R/pc_blog/content/bib/science.csl") {
  # pro_n='test'
  if (substr(root_dir, nchar(root_dir), nchar(root_dir)) == "/") {
    pro_dir <- paste0(root_dir, pro_n)
  } else {
    pro_dir <- paste0(root_dir, "/", pro_n)
  }

  if (dir.exists(pro_dir)) stop("directory exist, try other name")
  dir.create(pro_dir)
  lapply(c("data", "temp", "result", "summary", "analysis"), FUN = \(x)dir.create(paste(pro_dir, x, sep = "/")))
  lapply(c("R_config.R", paste0(pro_n, ".Rproj")), FUN = \(x)file.create(paste(pro_dir, "analysis", x, sep = "/")))
  if (file.exists(bib)) {
    file.copy(bib, paste(pro_dir, "analysis", "My_Library.bib", sep = "/"))
  }
  if (file.exists(csl)) {
    file.copy(csl, paste(pro_dir, "analysis", "my.csl", sep = "/"))
  }

  cat("Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: XeLaTeX", file = paste(pro_dir, "analysis", paste0(pro_n, ".Rproj"), sep = "/"))

  cat('devtools::load_all("~/Documents/R/pctax/")
devtools::load_all("~/Documents/R/pcutils/")
Packages <- c("dplyr", "reshape2","ggsci","ggpubr","RColorBrewer","cowplot","readr","tibble","vegan","ggrepel")
lib_ps(Packages)
kin_col=c(k__Bacteria="#a6bce3",k__Fungi="#fdbf6f",k__Metazoa="#fb9a99",k__Viridiplantae="#a9d483",
          k__Archaea="#1f78b4",k__Eukaryota_others="#8dd3c7",k__Viruses="#bda7c9")

add_theme()', file = paste(pro_dir, "analysis", "R_config.R", sep = "/"))

  message(paste0("Set `", pro_n, "` sucessfully! Open project at directory: `", pro_dir, "`"))
}

#' Add an analysis for a project
#'
#' @param analysis_n analysis name
#' @param title file title
#' @param author author
#' @param theme 1~10
#'
#' @export
#' @return No return value
add_analysis <- function(analysis_n, title = analysis_n, author = "Peng Chen", theme = 1) {
  pro_n <- list.files(pattern = "*.Rproj")
  pro_n <- gsub(".Rproj", "", pro_n)
  if (length(pro_n) != 1) stop("make sure there is a *.Rproj file in your `getwd()`")
  message(paste0("Add ", analysis_n, " for project ", pro_n))

  # rmdls=list.files(pattern = "*.Rmd")%>%gsub(".Rmd","",.)
  if (file.exists(paste0(analysis_n, ".Rmd"))) stop(paste0(analysis_n, ".Rmd already exists!"))
  file.create(paste0(analysis_n, ".Rmd"))
  if (dir.exists(analysis_n)) stop(paste0(analysis_n, " directory already exists!"))
  dir.create(analysis_n)

  my_bib <- "\nbibliography: My_Library.bib"
  if (!file.exists("My_Library.bib")) my_bib <- ""

  my_csl <- "\ncsl: ./my.csl"
  if (!file.exists("my.csl")) my_csl <- "\nbiblio-style: apa"

  if (theme != 1) {
    lib_ps("rmdformats", "prettydoc", "tufte", library = FALSE)
  }
  output_theme <- c("
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true", "
output:
  rmdformats::html_clean:
    toc: true
    toc_depth: 3", "
output:
  rmdformats::readthedown:
    toc_depth: 3", "
output:
  rmdformats::robobook:
    toc_depth: 3", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: cayman", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: tactile", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: architect", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: leonids", "
output:
  prettydoc::html_pretty:
    toc: true
    toc_depth: 3
    theme: hpstr", "
output:
  tufte::tufte_html:
    toc: true
    toc_depth: 3")[theme]

  cat('---
title: "', title, '"
author:
  - ', author, "
date: ", date(), my_bib, "
link-citations: yes", my_csl, output_theme, '
editor_options:
  markdown:
    wrap: 150
---

```{r setup, include = FALSE}
path <- "', getwd(), '"
knitr::opts_chunk$set(eval=TRUE, #run code in chunks (default = TRUE)
                       highlight = TRUE, #highlight display
                       echo = TRUE, #whether to include the source code in the output
                       tidy=TRUE, #whether to organize the code
                       error = TRUE, #Whether to include error information in the output
                       warning = FALSE, #Whether to include warnings in the output (default = TRUE)
                       message = FALSE, #whether to include reference information in the output
                       cache=TRUE, #whether to cache
                       collapse = FALSE # output in one piece
                       )
knitr::opts_knit$set(root.dir = path)
```
Install and import all dependent packages, data import:
```{r import,echo=TRUE, message=FALSE, warning=FALSE, results="hide"}
source("./R_config.R")
output="', analysis_n, '/"
```
# Header1
```{r test,echo=TRUE,fig.cap="Test fig"}
a=data.frame(type=letters[1:6],num=c(1,3,3,4,5,10))
gghuan(a)+scale_fill_manual(values=get_cols(6,"col3"))
ggsave("', paste0(analysis_n, "/test.pdf"), '")
```

', file = paste0(analysis_n, ".Rmd"), sep = "")
}

#=======some tips========

#' How to use parallel
#'
#' @param loop the main function
#'
#' @export
#' @return No return value
how_to_use_parallel=function(loop=function(i){return(mean(rnorm(100)))}){
  lib_ps("clipr",library = F)
  res_text=paste0('#parallel
  reps=100;threads=1
  #main function
  loop=',paste(deparse(loop),collapse="\n"),'
  {
  if(threads>1){
    pcutils::lib_ps("foreach","doSNOW","snow")
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
    pcutils::del_ps("doSNOW","snow","foreach")
  }
  else {
    res <-lapply(1:reps, loop)
  }}
  #simplify method
  res=do.call(c,res)
')
  clipr::write_clip(res_text)
  message(res_text)
}

#' How to update parameters
#' @export
#' @return No return value
how_to_update_parameters=function(){
  lib_ps("clipr",library = F)
  res_text=paste0('point_params = list(size=5,color="red")
ggplot(data.frame(x=1:5,y=5:1), aes(x = x, y = y))+
  do.call(geom_point, update_param(list(size=2,color="blue",alpha=0.5), point_params))')
  clipr::write_clip(res_text)
  message(res_text)
}

#' How to use sbatch
#'
#' @param mode 1~3
#'
#' @export
#' @return No return value
how_to_use_sbatch=function(mode=1){
  if(mode==1){
    lib_ps("clipr",library = F)
    res_text=paste0("#!/bin/bash
#SBATCH --job-name=fastp
#SBATCH --output=/share/home/jianglab/pengchen/work/%x_%A_%a.out
#SBATCH --error=/share/home/jianglab/pengchen/work/%x_%A_%a.err
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2g
#SBATCH --time=14-00:00:00
echo start: `date +'%Y-%m-%d %T'`
start=`date +%s`
####################

do something

####################
echo end: `date +'%Y-%m-%d %T'`
end=`date +%s`
echo TIME:`expr $end - $start`s")
  }
  if(mode==2){
    lib_ps("clipr",library = F)
    res_text=paste0("#!/bin/bash
#SBATCH --job-name=fastp
#SBATCH --output=/share/home/jianglab/pengchen/work/%x_%A_%a.out
#SBATCH --error=/share/home/jianglab/pengchen/work/%x_%A_%a.err
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2g
#SBATCH --time=14-00:00:00
#SBATCH --array=1-39
echo start: `date +'%Y-%m-%d %T'`
start=`date +%s`
echo \"SLURM_ARRAY_TASK_ID: \" $SLURM_ARRAY_TASK_ID
samplelist=~/work/st/samplelist
sample=$(head -n $SLURM_ARRAY_TASK_ID $samplelist | tail -1)
echo handling: $sample
####################

fastp -w 2 -i ~/work/st/data/data/${sample}'_1.clean.fq.gz' -o ~/work/st/data/fastp/${sample}_1.fq \
-I ~/work/st/data/data/${sample}'_2.clean.fq.gz' -O ~/work/st/data/fastp/${sample}_2.fq -j ~/work/st/data/fastp/${sample}.json

####################
echo end: `date +'%Y-%m-%d %T'`
end=`date +%s`
echo TIME:`expr $end - $start`s")
  }
  if(mode==3){
    lib_ps("clipr",library = F)
    res_text=paste0("#!/bin/bash
#SBATCH --job-name=fastp
#SBATCH --output=/share/home/jianglab/pengchen/work/%x_%A_%a.out
#SBATCH --error=/share/home/jianglab/pengchen/work/%x_%A_%a.err
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2g
#SBATCH --time=14-00:00:00
#SBATCH --array=1-39

echo start: `date +'%Y-%m-%d %T'`
start=`date +%s`

echo \"SLURM_ARRAY_TASK_ID: \" $SLURM_ARRAY_TASK_ID
START=$SLURM_ARRAY_TASK_ID
NUMLINES=4 #how many sample in one array
STOP=$((SLURM_ARRAY_TASK_ID*NUMLINES))
START=\"$(($STOP - $(($NUMLINES - 1))))\"

#set the min and max
if [ $START -lt 1 ]
then
  START=1
fi
if [ $STOP -gt 39 ]
then
  STOP=39
fi

echo \"START=$START\"
echo \"STOP=$STOP\"

samplelist=~/work/st/samplelist
#############
for (( N = $START; N <= $STOP; N++ ))
do
  sample=$(head -n \"$N\" $samplelist | tail -n 1)
	echo $sample

  bowtie2 -p 8 -x ~/db/humangenome/hg38 -1 ~/work/st/data/fastp/${sample}_1.fq -2 ~/work/st/data/fastp/${sample}_2.fq \
  -S ~/work/st/data/rm_human/${sample}.sam --un-conc ~/work/st/data/rm_human/${sample}.fq --very-sensitive

done
##############
echo end: `date +'%Y-%m-%d %T'`
end=`date +%s`
echo TIME:`expr $end - $start`s")
  }
  clipr::write_clip(res_text)
  message(res_text)
}

#' How to set options in a package
#'
#' @param package package name
#'
#' @return No value
#' @export
how_to_set_options=function(package="My_package"){
  lib_ps("clipr",library = F)
  res_text=paste0('# Load the value of the option on package startup
.onAttach <- function(libname, pkgname) {
  if(!dir.exists(tools::R_user_dir("',package,'")))dir.create(tools::R_user_dir("',package,'"),recursive = TRUE)
  refresh_config()
}

# read user config first.
refresh_config <- function() {
  default_options <- readRDS(file = system.file("config", package = "',package,'"))
  file_path=file.path(tools::R_user_dir("',package,'"),"config")
  if (file.exists(file_path)) {
    options_to_load <- readRDS(file = file_path)
    if(length(options_to_load)==0)options_to_load=NULL
    options_to_load=pcutils::update_param(default_options,options_to_load)
  }
  else {
    options_to_load=default_options
  }
  # set options
  options("',package,'_config"=options_to_load)
}

#\' Show config
#\'
#\' @return config
#\' @export
show_config=function(){
  config=getOption("',package,'_config")
  return(config)
}

#\' Set config
#\'
#\' @param item item
#\' @param value value
#\'
#\' @return No value
#\' @export
#\'
set_config <- function(item,value) {
  refresh_config()
  config=getOption("',package,'_config")
  if(is.null(value))config=config[-which(names(config)==item)]
  else config=pcutils::update_param(config,setNames(list(value),item))
  saveRDS(config, file = file.path(tools::R_user_dir("',package,'"),"config"))
  options("',package,'_config"=config)
  message("Set sucessfully!")
}
')
  clipr::write_clip(res_text)
  message(res_text)
}

micro_works={list(
  "fastp"=paste0('
  mkdir -p data/fastp
  ~/miniconda3/envs/waste/bin/fastp -w 8 -i data/rawdata/${sample}_f1.fastq.gz -o data/fastp/${sample}_1.fq.gz \\
    -I data/rawdata/${sample}_r2.fastq.gz -O data/fastp/${sample}_2.fq.gz -j data/fastp/${i}.json
'),
  "rm_human"=paste0('
  mkdir -p data/rm_human
  ~/miniconda3/envs/waste/bin/bowtie2 -p 8 -x ~/db/humangenome/hg38 -1 data/fastp/${sample}_1.fq.gz \\
    -2 data/fastp/${sample}_2.fq.gz -S data/rm_human/${sample}.sam \\
    --un-conc data/rm_human/${sample}.fq --very-sensitive
  rm data/rm_human/${sample}.sam
'),
  "kraken"=paste0('
  mkdir -p result/kraken
  python /share/home/jianglab/shared/krakenDB/K2ols/kraken2M.py -t 16 \\
      -i data/rm_human \\
      -c 0.05 \\
      -s .1.fq,.2.fq \\
      -o result/kraken \\
      -d /share/home/jianglab/shared/krakenDB/mydb2 \\
      -k ~/miniconda3/envs/waste/bin/kraken2 \\
      -kt /share/home/jianglab/shared/krakenDB/K2ols/KrakenTools
  mkdir -p result/kraken/kreport
  mv result/kraken/*_kreport.txt result/kraken/kreport/
  python ~/db/script/format_kreports.py -i result/kraken/kreport \\
      -kt /share/home/jianglab/shared/krakenDB/K2ols/KrakenTools --save-name2taxid
'),
  "kraken2"=paste0('
  mkdir -p result/kraken2
  ~/miniconda3/envs/waste/bin/kraken2 --threads 8 \\
    --db ~/db/kraken2/stand_krakenDB \\
    --confidence 0.05 \\
    --output result/kraken2/${sample}.output \\
    --report result/kraken2/${sample}.kreport \\
    --paired \\
    data/rm_human/${sample}.1.fq \\
    data/rm_human/${sample}.2.fq
'),
  "megahit"=paste0('
  mkdir -p result/megahit
  mkdir -p result/contigs
  ~/miniconda3/envs/waste/bin/megahit -t 8 -1 data/rm_human/${sample}.1.fq \\
    -2 data/rm_human/${sample}.2.fq \\
    -o result/megahit/${sample} --out-prefix ${sample}
  mv result/megahit/${sample}/${sample}.contigs.fa result/megahit/contigs/
'),
  "prodigal"=paste0("
  mkdir -p result/prodigal
  ~/miniconda3/envs/waste/bin/prodigal -i result/megahit/contigs/${sample}.contigs.fa \\
      -d result/prodigal/${sample}.gene.fa \\
      -o result/prodigal/${sample}.gene.gff \\
      -p meta -f gff

  mkdir -p result/prodigal/fullgene
  grep 'partial=00' result/prodigal/${sample}.gene.fa | cut -f1 -d ' '| sed 's/>//' > result/prodigal/${sample}.fullid
  seqkit grep -f result/prodigal/${sample}.fullid result/prodigal/${sample}.gene.fa > result/prodigal/fullgene/${sample}.gene.fa
"),
  "cluster"=paste0('
  #修改每条序列的名称，加上样本名
  for sample in `cat $samplelist`
  do
    echo $sample
    sed -i "/>/s/>/>${sample}_/" result/prodigal/fullgene/${sample}.gene.fa
  done
  echo "start merge"
  cat result/prodigal/fullgene/*.gene.fa > result/prodigal/fullgene/all.fullgene.fa
  echo "done"

  mkdir -p result/NR
  mmseqs easy-linclust result/prodigal/fullgene/all.fullgene.fa result/NR/nucleotide mmseqs_tmp \\
    --min-seq-id 0.9 -c 0.9 --cov-mode 1  --threads 8
  # ~/miniconda3/envs/waste/bin/cd-hit-est -i result/prodigal/fullgene/all.fullgene.fa \\
  #   -o result/NR/nucleotide.fa \\
  #   -aS 0.9 -c 0.9 -G 0 -g 0 -T 0 -M 0
  mv result/NR/nucleotide_rep_seq.fasta result/NR/nucleotide.fa
  ~/miniconda3/envs/waste/bin/seqkit translate result/NR/nucleotide.fa > result/NR/protein.fa
  sed \'s/\\*//g\' result/NR/protein.fa > result/NR/protein_no_end.fa
'),
  "salmon-index"=paste0('
  ## 建索引, -t序列, -i 索引，10s
  # 大点内存
  mkdir -p result/salmon
  ~/miniconda3/envs/waste/share/salmon/bin/salmon index \\
    -t result/NR/nucleotide.fa \\
    -p 4 \\
    -i result/salmon/index
'),
  "salmon-quant"=paste0('
  # 大点内存
  mkdir -p result/salmon/quant
  ~/miniconda3/envs/waste/share/salmon/bin/salmon quant \\
      -i result/salmon/index -l A -p 4 --meta \\
      -1 data/rm_human/${sample}.1.fq \\
      -2 data/rm_human/${sample}.2.fq \\
      -o result/salmon/quant/${sample}.quant
'),
  "salmon-merge"=paste0("
  ls result/salmon/quant/*.quant/*.sf |awk -F'/' '{print $(NF-1)}' |sed 's/.quant//' >tmp_finished
  diff_rows -f samplelist -s tmp_finished >tmp_diff
  # 计算结果的行数
  line_count=$( cat tmp_diff| wc -l)

  # 检查行数是否大于1，如果是则终止脚本
  if [ \"$line_count\" -gt 1 ]; then
    echo 'sf文件和samplelist数量不一致。'
    exit 1
  fi

  ## 合并
  ~/miniconda3/envs/waste/share/salmon/bin/salmon quantmerge \\
      --quants result/salmon/quant/*.quant \\
      -o result/salmon/gene.TPM
  ~/miniconda3/envs/waste/share/salmon/bin/salmon quantmerge \\
      --quants result/salmon/quant/*.quant \\
      --column NumReads -o result/salmon/gene.count
  sed -i '1 s/.quant//g' result/salmon/gene.*
"),
  "eggnog"=paste0("
  conda activate func
  ## diamond比对基因至eggNOG 5.0数据库, 1~9h，默认diamond 1e-3
  mkdir -p result/eggnog
  emapper.py --no_annot --no_file_comments --override \\
    --data_dir ~/db/eggnog5 \\
    -i result/NR/protein.fa \\
    --cpu 16 -m diamond \\
    -o result/eggnog/protein
  ## 比对结果功能注释, 1h
  emapper.py \\
    --annotate_hits_table result/eggnog/protein.emapper.seed_orthologs \\
    --data_dir ~/db/eggnog5 \\
    --cpu 16 --no_file_comments --override \\
    -o result/eggnog/output

  ## 添表头, 1列为ID，9列KO，16列CAZy，21列COG，22列描述
  sed '1 i Name\\tortholog\\tevalue\\tscore\\ttaxonomic\\tprotein\\tGO\\tEC\\tKO\\tPathway\\tModule\\tReaction\\trclass\\tBRITE\\tTC\\tCAZy\\tBiGG\\ttax_scope\\tOG\\tbestOG\\tCOG\\tdescription' \\
    result/eggnog/output.emapper.annotations \\
    > result/eggnog/eggnog_anno_output
"),
  "cazy"=paste0("
  mkdir -p result/dbcan2
  diamond blastp   \\
  	--db ~/db/dbcan2/CAZyDB.07312020  \\
  	--query result/NR/protein_no_end.fa \\
  	--threads 8 -e 1e-5 --outfmt 6 \\
  	--max-target-seqs 1 --quiet \\
  	--out result/dbcan2/gene_diamond.f6
"),
  "rgi"=paste0("
  #conda activate rgi
  mkdir -p result/card
  ~/miniconda3/envs/rgi/bin/rgi main \\
    --input_sequence result/NR/protein_no_end.fa \\
    --output_file result/card/protein \\
    --input_type protein --num_threads 8 \\
    --clean --alignment_tool DIAMOND
"),
  "vfdb"=paste0("
  mkdir -p result/vfdb
  diamond blastp   \\
  	--db ~/db/VFDB/VFDB_setB_pro \\
  	--query result/NR/protein_no_end.fa \\
  	--threads 8 -e 1e-5 --outfmt 6 \\
  	--max-target-seqs 1 --quiet \\
  	--out result/vfdb/gene_diamond.f6
"),
  "summary"=paste0("
  mkdir -p result/summ_table
  if [ -f result/eggnog/eggnog_anno_output ]; then
  # 汇总，9列KO，16列CAZy按逗号分隔，21列COG按字母分隔，原始值累加
  ~/db/script/summarizeAbundance.py \\
    -i result/salmon/gene.count \\
    -m result/eggnog/eggnog_anno_output \\
    -c '9,16,21' -s ',+,+*' -n raw \\
    -o result/summ_table/eggnog
  sed -i 's/^ko://' summ_table/eggnog.KO.raw.txt
  fi

  if [ -f result/card/protein.txt ]; then
  awk 'BEGIN {FS = \"\\t\"; OFS = \"\\t\"} {split($1, a, \" \"); $1 = a[1]} 1' result/card/protein.txt >result/card/protein_format_id.txt
  ~/db/script/summarizeAbundance.py \\
    -i result/salmon/gene.count \\
    -m result/card/protein_format_id.txt \\
    -c '11' -s ';' -n raw \\
    -o result/summ_table/card
  fi

  if [ -f result/dbcan2/gene_diamond.f6 ]; then
  # 提取基因与dbcan分类对应表
  perl ~/db/script/format_dbcan2list.pl \
    -i result/dbcan2/gene_diamond.f6 \
    -o result/dbcan2/gene.list
  # 按对应表累计丰度
  ~/db/script/summarizeAbundance.py \\
    -i result/salmon/gene.count \\
    -m result/dbcan2/gene.list \\
    -c '2' -s ',' -n raw \\
    -o result/dbcan2/cazy
  fi

  if [ -f result/vfdb/gene_diamond.f6 ]; then
  sed -i '1 i Name\tvf\tpident\tlength\tmismatch\tgapopen\tqstart\tqend\tsstart\tsend\tevalue\tbitscore' \\
    result/vfdb/gene_diamond.f6
  ~/db/script/summarizeAbundance.py \\
    -i result/salmon/gene.count \\
    -m result/vfdb/gene_diamond.f6 \\
    -c '2' -s ';' -n raw \\
    -o result/summ_table/card
  fi
")
)}

#' Microbiome sbatch
#'
#' @param work_dir work_dir
#' @param step "fastp","rm_human","megahit","prodigal","salmon-quant",...
#' @param all_sample_num all sample number
#' @param array array number
#' @param partition partition
#' @param cpus_per_task cpus_per_task
#' @param mem mem
#'
#' @export
micro_sbatch=function(work_dir="/share/home/jianglab/pengchen/work/asthma/",
                      step="fastp",all_sample_num=40,array=1,
                      partition="cpu",cpus_per_task=1,mem_per_cpu="2G"){
  num_each_array=ceiling(all_sample_num/array)
  if(!endsWith(work_dir,"/"))work_dir=paste0(work_dir,"/")
  array=ifelse(array>1,paste0("1-",array),"1")

  header={paste0("#!/bin/bash
#SBATCH --job-name=",step,"
#SBATCH --output=",work_dir,"log/%x_%A_%a.out
#SBATCH --error=",work_dir,"log/%x_%A_%a.err
#SBATCH --array=",array,"
#SBATCH --partition=",partition,"
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=",cpus_per_task,"
#SBATCH --mem-per-cpu=",mem_per_cpu,"
")}

  set={paste0('samplelist=',work_dir,'samplelist

echo start: `date +"%Y-%m-%d %T"`
start=`date +%s`

####################
')}

  set2={paste0('echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
START=$SLURM_ARRAY_TASK_ID

NUMLINES=',num_each_array,' #how many sample in one array

STOP=$((SLURM_ARRAY_TASK_ID*NUMLINES))
START="$(($STOP - $(($NUMLINES - 1))))"

#set the min and max
if [ $START -lt 1 ]
then
  START=1
fi
if [ $STOP -gt ',all_sample_num,' ]
then
  STOP=',all_sample_num,'
fi

echo "START=$START"
echo "STOP=$STOP"

####################
')}

  loop1={paste0('for (( N = $START; N <= $STOP; N++ ))
do
  sample=$(head -n "$N" $samplelist | tail -n 1)
  echo $sample')}

  if(step%in%names(micro_works))work={micro_works[[step]]}
  else work="
  do something"

  loop2="done
"

  end={paste0('
##############
echo end: `date +"%Y-%m-%d %T"`
end=`date +%s`
echo TIME:`expr $end - $start`s')}

  if(step=="kraken"){
    res_text=paste0("#!/bin/bash
#SBATCH --job-name=kraken2M
#SBATCH --output=",work_dir,"log/%x_%A_%a.out
#SBATCH --error=",work_dir,"log/%x_%A_%a.err
#SBATCH --time=14-00:00:00
#SBATCH --partition=mem
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=100G

fqp=",work_dir,"rm_human
python /share/home/jianglab/shared/krakenDB/K2ols/kraken2M.py -t 32 \\
    -i ${fqp} \\
    -c 0.05 \\
    -s _1.fastq,_2.fastq \\
    -o ",work_dir,"result/kraken/ \\
    -d /share/home/jianglab/shared/krakenDB/mydb2 \\
    -k ~/miniconda3/envs/waste/bin/kraken2 \\
    -kt /share/home/jianglab/shared/krakenDB/K2ols/KrakenTools")
  }

  if(step%in%c("fastp","rm_human","kraken2","megahit","prodigal","salmon-quant"))
    res_text=paste0(header,set,set2,loop1,work,loop2,end)
  else res_text=paste0(header,set,work,end)

  lib_ps("clipr", library = F)
  clipr::write_clip(res_text)
  message(res_text)
}

#' Re-install my packages
#' @param pkgs pkgs
#'
#' @export
reinstall_my_packages=function(pkgs=c("pcutils","pctax","MetaNet","ReporterScore")){
  for (i in pkgs) {
    devtools::document(paste0("~/Documents/R/",i))
    tryCatch({
      system(paste0("R CMD install ~/Documents/R/",i))
    },error=function(e){
        warning(i, "failed, please check.")
      }
             )
  }
}


