# pcutils

`pcutils` contains many useful tools or functions for statistics or visualization. I always use this package for productivity.

## install

The stable version is available on CRAN:

```
install.packages("pcutils")
```

Or try the development version, which contains more functions:

```         
install.packages("devtools")
devtools::install_github('Asa12138/pcutils',dependencies=T)
library(pcutils)
```

## project

-   `make_project` build a R project

-   `add_analysis` create a specific Rmd file

## little tools

-   `lib_ps`, `del_ps` can library or detach packages gracefully

-   `dabiao` print a message `dabiao("Message",char = "😀",n = 20)` 😀😀😀😀😀😀Message😀😀😀😀😀😀

-   `copy_vector` help to copy a vector, like `datapasta` package

-   `change_fac_lev` can change a factor levels

-   `sanxian` print a three-line table

-   `rgb2code` convert between r,g,b and color code; `is.ggplot.color` judge a right color; `add_alpha` add a alpha for a color

-   `plotpdf`, `plotgif` print pdf or gif for a plot list

-   `get_cols` generate n colors based on a palette or picture

-   `add_theme` generate a mytheme object for ggplot

-   `remove.outliers` remove the outliers

-   `count2` imitate the `uniq -c` in shell; `strsplit2` is better than `strsplit` for me

-   `grepl.data.frame` do grepl on a dataframe; `hebing` group your dataframe; `explode` expand a column in dataframe

-   `read.file` read some special format files; `trans_format` convert file format like jpg, png, svg, pdf, html...

-   `get_doi` download all supplemental materials from a doi

## statistics

-   `mmscale` do a scale specifying the min and max

-   `twotest` do a two-group test and `multitest` do a multi-group test

-   `fittest` test a vector fit which distribution

-   `toXY` transfer geographical latitude and longitude to XY(m)

## Visualization

![](images/pcutils1.png)

-   `stackplot` plot a bar plot or stack bar plot easily

-   `cor_plot` plot a correlation plot

-   `group_box` plot a boxplot easily

-   `gghuan` plot a doughnut chart

-   `my_lm` fit a linear model and plot

-   `china_map` plot a china map

![](images/pcutils2.png)

-   `my_sankey` plot a sankey plot

-   `my_circo` plot a circlize plot

-   `my_synteny` plot a synteny plot

-   `venn` plot a venn plot

-   `tax_pie` plot a pie plot

-   `tax_radar` plot a radar plot

-   `tax_wordcloud` plot a wordcloud plot

-   `triangp` plot a triangle plot

**Easter Egg**： `my_cat` will show my little cat named GuoDong.
