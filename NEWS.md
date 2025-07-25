# pcutils v0.2.9 Notes

## Added

- added `fill_na` function <2025-06-11, Wed>

# pcutils v0.2.8 Notes

## Added

- added function `format_credit_contributions` <2025-05-06, Tue>
- added function `generate_and_write_author_rmd` <2025-05-02, Fri>
- added `check_directory_structure` function <2025-03-27, Thu>

# pcutils v0.2.7 Notes

## Added

- added `get_legend2` function <2025-01-05, Sun>
- added `list_to_dataframe` function <2024-11-19, Tue>
- added `hebing2` function <2024-10-23, Wed>
- added `make_py_pkg` function <2024-10-22, Tue>
- added `distance2df` function <2024-10-22, Tue>
- added `euler` plot for venn <2024-07-20, Sat>
- added `igraph_translator` function <2024-07-11, Thu>

## Others

- modified `gghuan2` code: different legend for different huan <2024-11-30, Sat>

# pcutils v0.2.6 Notes

## Added

- added `download_ncbi_genome_file` function <2024-06-18, Tue>
- added 'paired' argument for `group_box()` <2024-06-14, Fri>
- added `df2distance` function <2024-05-10, Fri>
- added `ggmosaic` function for mosaic plot <2024-03-25, Mon>

## Fixed

- fixed `scale_color_pc()` and `scale_fill_pc()` <2024-04-07, Sun>

# pcutils v0.2.5 Notes

## Others

- fixed all examples without suggested packages <2024-03-19, Tue>
- deleted vignette and other useless functions <2024-03-19, Tue>

## Added

- added package page and R CMD check github action <2024-03-20, Wed>

## Fixed

- fixed the `read.file`. <2024-03-20, Wed>

# pcutils v0.2.4 Notes

## Added

- added `scale_fill(color)_pc` for ggplot. <2024-03-13, Wed>
- added some modes in `group_box`. <2024-03-11, Mon>

## Fixed

- fixed `ggplot_translator`, when some different english words will be translated to a same word. <2024-03-12, Tue>
- solved all the dependencies for CRAN. <2024-03-11, Mon>

## Others

- deleted `get_doi` <2024-03-13, Wed>

# pcutils v0.2.2 Notes

## Added

- added `split_text` for split a long text to multiple parts. <2024-01-19, Fri>
- added `ggplot_translator` to translate a ggplot labels. <2024-01-19, Fri>

## Fixed

- updated `translator`, use '\n'combine query words for a quicker translation. <2024-01-19, Fri>

## Others

- move `my_wordcloud`, `give_you_a_rose`, `DNA_plot`, `chunlian`, `Olympic_rings` to the package `plot4fun`. <2024-01-25, Thu>

# pcutils v0.2.1 Notes

## Fixed

- fixed CRAN check <2024-01-09>
- available on CRAN <https://CRAN.R-project.org/package=pcutils> <2024-01-09>

# pcutils v0.2.0 Notes

## Added

- added some little tools: `copy_df`, `tidai`, `download2`, and `translator`, updated `sanxian`.
- for data processing, added `rm_low`, `t2`, and `pre_number_str`.
- for statistics, added `group_test` and `multireg`.
- added some utils for ggplot: `legend_size`, `ggplot_lim`, and `generate_labels`, very useful.
- for plotting, added `gghist`, `sample_map`, `my_treemap`, `my_voronoi_treemap`, `my_circle_packing`, and `ggheatmap`.
- for projects, added `make_gitbook`, updated `how_to_use_parallel`.
- for developed version: updated `my_sankey`.

# pcutils v0.1.0 Notes

## Fixed

- fixed R CMD check <2023-06-10>
- submit to CRAN <2023-06-11>
- revise and re-submit to CRAN <2023-06-12>
- available on CRAN <https://CRAN.R-project.org/package=pcutils> <2023-06-13>

