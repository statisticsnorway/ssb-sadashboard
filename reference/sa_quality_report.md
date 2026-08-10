# Quality Report for Seasonal Adjustment with RJDemetra.

Function for creating a quality report for seasonal adjustment with
RJdemetra.

## Usage

``` r
sa_quality_report(
  models_in,
  report_file,
  title = NULL,
  author = NULL,
  group_series = NULL,
  plots_included = TRUE,
  plot_start = NULL,
  outlier_choiche = 1,
  spec_file = NULL,
  linearized = FALSE,
  cal_adjust = FALSE,
  ma_filter = FALSE,
  n_digits = 2
)
```

## Arguments

- models_in:

  List of output objects from x13_pickmdl()-function.  
  Must be named, i.e. names(models_in) can not be empty.

- report_file:

  Name of output file, including path.

- title:

  Title of the report. Default is NULL.

- author:

  Author of the report. Default is NULL.

- group_series:

  List of vectors with model names that defines structuring of tables
  and plots.  
  If NULL, all models are included in a single table. See examples.
  Default is NULL.  

- plots_included:

  If TRUE, interactive plots are included in the report. Default is TRUE

- plot_start:

  Start date of time axis in plot. If NULL, the whole time series is
  plotted. Default is NULL.

- outlier_choiche:

  How outliers are counted  

  - 1: All outliers are counted.

  - 2: When identifcation_end = TRUE and identify_outliers = TRUE
    (default), only outliers after identification end are counted, i.e.
    only after date of ARIMA model choice. When identification_end =
    TRUE and identify_outliers = FALSE, all outliers that are not
    pre-specified are counted. When identification_end = FALSE, no
    outliers are counted.

  - 3: When corona = TRUE, only outliers outside corona period are
    counted. When corona = FALSE, all outliers are counted.

  - 4: All outliers that are not pre-specified are counted.

  Default is 1.  

- spec_file:

  When outlier_choiche is 3 or 4, data frame with specifications must be
  given as input. Default is NULL.

- linearized:

  Linearized series to be shown in plots? Default is FALSE.  
  "decomposition.b1" must be userdefined in x13_pickmdl() for linearized
  to be calculated.

- cal_adjust:

  Calender adjusted times series to be shown in plots? Default is
  FALSE.  
  "decomposition.a8" and "decomposition.b1" must be userdefined in
  x13_pickmdl() for cal_adjust to be calculated.

- ma_filter:

  1x3 symmetric MA-filter to be shown in plots? Default is FALSE.

- n_digits:

  number of printed digits. Default is 2.

## Value

A html-document with quality report created at path given in parameter
report_file.

## Examples

``` r

time_series <- sadashboard::vhi
spec_now <- RJDemetra::x13_spec("RSA5c")

my_models <- list()

# Friedman test and independence of residuals must be user defined as output from x13_pickmdl().
# Warning if not user defined as part of output.

for(i in 1:ncol(time_series)){
  time_series_now <- time_series[,i]
  my_models[[i]] <- pickmdl::x13_pickmdl(time_series_now,spec_now,corona=FALSE,
                       pickmdl_method="first_tryautomdl",
                       userdefined = c("decomposition.a8","decomposition.b1",
                                "diagnostics.seas-sa-friedman","residuals.independence.value"))
}
#> automdl since no pickmdl model ok
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria

names(my_models) <- colnames(time_series)

# Grouping of models
group_key <- c(paste0("47.",1:7),"47.9")
groups_now <- lapply(group_key,
                 function(x){colnames(time_series)[which(grepl(x,colnames(time_series)))]})
names(groups_now) <- group_key

path_now <- getwd()

sa_quality_report(models_in= my_models,report_file = paste0(path_now,"/my_report.html"),
                     group_series = groups_now,
                     title = "Eksempelrapport", author = "SSB")
#> 
#> 
#> processing file: Show_SA_function.Rmd
#> 1/9                 
#> 2/9 [Loading]       
#> 3/9                 
#> 4/9 [mainshow]      
#> 5/9                 
#> 6/9 [figurer_groups]
#> 7/9                 
#> 8/9 [arimashow]     
#> 9/9                 
#> output file: Show_SA_function.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS Show_SA_function.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output /home/runner/work/ssb-sadashboard/ssb-sadashboard/docs/reference/my_report.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpY80t0K/rmarkdown-str213d7c19c976.html 
#> 
#> Output created: my_report.html
```
