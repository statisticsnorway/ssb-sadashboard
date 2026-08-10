# kableExtra table with Quality Indicators.

A function that creates kableExtra tables to be presented in Rmarkdown
report for seasonally adjusted data.  

## Usage

``` r
make_table_fig(view_now, series_now = NULL)
```

## Arguments

- view_now:

  A data frame with quality indicators. Output from make_quality_df().

- series_now:

  names of series to be included in the table. If NULL all series are
  included.

## Value

A data frame with quality indicators, color coded with kableExtra.

## Examples

``` r
myseries <- pickmdl::pickmdl_data("myseries")
spec_1 <- RJDemetra::x13_spec("RSA5c")
spec_2 <- RJDemetra::x13_spec("RSA4c")

# Friedman test and independence of residuals defined by user as part of output from x13_pickmdl().
# Warning if not user defined as part of output.

my_model1 <- pickmdl::x13_pickmdl(myseries,spec=spec_1,
             userdefined=c("diagnostics.seas-sa-friedman","residuals.independence.value"))

my_model2 <- pickmdl::x13_pickmdl(myseries,spec=spec_2,
             userdefined=c("diagnostics.seas-sa-friedman","residuals.independence.value"))

my_models <- list(my_model1,my_model2)

names(my_models) <- c("model_1","model_2")

my_quality <- make_quality_df(my_models)

seasonal_indicators <- my_quality[[1]]
regarima_indicators <- my_quality[[2]]

seasonal_table <- make_table_fig(seasonal_indicators)
regarima_table <- make_table_fig(regarima_indicators)
```
