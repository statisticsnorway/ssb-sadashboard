# Quality tables for Seasonal Adjustment with RJDemetra.

Function creates two data frames with selected quality indicators for
seasonal adjustment with RJDemetra at SSB. First data frame contains
quality indicators for sesonality and residual seasonality. Second data
frame contains quality indicators for pre-processing and RegARIMA-model.

## Usage

``` r
make_quality_df(models_in, n_digits = 2, outlier_choiche = 1, spec_file = NULL)
```

## Arguments

- models_in:

  List of output objects from x13_pickmdl()-function.  

- n_digits:

  number of printed digits. Default is 2.

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

  data frame with specifications. Only needed when outlier_choiche is
  set to 3 or 4. This is the data frame with specifications as used in
  x13_text_frame().

## Value

A list of data frames.

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

my_quality <- make_quality_df(my_models)

seasonal_indicators <- my_quality[[1]]
regarima_indicators <- my_quality[[2]]
```
