# Create an initial parameter file where all values in a column are the same

Create an initial parameter file where all values in a column are the
same

## Usage

``` r
make_paramfile(inndat, ...)
```

## Arguments

- inndat:

  Either i) a multiple time series object or ii) a data frame with time
  variable in the first column. In both cases columns must be named.

- ...:

  Additional arguments passed to x13_spec or x13_both

## Value

A data frame.

## Details

The '...' parameter can include any combination of arguments and their
values that are valid for the functions 'x13_spec' and 'x13_both'.

## Examples

``` r

set.seed(123)

years <- 2000:2024
ts1 <- runif(length(years), min = 50, max = 150)
ts2 <- runif(length(years), min = 50, max = 150)
ts3 <- runif(length(years), min = 50, max = 150)

inndata <- data.frame(
 year = years,
 tidsserie_1 = ts1,
 tidsserie_2 = ts2,
 tidsserie_3 = ts3
)

tf_test1 <- make_paramfile(inndat = inndata, spec="RSA3")
tf_test2 <- make_paramfile(inndat = inndata,transform.function = "Auto", outlier.ao  = TRUE,
                          outlier.ls  =  TRUE, outlier.tc = FALSE,  corona= TRUE,
                          outlier.from   =  "2022-04-01" , outlier.cv  = 4,
                          identification_end = "c(identaar,12)", identify_outliers   = FALSE,
                          x11.seasonalComp = TRUE, x11.seasonalma = "Msr")
```
