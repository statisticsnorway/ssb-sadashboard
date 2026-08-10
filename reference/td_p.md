# Test of trading day variables in RegARIMA

F-test of trading days variables in RegARIMA pre-processing with
RJDemetra.  
\\H\_{0}\\: There are no trading days effects.  
Trading days variables as defined by JDemetra or as by user with
pickmdl::konstruksjon().

## Usage

``` r
td_p(model_now)
```

## Arguments

- model_now:

  output from x13() or x13_pickmdl().

## Value

P-value of the F-test.

## Examples

``` r
time_series <- sadashboard::vhi[,"47.3"]
my_model <- RJDemetra::x13(time_series,spec="RSA5c")

td_p(my_model)
#> [1] 0.02263526
```
