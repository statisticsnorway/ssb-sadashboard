# Making a list of possible parameters.

Making a list of possible parameters.

## Usage

``` r
combine_param_names(func1, func2)
```

## Arguments

- func1:

  A function.

- func2:

  A function.

## Value

A vector of possible parameter names

## Examples

``` r
possible_parameternames<- combine_param_names(pickmdl::x13_both, RJDemetra::x13_spec)
```
