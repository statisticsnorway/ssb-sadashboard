# Save RJDemetra output as Workspace

A wrapper function that creates and saves a workspace with the selected
RJDemetra output. The output is saved in a .xml-file that can be opened
in JDemetra+, the graphical user interface.

## Usage

``` r
save_workspace_wrap(models_in, wk_file, wk_names = NULL, multi = FALSE)
```

## Arguments

- models_in:

  A list of output objects from the x13()-function.

- wk_file:

  File name (.xml), including path.

- wk_names:

  Vector with the names of the multi-documents to be created in
  workspace. Default is NULL.

- multi:

  Single or sa-processing units in workspace? Default is FALSE.  

  - FALSE: Create a single sa-processing unit containing all series.

  - TRUE2: Create several sa-processing units. See examples.

## Examples

``` r

library(pickmdl)
#> Loading required package: RJDemetra

# Create a single sa_processing unit in workspace

vhi_ts <- sadashboard::vhi
spec_file_vhi <- make_paramfile(inndat = vhi_ts,spec= "RSA3")
mysa_vhi <- x13_text_frame(spec_file_vhi,series= "vhi_ts")
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria

save_workspace_wrap(models_in = mysa_vhi,wk_file=paste0(getwd(),"/wk_example.xml"))

# Create several sa-processing units in workspace

ledige_ts <- sadashboard::ledige
spec_file_ledige <- make_paramfile(inndat = ledige_ts,spec= "RSA3")
mysa_ledige <- x13_text_frame(spec_file_ledige, series = "ledige_ts")
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria
#> Warning: No model is ok according to criteria

 my_sa_models <- list(
   vhi = mysa_vhi,
   ledige = mysa_ledige
 )

save_workspace_wrap(models_in = my_sa_models,wk_names = c("vhi","ledige"),
                  wk_file=paste0(getwd(),"/wk_example.xml"),multi=TRUE)
```
