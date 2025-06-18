#' Quality Report for Seasonal Adjustment with RJDemetra.
#'
#' Wrapper function for creating a html-document with interactive quality report for seasonal adjustment with RJdemetra.
#' The quality report includes tables with selected quality indicators. User may also choose to include interactive plots
#' of seasonally adjusted time series.
#'
#' @param models_in List of output objects from x13_pickmdl()-function. \cr
#' Must be named, names(models_in) cannot be empty.
#' @param report_file Name of output file, including path.
#' @param title Title of the report. Default is NULL.
#' @param author Author of the report. Default is NULL.
#' @param group_series List of vectors of model names. For structuring of tables and plots. \cr
#' If NULL, all models are included in a single table. See examples. Default is NULL. \cr
#' Preferably, groups should be named. If names(group_series) = NULL, some headers in output are empty.
#' @param plots_included If TRUE, interactive plots are included in the report. Default is TRUE
#' @param plot_start Start date of time axis in plot. If NULL, the whole time series is plotted. Default is NULL.
#' @param outlier_choiche how to count outliers in report. Default is 1. \cr
#'  1: All outliers are counted.
#'  2 : When identifcation_end = TRUE and identify_outliers = TRUE (default), only outliers after identification end are counted, i.e. only after date of ARIMA model choice.
#'  When identification_end = TRUE and identify_outliers = FALSE, all outliers that are not pre-specified are counted.
#'  When identification_end = FALSE, no outliers are counted.
#'  3 : When corona = TRUE, only outliers outside corona period are counted. When corona = FALSE, all outliers are counted.
#'  4 : All outliers that are not pre-specified are counted.
#' @param spec_file When outlier_choiche is 3 or 4, data frame with specifications (used in x_13_text_frame()) needs to be given as input. Default is NULL.
#' @param linearized Linearized series to be shown in plots? Default is FALSE.
#' "decomposition.b1" must be userdefined in x13_pickmdl() for linearized to be calculated.
#' @param cal_adjust Calender adjusted times series to be shown in plots? Default is FALSE.
#' "decomposition.a8" and "decomposition.b1" must be userdefined in x13_pickmdl() for cal_adjust to be calculated.
#' @param ma_filter 1x3 asymmetric MA-filter to be shown in plots? Default is FALSE.
#' @param n_digits number of printed digits. Default is 2.
#' @return A html-document with quality report created at selected path.
#' @export
#' @examples
#'
#' time_series <- sadashboard::vhi
#' spec_now <- RJDemetra::x13_spec("RSA5c")
#'
#' my_models <- list()
#'
#' # Friedman test and independence of residuals must be user defined as output from x13_pickmdl().
#' # Warning if not user defined as part of output.
#'
#'for(i in 1:ncol(time_series)){
#'   time_series_now <- time_series[,i]
#'   my_models[[i]] <- pickmdl::x13_pickmdl(time_series_now,spec_now,corona=FALSE,
#'                        pickmdl_method="first_tryautomdl",
#'                        userdefined = c("decomposition.a8","decomposition.b1",
#'                                 "diagnostics.seas-sa-friedman","residuals.independence.value"))
#'}
#'
#' names(my_models) <- colnames(time_series)
#'
#' # Grouping of models
#' group_key <- c(paste0("47.",1:7),"47.9")
#' groups_now <- lapply(group_key,
#'                  function(x){colnames(time_series)[which(grepl(x,colnames(time_series)))]})
#' names(groups_now) <- group_key
#'
#' path_now <- getwd()
#'
#' sa_quality_report(models_in= my_models,report_file = paste0(path_now,"/my_report.html"),
#'                      group_series = groups_now,
#'                      title = "Eksempelrapport", author = "SSB")







sa_quality_report <- function(models_in, report_file, title=NULL ,author = NULL,
                              group_series= NULL,plots_included =TRUE,plot_start =NULL,
                              outlier_choiche = 1, spec_file = NULL,
                              linearized = FALSE,cal_adjust = FALSE, ma_filter= FALSE, n_digits = 2){

  if(is.null(names(models_in))){
    stop("names(models_in) = NULL ; models_in must be named.")
  }

  if(is.null(title)){
    title <- "Quality Report"
  }
  if(is.null(author)){
    author <- ""
  }

  if(is.null(group_series)){
    group_series <- list()
    group_series[[1]] <- names(models_in)
    group_names <- ""
  }else if(!is.null(group_series)){
    group_names <- names(group_series)
    if(is.null(group_names)){
      group_names <- paste0("Gruppe ",1:length(group_series))
    }
  }

  quality_df <- make_quality_df(models_in,n_digits,outlier_choiche,spec_file)

  group_season_table <- lapply(group_series,function(x){make_table_fig(quality_df[[1]],x)})
  group_arima_table <- lapply(group_series,function(x){make_table_fig(quality_df[[2]],x)})

  group_plots <- NULL

  if(isTRUE(plots_included)){
    group_plots <- lapply(group_series,function(x){make_plot_fig(models_in,x,plot_start,linearized,cal_adjust,ma_filter)})
    group_plots <- lapply(group_plots,function(x){make_taglist(x)})
  }

  where_rmd <- paste0(system.file(package="sadashboard"),"/Show_SA_function.Rmd")

  Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64/")

  env_now <- rlang::env(title = title, author = author,group_series = group_series,
                        group_season_table= group_season_table, group_arima_table = group_arima_table,
                        group_plots = group_plots,plots_included = plots_included)
  rmarkdown::render(input=where_rmd,output_file=report_file,envir = env_now,output_format ="html_document")
}

