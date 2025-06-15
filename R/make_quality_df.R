#' Quality tables for Seasonal Adjustment with RJDemetra.
#'
#' Function creates two data frames with selected quality indicators for seasonal adjustment with RJDemetra at SSB.
#' First data frame contains quality indicators for sesonality and residual seasonality.
#' Second data frame contains quality indicators for pre-processing and RegARIMA-model.
#'
#' @param models_in list of relevant models. List of output objects from x13_pickmdl()-function. \cr
#' See examples for details.
#' @param n_digits number of printed digits. Default is 2.
#' @param outlier_choiche how to count outliers. Default is 1.
#' @param paramfile data frame with parameters. Only needed when outlier_choiche is set to 3 or 4. This is the data frame with parameter as used in x13_text_frame().
#' @return A list of data frames.
#' @export
#'
#'
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
#' my_quality <- make_quality_df(my_models)
#'
#' seasonal_indicators <- my_quality[[1]]
#' regarima_indicators <- my_quality[[2]]


make_quality_df <- function(models_in,n_digits=2,outlier_choiche,paramfile){

  if(is.null(names(models_in))){
    warning("List with models must be named! Use names(models_in).")
  }

  main_view <- main_results_frame(models_in,n_digits)
  arima_view <- arima_results_frame(models_in,n_digits,outlier_choiche,paramfile)

  if(is.null(models_in[[1]][["user_defined"]][["residuals.independence.value"]])|
     is.null(models_in[[1]][["user_defined"]][["diagnostics.seas-sa-friedman"]])){
    warning("Friedman-test and/or independence of residuals not included. NA inserted.")
  }

  return(list(main_view,arima_view))
}


main_results_frame <- function(models_in,n_digits){#,model_names){

  main_view <- NULL

  for(i in 1:length(models_in)){#_names)){

    name_now <- names(models_in)[i]
    model_now <- models_in[[i]]

    main_results_now <- data.frame(Navn = name_now,Sesong=model_now$diagnostics$combined_test$combined_seasonality_test,
                                   M7 = round(model_now$decomposition$mstats[7],n_digits),
                                   M10 = round(model_now$decomposition$mstats[10],n_digits),
                                   M11 = round(model_now$decomposition$mstats[11],n_digits),
                                   Q = round(model_now$decomposition$mstats[12],n_digits),
                                   qs = round(model_now$diagnostics$residuals_test$P.value[1],n_digits),
                                   fried = ifelse(is.null(model_now[["user_defined"]][["diagnostics.seas-sa-friedman"]]),NA,
                                                  round(model_now[["user_defined"]][["diagnostics.seas-sa-friedman"]][[2]],n_digits)),
                                   f_reg = round(model_now$diagnostics$residuals_test$P.value[3],n_digits),
                                   f_td = round(model_now$diagnostics$residuals_test$P.value[7],n_digits))
    main_view <- rbind(main_view,main_results_now)
  }
  return(main_view)
}


arima_results_frame <- function(models_in,n_digits,outlier_choiche,paramfile){#,model_names){

  arima_view <- NULL

  if(!is.null(paramfile) & outlier_choiche==3 & !("corona" %in% colnames(paramfile))){
    warning("The parameter corona not included in parameter file. All outliers listed.")
  }
  if(is.null(paramfile) & outlier_choiche %in% c(3,4)){
    stop("Parameter file missing. Need to be given as input when outlier_choiche = 3 or outlier_choiche = 4.")
  }
  if(outlier_choiche > 4 | outlier_choiche < 1){
    stop("The parameter outlier_choiche must be between 1 and 4.")
  }

  for(i in 1:length(models_in)){

    name_now <- names(models_in)[i]
    model_now <- models_in[[i]]

    spec_now <- RJDemetra::x13_spec(model_now)
    spec_def_outlier <- nrow(spec_now$regarima$regression$userdef$outliers$Final)
    spec_def_outlier <- ifelse(is.null(spec_def_outlier),0,spec_def_outlier)

    ok_now <- pickmdl::ok(model_now)
    td_p_now <- td_p(model_now)

    if(outlier_choiche == 1){
      outliers_number <- model_now$regarima$model$spec_rslt[8][[1]]
    }else if(outlier_choiche == 2){
      outliers_number <- model_now$regarima$model$spec_rslt[8][[1]] - spec_def_outlier
    }else if(outlier_choiche == 3){
      if("corona" %in% colnames(paramfile)){
        if(paramfile$corona[which(paramfile$name == names(mysa)[[i]])]){
          outliers_number <- model_now$regarima$model$spec_rslt[8][[1]] - 25
        }else{
          outliers_number <- model_now$regarima$model$spec_rslt[8][[1]]
        }
      }else{
        outliers_number <- model_now$regarima$model$spec_rslt[8][[1]]
      }
    }else if(outlier_choiche == 4){
      outliers_number <-  model_now$regarima$model$spec_rslt[8][[1]]
      outliers_twice <- 0

      if(all(c("usrdef.outliersEnabled","usrdef.outliersType", "usrdef.outliersDate") %in% colnames(paramfile))){

        if(isTRUE(as.logical(paramfile$usrdef.outliersEnabled[[which(paramfile$name == names(mysa)[[i]])]]))){
          type_now <- paramfile$usrdef.outliersType[[which(paramfile$name == names(mysa)[[i]])]]
          type_now <- strsplit(gsub("c\\(|\\)", "", type_now), ", ")[[1]]
          type_now <- gsub("\"", "", type_now)
          spec_def_outlier <- length(type_now %in% c("AO","LS","TS","SO"))
          outliers_number <- outliers_number - spec_def_outlier

          def_date_now <- paramfile$usrdef.outliersDate[[which(paramfile$name == names(mysa)[[i]])]]
          def_date_now <- strsplit(gsub("c\\(|\\)", "", def_date_now), ", ")[[1]]
          def_date_now <- gsub("\"", "", def_date_now)

          corona_dates <- seq(as.Date("2020-03-01"), as.Date("2022-03-01"),by = "1 month")
          outliers_twice <- sum(def_date_now %in% corona_dates)

        }

        if("corona" %in% colnames(paramfile)){
          if(paramfile$corona[which(paramfile$name == names(mysa)[[i]])]){
            outliers_number <- outliers_number - 25 + outliers_twice

          }
        }

      }else{
        warning(paste0(name_now,": Userdefined outliers not defined correctly in paramfile. All outliers listed."))
      }

    }

    arima_results_now  <- data.frame(Navn = name_now,
                                     log=ifelse(model_now$regarima$model$spec_rslt[3][[1]],"yes","no"),
                                     ARIMA = paste0("(",model_now$regarima$arma[1],",",model_now$regarima$arma[2],",",model_now$regarima$arma[3],")(",
                                                    model_now$regarima$arma[4],",",model_now$regarima$arma[5],",",model_now$regarima$arma[6],")"),
                                     outliers = model_now$regarima$model$spec_rslt[8][[1]]- spec_def_outlier,
                                     Td_p = round(td_p_now,n_digits),
                                     indRes = ifelse(is.null(model_now$user_defined$residuals.independence.value),NA,
                                                     round(model_now$user_defined$residuals.independence.value,n_digits)),
                                     ok = ok_now$ok, ok_final = ok_now$ok_final, mdl_nr = ok_now$mdl_nr)
    arima_view <- rbind(arima_view,arima_results_now)
  }
  return(arima_view)
}