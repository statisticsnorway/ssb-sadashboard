#' Data frame with seasonally adjusted time series
#'
#' A function that creates a data frame with seasonally adjusted series from x13().
#' Columns in the data frame are date, unadjusted series, seasonally adjusted series and trend.
#' Optionally, linearized series, calendar adjusted series and 1x3 assymmetric MA-filter may be included.
#'
#' @param model_now Output from x13() or x13_pickmdl().
#' @param linearized Linearized times series (B1) to be included in data frame? Default is FALSE.
#' Only available if decomposition.b1 is included in userdefined in x13().
#' @param cal_adjust Calender adjusted times series to be included in data frame? Default is FALSE.
#' Only available if decomposition.a8 and decomposition.b1 are included in userdefined in x13().
#' @param ma_filter 1x3 symmetric MA-filter to be included in data frame? Default is FALSE.
#' @return A data frame with date and selected time series.
#' @export
#' @examples
#'
#' time_series <- sadashboard::vhi[,"47"]
#' my_model <- RJDemetra::x13(time_series,spec="RSA5c")
#' make_ts_df(my_model)
#'
#'# Include linearized series.
#' my_model <- RJDemetra::x13(time_series,spec="RSA5c",
#'              userdefined=c("decomposition.b1"))
#' make_ts_df(my_model,cal_adjust=TRUE)
#'
#' # Include calendar adjusted series.
#' my_model <- RJDemetra::x13(time_series,spec="RSA5c",
#'              userdefined=c("decomposition.a8","decomposition.b1"))
#' make_ts_df(my_model,cal_adjust=TRUE)
#'
#' # Include assymetric 1x3 MA-filter
#' make_ts_df(my_model,ma_filter=TRUE)
#'




make_ts_df <- function(model_now,linearized=F,cal_adjust=F,ma_filter=F){

  freq_now <- stats::frequency(model_now$final$series)
  aux <- ifelse(freq_now==4,3,1)

  start_now <- stats::start(model_now$final$series)

  start_date <- paste0(start_now[1],"-",ifelse((start_now[2]*aux)<10,paste0("0",(start_now[2]*aux)),start_now[2]*aux),"-01")
  end_now <- stats::end(model_now$final$series)
  end_date <- paste0(end_now[1],"-",ifelse((end_now[2]*aux)<10,paste0("0",(end_now[2]*aux)),end_now[2]*aux),"-01")


  if(freq_now==12){
    dato_now <- seq(as.Date(start_date), as.Date(end_date),by = "1 month")
  }else if(freq_now == 4){
    dato_now <- seq(as.Date(start_date), as.Date(end_date),by = "3 months")
  }
  data_now <- stats::ts(model_now$final$series[,1],start=start_now,frequency=freq_now)
  model_sa <- stats::ts(model_now$final$series[,2],start=start_now,frequency=freq_now)
  model_trend <- stats::ts(model_now$final$series[,3],start=start_now,frequency=freq_now)

  ts_df <- data.frame("date"=dato_now,"Ujust"=data_now,"SA"=model_sa,"Trend"=model_trend)

  if(isTRUE(linearized) & !is.null(model_now[["user_defined"]][["decomposition.b1"]])){
    tabb1_now  <- model_now[["user_defined"]][["decomposition.b1"]][1:length(1:length(data_now))]
    tabb1_now  <- stats::ts(c(tabb1_now),start=start_now,frequency=freq_now)
    ts_df <- cbind(ts_df, "B1" = tabb1_now)
  }else if(isTRUE(linearized) & (is.null(model_now[["user_defined"]][["decomposition.b1"]]))){
    warning("linearized could not be calculated. decomposition.b1 is not userdefined as output in x13()")
  }

  if(isTRUE(cal_adjust) & !is.null(model_now[["user_defined"]][["decomposition.a8"]]) & !is.null(model_now[["user_defined"]][["decomposition.b1"]])){
    taba8_now <- model_now[["user_defined"]][["decomposition.a8"]][1:length(data_now)]
    tabb1_now  <- model_now[["user_defined"]][["decomposition.b1"]][1:length(1:length(data_now))]

    if(model_now[["regarima"]][["model"]][["spec_rslt"]][["Log transformation"]] == FALSE){
      model_virk  <- stats::ts(c(taba8_now + tabb1_now),start=start_now,frequency=freq_now)
    }else{
      model_virk  <- stats::ts(c(taba8_now*tabb1_now),start=start_now,frequency=freq_now)
    }
    ts_df <- cbind(ts_df, "cal_adjust" = model_virk)
  }else if(isTRUE(cal_adjust) & (is.null(model_now[["user_defined"]][["decomposition.a8"]]) | is.null(model_now[["user_defined"]][["decomposition.b1"]]))){
    warning("cal_adjust could not be calculated. decomposition.a8 and/or decomposition.b1 are not userdefined as utput in x13()")
  }
  if(isTRUE(ma_filter)){
    ma_sym <- stats::ts(stats::filter(model_sa,filter=rep(1/3,3),method="convolution",2),start=start_now,frequency=freq_now)
    ts_df <- cbind(ts_df,"MA_filter"=ma_sym)
  }

  return(ts_df)
}
