#' Test of trading day variables in RegARIMA
#'
#' F-test of trading days variables in RegARIMA pre-processing with RJDemetra. \cr
#' \eqn{H_{0}}: There are no trading days effects. \cr
#' Trading days variables as defined by JDemetra or as by user with pickmdl::konstruksjon().
#' @param model_now output from x13() or x13_pickmdl().
#' @return P-value of the F-test.
#' @export
#'
#' @examples
#' time_series <- sadashboard::vhi[,"47.3"]
#' my_model <- RJDemetra::x13(time_series,spec="RSA5c")
#'
#' td_p(my_model)
#'


td_p <- function(model_now){

  if(TRUE %in% (rownames(model_now$regarima$regression.coefficients) %in% c("td15","td16","Week days","hverdag"))){
    here_now <- which(rownames(model_now$regarima$regression.coefficients) %in% c("td15","td16", "Week days","hverdag"))
    td_p_now <- tryCatch({stats::pt(abs(model_now$regarima$regression.coefficients[here_now,3]),
                                    model_now$regarima[[5]][3]-model_now$regarima[[5]][2],lower.tail=FALSE)},error=function(e){-99})
  }else if(TRUE %in% (rownames(model_now$regarima$regression.coefficients) %in% c("man","Monday"))){
    here_now <- which(rownames(model_now$regarima$regression.coefficients) %in% c("man","tir","ons","tor","fre","lor",
                                                                                  "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
    td_p_now <- tryCatch({car::linearHypothesis(model_now,rownames(model_now$regarima$regression.coefficients)[here_now],
                                                rep(0,length(here_now)),test="F")[2,4]},error=function(e){-99})
  }else if(TRUE %in% (rownames(model_now$regarima$regression.coefficients) %in% "gr_1")){
    here_now <- which(rownames(model_now$regarima$regression.coefficients) %in% paste0("gr_",1:6))
    td_p_now <- tryCatch({car::linearHypothesis(model_now,rownames(model_now$regarima$regression.coefficients)[here_now],
                                                rep(0,length(here_now)),test="F")[2,4]},error=function(e){-99})
  }else{
    td_p_now <- NA
  }

  return(td_p_now)
}

