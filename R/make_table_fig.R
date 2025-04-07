#'
#' kableExtra table with Quality Indicators.
#'
#' A function that creates kableExtra tables to be presented in Rmarkdown report for seasonally adjusted data. \cr
#'
#'
#' @param view_now A data frame with quality indicators. Output from make_quality_df().
#' @param series_now names of series to be included in the table. If NULL all series are included.
#' @return A data frame with quality indicators, including color coding.
#' @export
#'
#' @examples
#' myseries <- pickmdl::pickmdl_data("myseries")
#' spec_1 <- RJDemetra::x13_spec("RSA5c")
#' spec_2 <- RJDemetra::x13_spec("RSA4c")
#'
#' # Friedman test and independence of residuals defined by user as part of output from x13_pickmdl().
#' # Warning if not user defined as part of output.
#'
#' my_model1 <- pickmdl::x13_pickmdl(myseries,spec=spec_1,
#'              userdefined=c("diagnostics.seas-sa-friedman","residuals.independence.value"))
#'
#' my_model2 <- pickmdl::x13_pickmdl(myseries,spec=spec_2,
#'              userdefined=c("diagnostics.seas-sa-friedman","residuals.independence.value"))
#'
#' my_models <- list(my_model1,my_model2)
#'
#' names(my_models) <- c("model_1","model_2")
#'
#' my_quality <- make_quality_df(my_models)
#'
#' seasonal_indicators <- my_quality[[1]]
#' regarima_indicators <- my_quality[[2]]
#'
#' seasonal_table <- make_table_fig(seasonal_indicators)
#' regarima_table <- make_table_fig(regarima_indicators)

make_table_fig <- function(view_now,series_now=NULL){

  if(!is.null(series_now)){
    view_now <- view_now[view_now$Navn %in% series_now,]
  }

  if("M7" %in% colnames(view_now)){

    table_now <- kableExtra::kable_styling(kableExtra::kbl(view_now,caption="*Kvalitetsindikatorer Sesong*",row.names = TRUE)) |>
      kableExtra::kable_classic() |>
      kableExtra::add_header_above(c(" "= 3,"Komponenter"=4,"Residualsesong" =3,"td" = 1))|>
      kableExtra::column_spec(3,color= ifelse(is.na(view_now$Sesong),"black",
                                              ifelse(view_now$Sesong=="Present","green",ifelse(view_now$Sesong =="ProbablyNone", "orange","red")))) |>
      kableExtra::column_spec(4,background=ifelse(is.na(view_now$M7) | is.nan(view_now$M7),"grey",
                                                  ifelse(view_now$M7 == 0,"khaki", ifelse(view_now$M7 < 1,"yellowgreen","tomato"))))  |>
      kableExtra::column_spec(5,background=ifelse(is.na(view_now$M10)|is.nan(view_now$M10),"grey",
                                                  ifelse(view_now$M10 == 0,"khaki", ifelse(view_now$M10 < 1,"yellowgreen",ifelse(view_now$M10 <=1.2,"orange","tomato")))))  |>
      kableExtra::column_spec(6,background=ifelse(is.na(view_now$M11)|is.nan(view_now$M11),"grey",
                                                  ifelse(view_now$M11 == 0,"khaki", ifelse(view_now$M11 < 1,"yellowgreen",ifelse(view_now$M11 <=1.2,"orange","tomato"))))) |>
      kableExtra::column_spec(7,background=ifelse(is.na(view_now$Q)|is.nan(view_now$Q),"grey",
                                                  ifelse(view_now$Q == 0,"khaki",ifelse(view_now$Q < 1,"yellowgreen",ifelse(view_now$Q <=1.2,"orange","tomato")))))  |>
      kableExtra::column_spec(8,background=ifelse(is.na(view_now$qs)|is.nan(view_now$qs),"grey",
                                                  ifelse(view_now$qs >=0.1,"yellowgreen",ifelse(view_now$qs < 0.01,"tomato","orange")))) |>
      kableExtra::column_spec(9,background=ifelse(is.na(view_now$fried)|is.nan(view_now$fried),"grey",
                                                  ifelse(view_now$fried >=0.1,"yellowgreen",ifelse(view_now$fried < 0.01,"tomato","orange")))) |>
      kableExtra::column_spec(10,background=ifelse(is.na(view_now$f_reg)|is.nan(view_now$f_reg),"grey",
                                                   ifelse(view_now$f_reg >=0.1,"yellowgreen",ifelse(view_now$f_reg < 0.01,"tomato","orange")))) |>
      kableExtra::column_spec(11,background=ifelse(is.na(view_now$f_td)|is.nan(view_now$f_td),"grey",
                                                   ifelse(view_now$f_td >=0.1,"yellowgreen",ifelse(view_now$f_td < 0.01,"tomato","orange"))))
  }
  if("ARIMA" %in% colnames(view_now)){

    table_now <- kableExtra::kable_styling(kableExtra::kbl(view_now,caption="*Kvalitetsindikatorer ARIMA*",row.names = TRUE)) |>
      kableExtra::kable_classic() |>
      kableExtra::add_header_above(c(" "= 5,"Koeffisienter"=1,"Residualer" =1,"Pickmdl()"=3) )|>
      kableExtra::column_spec(6,background=ifelse(is.na(view_now$Td_p)|is.nan(view_now$Td_p) ,"grey",ifelse(view_now$Td_p == -99,"lightgrey",
                                                                                                            ifelse(view_now$Td_p < 0.01,"yellowgreen",ifelse(view_now$Td_p >= 0.10,"tomato","orange"))))) |>
      kableExtra::column_spec(7,background=ifelse(is.na(view_now$indRes)|is.nan(view_now$indRes), "grey",
                                                  ifelse(view_now$indRes >=0.10,"yellowgreen",ifelse(view_now$indRes < 0.01,"tomato","orange"))))  |>
      kableExtra::column_spec(8,background=ifelse(is.na(view_now$ok),"grey",ifelse(view_now$ok==TRUE,"yellowgreen","tomato")))|>
      kableExtra::column_spec(9,background=ifelse(is.na(view_now$ok_final),"grey",ifelse(view_now$ok_final==TRUE,"yellowgreen","tomato"))) |>
      kableExtra::column_spec(10,background=ifelse(is.na(view_now$mdl_nr),"grey",ifelse(view_now$mdl_nr==1,"lemonchiffon",
                                                                                        ifelse(view_now$mdl_nr %in% 2:5,"khaki","burlywood"))))
  }

  return(table_now)
}

is.nogood <- function(x){
  y <- FALSE

  if(is.na(x)|is.nan(x)|is.null(x)){
    y <- TRUE
  }
  return(y)
}
