
#' Create an initial parameter file where all values in a column are the same
#'
#' @param inndat Either i) a multiple time series object or ii) a data frame with time variable in the first column.
#' In both cases columns must be named.
#' @param ... Additional arguments passed to x13_spec or x13_both
#'
#' @details
#' The '...' parameter can include any combination of arguments and their values that are valid for the functions 'x13_spec' and 'x13_both'.
#' @return A data frame.
#' @export
#'
#' @examples
#'
#' set.seed(123)
#'
#' years <- 2000:2024
#' ts1 <- runif(length(years), min = 50, max = 150)
#' ts2 <- runif(length(years), min = 50, max = 150)
#' ts3 <- runif(length(years), min = 50, max = 150)
#'
#' inndata <- data.frame(
#'  year = years,
#'  tidsserie_1 = ts1,
#'  tidsserie_2 = ts2,
#'  tidsserie_3 = ts3
#')
#'
#' tf_test1 <- make_paramfile(inndat = inndata, spec="RSA3")
#' tf_test2 <- make_paramfile(inndat = inndata,transform.function = "Auto", outlier.ao  = TRUE,
#'                           outlier.ls  =  TRUE, outlier.tc = FALSE,  corona= TRUE,
#'                           outlier.from   =  "2022-04-01" , outlier.cv  = 4,
#'                           identification_end = "c(identaar,12)", identify_outliers   = FALSE,
#'                           x11.seasonalComp = TRUE, x11.seasonalma = "Msr")
make_paramfile <- function(inndat,...) {

  mulige_parametere <- combine_param_names(pickmdl::x13_both, RJDemetra::x13_spec)
  # Capture all the parameters passed to the function
  params <- list(...)

  if (!"spec" %in% names(params)) {
    params$spec <- "RSA5c"
  }

  #Ensure 'userdefined' is included in the parameters
  if (!"userdefined" %in% names(params)) {
    params$userdefined <- c("decomposition.a1","decomposition.a6","decomposition.a7","decomposition.a8","decomposition.b1",
                            "decomposition.d10", "decomposition.d11", "decomposition.d12", "decomposition.d13", "decomposition.d18",
                            "diagnostics.seas-sa-friedman","residuals.independence.value")
  }

  params$userdefined <- paste0('c(', paste0('"', params$userdefined, '"', collapse = ", "), ')')


  if ("identification_end" %in% names(params)) {
    params$identification_end <- params$identification_end
  }

  if ("usrdef.var" %in% names(params)) {
    params$usrdef.var <- params$usrdef.var
  }



  if ("usrdef.outliersDate" %in% names(params)) {
    params$usrdef.outliersDate <- paste0('c(', paste0('"', params$usrdef.outliersDate, '"', collapse = ", "), ')')
  }


  if ("usrdef.outliersType" %in% names(params)) {

    params$usrdef.outliersType <- paste0('c(', paste0('"', params$usrdef.outliersType, '"', collapse = ", "), ')')

  }


  # Check each parameter and add quotes if it's a character
  for (navn in names(params)) {
    if (!navn %in% mulige_parametere){
      warning(paste0("The parameter ", navn, " is included, but is unknown in the list of parameters used in x13_both."))
    }
    if (!navn %in%  c("userdefined","identification_end","usrdef.var","usrdef.outliersType","usrdef.outliersDate")) {
      if (is.character(params[[navn]]) && !is.na(params[[navn]])) {
        params[[navn]] <- paste0('"', params[[navn]], '"')
      } else if (is.logical(params[[navn]])) {
        params[[navn]] <- as.character(params[[navn]])
      } else if (is.numeric(params[[navn]])) {
        params[[navn]] <- as.character(params[[navn]])
      }
    }
  }

  # Create the data frame
  df <- as.data.frame(params)

  if (ncol(df) == 1 && !"spec" %in% names(df)) {
    df <- cbind(df, spec = params$spec)
  }

  datasett1 <- inndat
  if(!is.ts(inndat)){
    datasett1 <- inndat[,-1]
  }

  serienavn <- c(colnames(datasett1))

  # Replicate the rows based on the length of inndat
  df <- df[rep(1, length(serienavn)), ]


  # Add the serienavn column
  df <- cbind(name = serienavn, df)

  # Drop the row names
  rownames(df) <- NULL

  return(df)
}
