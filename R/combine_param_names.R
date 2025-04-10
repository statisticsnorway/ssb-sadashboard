#' Making a list of possible parameters.
#'
#' @param func1 A function.
#' @param func2 A function.
#'
#' @return A vector of possible parameter names
#'
#' @export
#'
#' @examples
#' possible_parameternames<- combine_param_names(pickmdl::x13_both, RJDemetra::x13_spec)
combine_param_names<- function(func1, func2) {
  list_of_parameters1 <- as.list(args(func1))
  list_of_parameters2 <- as.list(args(func2))
  namelist1 <- names(list_of_parameters1)
  namelist2 <- names(list_of_parameters2)

  possible_parameternames <- unique(c(namelist1, namelist2))
  possible_parameternames <- possible_parameternames[!possible_parameternames %in% c("...", "")]
  return(possible_parameternames)
}