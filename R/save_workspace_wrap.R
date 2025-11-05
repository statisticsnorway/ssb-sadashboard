#' Save RJDemetra output as Workspace
#'
#' A wrapper function that creates and saves a workspace with the selected RJDemetra output.
#' The output is saved in a .xml-file that can be opened in JDemetra+, the graphical user interface.
#'
#'
#' @param models_in A list of output objects from the x13()-function.
#' @param wk_file File name (.xml), including path.
#' @param wk_names Vector with the names of the multi-documents to be created in workspace. Default is NULL.
#' @param multi Single or sa-processing units in workspace? Default is FALSE. \cr
#' \itemize{
#'  \item FALSE: Create a single sa-processing unit containing all series.
#'  \item TRUE2: Create several sa-processing units. See examples.
#'  }
#' @export
#' @examples
#'
#' library(pickmdl)
#'
#' # Create a single sa_processing unit in workspace
#'
#' vhi_ts <- sadashboard::vhi
#' spec_file_vhi <- make_paramfile(inndat = vhi_ts,spec= "RSA3")
#' mysa_vhi <- x13_text_frame(spec_file_vhi,series= "vhi_ts")
#'
#' save_workspace_wrap(models_in = mysa_vhi,wk_file=paste0(getwd(),"/wk_example.xml"))
#'
#' # Create several sa-processing units in workspace
#'
#' ledige_ts <- sadashboard::ledige
#' spec_file_ledige <- make_paramfile(inndat = ledige_ts,spec= "RSA3")
#' mysa_ledige <- x13_text_frame(spec_file_ledige, series = "ledige_ts")
#'
#'  my_sa_models <- list(
#'    vhi = mysa_vhi,
#'    ledige = mysa_ledige
#'  )
#'
#' save_workspace_wrap(models_in = my_sa_models,wk_names = c("vhi","ledige"),
#'                   wk_file=paste0(getwd(),"/wk_example.xml"),multi=TRUE)


save_workspace_wrap <- function(models_in,wk_file,wk_names = NULL,multi=FALSE){
  wk <- RJDemetra::new_workspace()

  if(!isTRUE(multi) & class(models_in[[1]])[1]=="SA"){
    models_in <- list(models_in)
  }
  if(is.null(wk_names)){
      wk_names <- paste0("sa_processing_",1:length(models_in))
  }
  if(is.null(models_in)){
    stop("models_in missing!")
  }
  if(is.null(wk_file)| !is.character(wk_file) | substr(wk_file,nchar(wk_file)-3,nchar(wk_file))!=".xml"){
    stop("path and filename must be given in wk_file, filename extension must be \".xml\"")
  }
  if(length(models_in)<length(wk_names)){
    wk_names <- wk_names[1:length(models_in)]
  }else if(!is.null(wk_names) & length(models_in)>length(wk_names)){
    wk_names[(length(wk_names)+1):length(models_in)] <- wk_names[(length(wk_names)+1):length(models_in)]
  }

  for(i in 1:length(models_in)){
    RJDemetra::new_multiprocessing(wk,wk_names[i])
    for(j in 1:length(models_in[[i]])){
      sa_now <- models_in[[i]][j]
      sa_name_now <- names(models_in[[i]])[j]
      RJDemetra::add_sa_item(wk,wk_names[i],sa_now[[1]],sa_name_now)
    }
  }
  RJDemetra::save_workspace(wk,wk_file)
}
