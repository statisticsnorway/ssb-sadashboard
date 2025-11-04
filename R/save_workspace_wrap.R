#' Save RJDemetra output as Workspace
#'
#' A wrapper function that creates and saves a workspace with the selected RJDemetra output.
#' The output is saved in a .xml-file that can be opened in JDemetra+, the graphical user interface.
#'
#'
#' @param models_in A list of output objects from the x13()-function. One list for each multi-document to be created in workspace.
#' @param wk_names Vector with the names of the multi-documents to be created in workspace
#' @param wk_file File name (.xml), including path.
#' @export
#' @examples
#'
#' my_sa_models <- list()
#'
#' vhi_ts <- sadashboard::vhi
#'
#' spec_file_vhi <- make_paramfile(inndat = vhi_ts,spec= "RSA3")
#' my_sa_models[[1]] <- x13_text_frame(spec_file_vhi,series= "vhi_ts")
#'
#' ledige_ts <- sadashboard::ledige
#' spec_file_ledige <- make_paramfile(inndat = ledige_ts,spec= "RSA3")
#' my_sa_models[[2]] <- x13_text_frame(spec_file_ledige,series= "ledige_ts")
#'
#' save_workspace_wrap(models_in = my_sa_models,wk_names = c("vhi","ledige"),wk_file=paste0(getwd(),"/wk_example.xml"))


save_workspace_wrap <- function(models_in,wk_names,wk_file){
  wk <- RJDemetra::new_workspace()
  if(length(models_in) == length(wk_names)){
    for(i in 1:length(models_in)){
      RJDemetra::new_multiprocessing(wk,wk_names[i])
      for(j in 1:length(models_in[[i]])){
        sa_now <- models_in[[i]][j]
        sa_name_now <- names(models_in[[i]])[j]
        RJDemetra::add_sa_item(wk,wk_names[i],sa_now[[1]],sa_name_now)
      }
    }
  }
  save_workspace(wk,wk_file)
}
