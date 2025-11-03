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
#'

save_workspace_wrap <- function(models_in,wk_names,wk_file){
  wk <- RJDemetra::new_workspace()
  if(length(models_in) == length(wk_names)){
    for(i in 1:length(models_in)){
      #print(i)
      new_multiprocessing(wk,wk_names[i])
      for(j in 1:length(models_in[[i]])){
        sa_now <- models_in[[i]][j]
        sa_name_now <- names(models_in[[i]])[j]
        add_sa_item(wk,wk_names[i],sa_now[[1]],sa_name_now)
      }
    }
  }
  save_workspace(wk,paste0("/buckets/produkt/sesongjustering/test_workspace.xml"))
}

wk <- new_workspace()
new_multiprocessing(wk, "tjenest_pit")

for(i in 1:length(mysa)){
  add_sa_item(wk,"tjenest_pit",mysa[[i]],names(mysa)[i])
}


save_workspace(wk, file.path(dir, "tjenest_pit.xml"))

