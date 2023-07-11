#' Load BirdFlow models from a collection
#'
#' Load a named model from the current (likely the default) model collection.
#' If the model is not cached locally it will be downloaded. If the model
#' doesn't exist in the local cache or isn't up-to-date the the cache will be
#' updated prior to loading.
#'
#' @param model The model name to load
#' @param update If `TRUE` (the default) then both the index and cached model file
#' are checked against the server's version to make sure they are up-to-date and
#' downloaded again if they are not.
#' If `FALSE` then neither the index nor the model will be checked.
#'  Set to `FALSE` after downloading the model(s) you need if you want to make
#' sure the model does not change during your analysis (even if updated on
#' the server); or if working offline.
#' @return The designated BirdFlow model is returned.
#' @seealso [load_collection_index()]
#' @export
load_model <- function(model, update = TRUE){
  stopifnot(!is.null(model), !is.na(model), is.character(model),
            length(model) == 1)
  index <- load_collection_index(update = update)
  if(!model %in% index$model_name){
    stop('"', model, '" is not in the current collection. ',
         'Run "load_collection_index()" for information on available models.')
  }
  r <- which(index$model_name == model)[1] # row associated with model

  file_name <- index$file[r]

  local_path <- paste0(cache_path(), file_name)
  remote_url <- paste0(birdflow_options("collection_url"), file_name)
  up_to_date <- FALSE

  if(update && file.exists(local_path)){
    remote_md5 <- index$md5[r]
    local_md5 <- tools::md5sum(local_path)
    up_to_date <- local_md5 == remote_md5
  }

  if (update && !up_to_date){
    download.file(remote_url, local_path)
  }

  if(!file.exists(local_path)){
    stop("Do no set update to FALSE unless you have already downloaded your model")
  }

  return(readRDS(local_path))

}