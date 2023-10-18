#' Load BirdFlow models from a collection
#'
#' Load a named model from the current (likely the default) model collection.
#' If the model doesn't exist in the local cache or isn't up-to-date the the
#' cache will be updated prior to loading.
#'
#' @param model The model name to load
#' @param update If `TRUE` (the default) then both the index and cached model
#' file are checked against the server's version to make sure they are
#' up-to-date and downloaded again if they are not.
#' If `FALSE` then neither the index nor the model will be checked.
#' Set to `FALSE` after downloading the model(s) you need if you want to make
#' sure the model does not change during your analysis (even if updated on
#' the server); or if working offline.
#' @param collection_url The url of a collection. Should be the path to
#' the base directory (not an index.html file).
#' @return The designated BirdFlow model is returned.
#' @seealso [load_collection_index()]
#' @examples
#' \dontrun{
#'
#'  index <- load_collection_index()
#'  bf <- load_model(index$model[1])
#'
#' }
#'
#' @export
load_model <- function(model, update = TRUE,
                       collection_url = birdflow_options("collection_url")) {


  collection_url <- gsub("/*$", "/", collection_url) # force trailing slash
  verbose <- birdflow_options("verbose")

  stopifnot(!is.null(model), !is.na(model), is.character(model),
            length(model) == 1)
  index <- load_collection_index(update = update,
                                 collection_url = collection_url)

  if (!model %in% index$model) {
    stop('"', model, '" is not in the current collection. ',
         'Run "load_collection_index()" for information on available models.')
  }
  r <- which(index$model == model)[1] # row associated with model

  file_name <- index$file[r]

  local_path <- file.path(cache_path(collection_url), file_name)
  remote_url <- paste0(collection_url, file_name)
  up_to_date <- FALSE

  if (update && file.exists(local_path)) {
    remote_md5 <- index$md5[r]
    local_md5 <- tools::md5sum(local_path)
    up_to_date <- local_md5 == remote_md5
  }

  if (update && (is.na(up_to_date) || !up_to_date)) {
    if (verbose) {
      cat("Downloading ", model, "\n\tFrom:", remote_url, "\n\tTo:",
          local_path, "\n", sep = "")
    }
    utils::download.file(remote_url, local_path, quiet = TRUE, mode = "wb")
    make_cache_readme(collection_url)
  }

  if (!file.exists(local_path)) {
    if (update == FALSE) {
      stop("Do not set update to FALSE unless you have already downloaded",
           "the model")
    }
    stop("Model file download failed.")
  }

  return(readRDS(local_path))
}
