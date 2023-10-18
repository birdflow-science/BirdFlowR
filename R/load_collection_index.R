#' Load the index to a remote model collection
#'
#' Load the collection index for the current model collection.  This function
#' will return an up-to-date index for the current BirdFlow collection. It
#' caches the index locally and will update the cached version if it is not
#' up-to-date.
#'
#' The collection will default to the main BirdFlow model collection and most
#' users will not need to set it.
#'
#' The local cache directory (for all collections) defaults to
#' [birdflow_options("cache")](birdflow_otions()) the cache directory for the
#' current collection will be in a subdirectory.  Both of the above options
#' can be changed for the duration of the session with [birdflow_options()],
#' but the defaults should be suitable for most users.
#' @inheritParams load_model
#' @return  A data frame with a row for every model in the collection.
#'
#' @export
load_collection_index <-
  function(update = TRUE,
           collection_url = birdflow_options("collection_url")) {

  local_index <- file.path(cache_path(collection_url), "index.Rds")

  collection_url <- gsub("/*$", "/", collection_url)  # force trailing slash

  if (!update) {
    if (!file.exists(local_index)) {
      stop("Do no set update to FALSE unless you have already downloaded your ",
      "model")
    }
    return(readRDS(local_index))
  }


  md5_url <- paste0(collection_url, "index_md5.txt")
  index_url <- paste0(collection_url, "index.Rds")

  up_to_date <- FALSE

  if (file.exists(local_index)) {
    local_md5 <- tools::md5sum(local_index)
    remote_md5 <- base::readLines(md5_url)
    up_to_date <- local_md5 == remote_md5
  }

  if (!up_to_date) {
    if (birdflow_options("verbose"))
      cat("Downloading collection index\n")
    dir.create(dirname(local_index), recursive = TRUE, showWarnings = FALSE)
    utils::download.file(index_url, local_index, mode = "wb")
    make_cache_readme(collection_url)
  }
  return(readRDS(local_index))
}
