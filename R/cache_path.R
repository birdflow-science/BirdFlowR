#' Internal function to get the local path to the cache for the current
#' model collection.
#'
#' Use [birdflow_options("cache")](birdflow_options()) to get the main cache
#' directory (parent of the collection directory) and
#'[birdflow_options("collection_url")](birdflow_options()) for the current model
#' collection url.
#' @inheritParams load_model
#' @return Path to the local cache for the current collection including a
#' trailing delimiter.
#' @keywords internal
#'
cache_path <- function(collection_url = birdflow_options("collection_url")) {
  # Return path to cache directory for the current  collection
  # url with a trailing delimiter
  url <- collection_url |> tolower()
  url <- gsub("/*$", "/", url) # enforce trailing slash
  cache_dir <- birdflow_options("cache")
  subdir <- digest::digest(tolower(url), serialize = FALSE) |> substr(1, 6)
  return(file.path(cache_dir, subdir, ""))
}
