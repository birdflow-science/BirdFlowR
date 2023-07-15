#' Internal function to create or update readme files within the local
#' BirdFlow model cache.
#'
#' This is called anytime new files are downloaded to the cache by
#' [load_collection_index()] or [load_model()].
#' @returns Nothing is returned
#'
#' @keywords internal
make_cache_readme <-
  function(collection_url = birdflow_options("collection_url")){

  main_readme_path <- file.path(birdflow_options("cache"), "readme.txt")
  collection_readme_path <- paste0(cache_path(collection_url), "readme.txt")

  main <- system.file("readme_templates/main_cache_readme.txt",
                      package = "BirdFlowR") |>  readLines()

  collection <- system.file("readme_templates/collection_cache_readme.txt",
                            package = "BirdFlowR") |>  readLines()

  # Local objects that exactly match field names in templates
  # eg "[date]" is a field in the template
  date <-  lubridate::today() |> as.character()
  cache_path <- cache_path(collection_url) # local variable for the code below

  # For each text object in memory and for each field
  # replace the field alias with it's value.
  for (obj_name in c("main", "collection")) {
    text <- get(obj_name)
    for(field in c("date", "collection_url", "cache_path")){
      text <- gsub(paste0("[", field, "]"), get(field), text, fixed = TRUE)
    }
    assign(obj_name, text)
  }

  # Write
  writeLines(main, main_readme_path)
  writeLines(collection, collection_readme_path)
  invisible()
}
