build_collection_index <- function(dir){

  index_path <- file.path(dir, "index.Rds")
  index_md5_path <- file.path(dir, "index_md5.txt")

  model_extension <- "Rds"
  cols <- c("model_name", "species_code", "scientific_name", "common_name",
            "file", "release_date", "md5")
  md_cols <- setdiff(cols, c("file", "release_date", "md5")) # from BF metadata

  # List model files in directory
  files <- list.files(dir, pattern = paste0("^.*\\.", model_extension, "$"),
                  ignore.case = TRUE)
  files <- files[!tolower(files) %in% "index.rds"]

  # Create an empty index table
  index <- matrix(nrow = 0, ncol = length(cols), dimnames = list(NULL, cols)) |>
    as.data.frame()
  index$files <- files

  # Merge in data (if any) from old index
  if (file.exists(index_path)) {
    old_index <- readRDS(index_path)
    new_index <-
      rbind(old_index[ tolower(old_index$files) %in% tolower(files),  drop = FALSE],
            index[ !index$files %in% old_index$files, , drop = FALSE])
    stopifnot(setequal(tolower(new_index$files), tolower(files)))
  }

  index <- index[order(tolower(index$file)), , drop = FALSE]

  index$error <- FALSE # temporary col to track errors

  for(i in seq_len(nrow(index))){
    f <- index$file
    md5 <- tools::md5sum(f)

    if(is.na(index$md5[i] || index$md5 != md5)){ # if new or changed model
      index$md5[i] <- md5
      bf <- readRDS(f)
      if (!inherits(bf, "BirdFlow")) {
        index$error[i] <- TRUE
        next
      }
      for (col in md_cols) {
        index[[col]][i] <- bf$metadata[[col]]
      }
    } # end new or changed
  } # end loop through models
  if (any(index$error)) {
    warning("Some .", model_extension,
            "files do not appear to be valid BirdFlow models",
            'and were skipped: "',
            paste(index$files[index$error], collapse = '", "'), '"', sep = "")
    index <- index[!index$error, , drop = FALSE]
  }
  saveRDS(index, index_path)
  index_md5 <- tools::md5sum(index_path)
  writeLines(index_md5, index_md5_path)
}



