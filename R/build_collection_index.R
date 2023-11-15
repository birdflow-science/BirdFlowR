#' Convert a directory of BirdFlow models into a collection
#'
#' A BirdFlow model collection consists of 1 or more BirdFlow objects each
#' saved to a single `.Rds` file, an `index.Rds` file, an `index.html` file and
#' a `index_md5.txt` file. This function converts a directory with model `.Rds`
#' files into a collection. It needs to be run locally to an R session so likely
#' the resulting directory will be synchronized with a server after it is
#' created.
#'
#' It is primarily intended for use by the BirdFlow team but is exported from
#' the package so anyone can use it.
#'
#' @param dir A local directory for containing BirdFlow models saved as `.Rds`
#' files. The base file name will be used as the model name and other metadata
#' will be extracted from the BirdFlow object.
#'
#' @param collection_url The final URL for the BirdFlow collection (where it
#' will be served).
#'
#' @export
#' @keywords internal
#'
build_collection_index <- function(dir, collection_url) {

  verbose <- birdflow_options("verbose")
  index_path <- file.path(dir, "index.Rds")
  index_md5_path <- file.path(dir, "index_md5.txt")

  model_extension <- "Rds"
  cols <- c("model", "species_code", "scientific_name", "common_name",
            "file", "release_date", "md5", "version", "size")
  species_cols <- c("species_code", "scientific_name", "common_name")

   # List model files in directory
  files <- list.files(dir, pattern = paste0("^.*\\.", model_extension, "$"),
                  ignore.case = TRUE)
  files <- files[!tolower(files) %in% "index.rds"]

  if (length(files) == 0)
    stop("No model files detected.")

  # Create an empty index table
  index <- matrix(nrow = length(files), ncol = length(cols),
                  dimnames = list(NULL, cols)) |>
    as.data.frame()
  index$file <- files
  index$model <- gsub("\\.Rds", "", files, ignore.case = TRUE)

  # Merge in data (if any) from old index
  if (file.exists(index_path)) {
    old_index <- readRDS(index_path)
    old_index <- old_index[tolower(old_index$file) %in% tolower(files), ,
                           drop = FALSE]
    old_cols <- colnames(old_index)
    if (setequal(old_cols, cols) && all(old_cols == cols) &&
       any(old_index$file %in% files)) {
      if (all(files %in% old_index$file)) {
        new_index <- old_index
      } else {
        new_index <-
          rbind(old_index,
                index[!index$files %in% old_index$files, , drop = FALSE])

      }
      stopifnot(setequal(tolower(new_index$file), tolower(files)))
      index <- new_index
    }
  }

  index <- index[order(tolower(index$file)), , drop = FALSE]

  index$error <- FALSE # temporary col to track errors

  for (i in seq_len(nrow(index))) {
    f <- file.path(dir, index$file[i])
    md5 <- as.character(tools::md5sum(f))

    if (is.na(index$md5[i]) || index$md5[i] != md5) { # if new or changed model
      if (verbose)
        cat("Reading metadata for ", index$model[i], "\n", sep = "")
      index$md5[i] <- md5
      bf <- readRDS(f)
      if (!inherits(bf, "BirdFlow")) {
        index$error[i] <- TRUE
        next
      }
      for (col in species_cols) {
        index[[col]][i] <- bf$species[[col]]
      }
      index$release_date[i] <- as.character(lubridate::today())
      index$size[i] <- file.size(f) / (1000^2)
    } # end new or changed
  } # end loop through models
  if (any(index$error)) {
    warning("Some .", model_extension,
            "files do not appear to be valid BirdFlow models",
            'and were skipped: "',
            paste(index$files[index$error], collapse = '", "'), '"', sep = "")
    index <- index[!index$error, , drop = FALSE]
  }
  index$error <- NULL # drop temporary column

  index$version <- "beta"


  # Save index .Rds
  saveRDS(index, index_path)

  # Download logo
  logo_file <-   file.path(dir, "logo.png")
  if (!file.exists(logo_file)) {
    utils::download.file(
      "https://birdflow-science.github.io/BirdFlowR/logo.png",
      destfile = logo_file, method = "wget")
  }
  # Save index.htm
  model <- index$model[1]

  rmd_file <- file.path(dir, "index_template.Rmd")

  # Edit text in rmd to reference this specific collection url and the first
  # model in it
  # I couldn't figure out how to do inline text within a code block - the
  # reverse of inline code in the document text so I'm editing the .Rmd
  # here
  text <- readLines(system.file("markdown_templates/collection_index.Rmd",
                                package = "BirdFlowR"))
  text <- gsub("[collection_url]", collection_url, text, fixed = TRUE)
  text <- gsub("[model]", model, text, fixed = TRUE)
  writeLines(text, rmd_file)

  # Add information on whether the report exists to the index
  # Index is already saved so it won't persist in the file
  # but is used by the markdown document
  index$report_exists <- file.exists(
    file.path(dir, paste0(index$model, ".html")))

  rmarkdown::render(
    input = rmd_file,
    output_file = file.path(dir, "index.html"),
    params = list(index = index, collection_url = collection_url))

  file.remove(rmd_file)

  # Write separate index.md5 file
  index_md5 <- as.character(tools::md5sum(index_path))
  writeLines(index_md5, index_md5_path)
}
