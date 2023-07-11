
# Create .birdflow_config environment to hold options
.birdflow_config <- new.env()

# Defualt collection url (for downloading species) should end in "/"
.birdflow_config$collection_url =
  "http://doppler.cs.umass.edu/birdflow/Species/"

.birdflow_config$verbose <- TRUE
.birdflow_config$time_format = "month_day"

# Relationship between parameters and GPU_ram
.birdflow_config$max_param_per_gpu_gb = 23224801

# Local cache (no trailing delimiter - will use with file.path())
.birdflow_config$cache <- tools::R_user_dir("BirdFlowR", "data") |>
  base::normalizePath(winslash = "/", mustWork = FALSE)


#' Set and retrieve BirdFlowR options
#'
#' With no arguments all the BirdFlowR options will be returned as a list. Use a
#' single character value to retreive the value of a single option. Use one or
#' more named arguments to set options.
#'
#' @details
#'
#' \describe{
#' \item{time_format}{Indicates what time format to use to label dimensions of
#' distribution tables and layers of raster objects returned by [get_distr()],
#' [rast()], [rasterize_distr()], and [predict()].  It does not affect
#' internally stored distribution column labels (which are always t1, t2, etc.).
#' Default is "month_day".
#'
#' Valid values are: "timestep" which uses the timestep integer appended to "t"
#'e.g. "t1");  "date" which uses a date in the format year-month-day
#'(as numbers) e.g. "2022-11-23; and "month_day" which uses the name of the
#'month followed by the day of the month e.g. "November 23"  }
#'
#' \item{verbose}{Defaults to `TRUE` for printing of progress and information
#' about the process of functions.  Set to `FALSE` to turn off printing. }
#' }
#'
#' \item{max_param_per_gpu_gb}{
#'   Controls how many parameters can be fit by BirdFlowPy per gigabyte of
#'   GPU Ram.  This is a conservative estimate based on empirical testing.  See
#'   [preprocess_species()]
#' }
#'
#' \item{cache}{
#'   The local directory to store downloaded model files.  Defaults to
#'   `tools::R_user_dir("BirdFlowR", "data")`. This is the base cache directory
#'   within which there will be one or more collection specific
#'   directories, which in turn will hold BirdFlow model files and an index.
#' }
#' \item{collection_url}{
#'   This is the base URL of a collection of model files and its associated
#'   index.  The default is for the standard BirdFlowR model collection.
#' }
#'
#'
#'
#' @param ... One of: (1) one or more named arguments where the name is a
#'   an option and the value its new setting e.g. `verbose = FALSE` ; (2) a
#'   single unnamed argument stating an option to retrieve e.g. `"verbose"`
#'   with an option to retrieve; or (3) No arguments, indicating that all
#'   options and their current settings should be returned in a list.
#'
#' @return If no arguments are used than all options will be returned as a list.
#'   If there is a single, unnamed argument with a character value indicating an
#'   option than the value of that option will be returned. Otherwise, the
#'   arguments should indicate new option settings and nothing will be returned.
#' @export
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' birdflow_options() # print current settings
#' original_format <- birdflow_options("time_format")
#' birdflow_options(time_format = "date")
#' head(get_distr(bf, 1:3))
#' birdflow_options(time_format = original_format)
#'
birdflow_options <- function(...){
  args <- list(...)
  if(length(args) == 0){
    o <- as.list(.birdflow_config)
    o <- o[order(names(o))]
    return(o)
  }

  # Process single unamed arguments by returning the relevant option(s)
  if(length(args) == 1 && is.null(names(args)) ){
    if(length(args[[1]]) > 1){
      stop("You can only retreive one option by name. Use birdflow_options() (no arguments) to retreive all options.")
    }
    if( args[[1]] %in% names(.birdflow_config)  ){
      return(.birdflow_config[[ args[[1]] ]])
    } else {
      stop(paste(
        setdiff(args[[1]], names(.birdflow_config) ) , collapse = ", "),
        " is not a BirdFlowR configuration option.")
    }
  }


  if(any(is.null(names(args))))
    stop("You can not set and retreive options in the same call;",
         " an unamed argument must be the only argument.")

  if(!all(names(args) %in% names(.birdflow_config) ) ){
    missing <- setdiff(names(args), names(.birdflow_config) )
    multiple <- length(missing > 1)
    stop(paste0(missing, collapse = ", "), ifelse(multiple, "are", "is"),
         "not a BirdFlowR configuration option.")
  }

  if("time_format" %in% names(args)){
    tf <- args[["time_format"]]
    valid_values <- c("timestep", "date", "month_day")
    if(length(tf) != 1 || !tf %in% valid_values){
      stop("time_format must be one of '",
           paste(valid_values, collapse = "', '"), "'", sep = "")
    }
    .birdflow_config$time_format <- tf
  }

  if("verbose" %in% names(args)){
    v <- args[["verbose"]]
    if(!is.logical(v) && length(v) == 1 %% v %in% c(TRUE, FALSE)){
      stop("verbose must be TRUE or FALSE.")
    }
    .birdflow_config$verbose <- v
  }


  if("max_param_per_gpu_gb" %in% names(args)){
    mp <- args[["max_param_per_gpu_gb"]]
    if(!(is.numeric(mp) && length(mp) == 1 && !is.na(mp) && mp > 1000)){
      stop("max_param_per_gpu_gb must be a single numeric greater than 1000" )
    }
    .birdflow_config$max_param_per_gpu_gb <- mp
  }

  if("collection_url" %in% names(args)){
    url <- args[["collection_url"]]
    if(!(!is_null(url) && is.character(url) &&
         length(url) == 1 && !is.na(url))){
      stop("Invalid collection_url" )
    }
    url <- gsub("/*$", "/", url) # enforce trailing slash

    .birdflow_config$collection_url <- url
  }

  if("cache" %in% names(args)){
    lc <- args[["cache"]]
    if(!(!is_null(lc) && is.character(lc) &&
         length(lc) == 1 && !is.na(lc))){
      stop("Invalid cache" )
    }
    lc <- gsub("(/|\\\\)*$", base::.Platform$file.sep, lc) # enforce trailing slash

    .birdflow_config$cache <- lc
  }

}

