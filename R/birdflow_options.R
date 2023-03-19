
# Create .birdflow_config environment to hold options
.birdflow_config <- new.env()
.birdflow_config$verbose <- TRUE
.birdflow_config$time_format = "month_day"


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
#' [rast()], [rasterize_distr()], and [forecast()].  It does not affect
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
#'
#' }
#'
#'
#' @param ... One of: (1) one or more named arguments where the name is a
#'   an option and the value its new settting e.g. `verbose = FALSE` ; (2) a
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
#' birdflow_options(time_format = "month_day")
#' head(get_distr(bf, 1:3))
#' birdflow_options(time_format = original_format)
#'
birdflow_options <- function(...){
  args <- list(...)
  if(length(args) == 0){
    return(as.list(.birdflow_config))
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

}

