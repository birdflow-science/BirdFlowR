#' reformatt distr time labels based on birdflow_options("time_format")
#'
#' This is an internal function to change the labels of an object containing
#' distributions to match the time format the user has specified in the global
#' options. Internally distributions are stored and generated with labels
#' based on timestep ("t1"  etc.).
#'
#' As of March 15, 2023 I'm experimenting with adding an attribute "time" to a
#' vector distribution to keep track of the time label.
#'
#' If there are more than one distribution they are stored as a matrix and
#' the colnames store the time associated with each distribution.
#'
#' This function currently returns the input object as is if it cannot resolve
#' labels.
#'
#' @param x An object containing bird distributions
#' @param bf A BirdFlow object (used for its date information)
#'
#' @return `x` with (potentially) new labels
#' @keywords internal
reformat_distr_labels <- function( x, bf){

  time_format <- birdflow_options("time_format")
  if(time_format == "timestep")
    return(x)

  d <- dim(x)
  ndim <- ifelse(is.null(d), 1, length(d))

  # single, vector distribution
  if(ndim == 1){
    timestep <- attr(x, "time")
    if(is.null(timestep))
      return(x)  # cannot update missing dimnames, return as is
    attr(x, "time") <- reformat_timestep(timestep, bf)
    return(x)
  }

  # Matrix  or array update names for time dimension
  dn <- dimnames(x)

  if(is.null(dn))
    return(x)

  time_dimension <- which(names(dn) %in%  c("timestep", "time"))[1] # as of 3/18 should be "time" older models used "timestep"

  if(length(time_dimension) == 0 || is.na(time_dimension)){
    # in preprocessed models the second dimension has "t1"... labels
    # but the dimension itself doesn't have a name.
    # I don't want to change this because it would detriment the
    # hdf5 file, so I'm catching that condition here instead.
    if (ndim == 2 && all(grepl("^t[[:digit:]]+$", dn[[2]]))) {
      time_dimension <- 2
    }
  }

  # back compatability code:
  names(dn)[time_dimension] <- "time"

  # Check for timestep column names
  if(!all(grepl("^t[[:digit:]]+$", dn[[time_dimension]])) &&
     !is.null(dn[[time_dimension]]))
    return(x)

  time_labels <- reformat_timestep(dn[[time_dimension]], bf)

  dn[[time_dimension]] <- time_labels

  dimnames(x) <- dn

  return(x)
}
