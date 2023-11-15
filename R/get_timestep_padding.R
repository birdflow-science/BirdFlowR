
#' Internal function to determine how much padding to use with transition
#' numbers.
#'
#' Return the level of padding that is used with timestep numbers in transition
#' and marginal names.
#'
#' So far (June 2023) in all BirdFlow models the padding used for timesteps in
#' marginal and transition names has always been 2, as that's what's necessary
#' to support 52 weeks (e.g. T_01-02) but this has, to date, been determined
#' from the number of timesteps in the model. The idea was to not constrain
#' the models to less than 100 transitions if in a hypothetical future we
#' decided say, to have, 3 day timesteps instead of weekly.
#'
#' In June 2023 I added the ability to truncate models (to part of year), and
#' didn't want to change the level of padding when truncating, so instead
#' decided to store the padding in `metadat$timestep_padding`.
#'
#' This function is designed to work with any model regardless of whether the
#' padding is stored in the metadata.
#'
#' @param bf
#'
#' @return An integer indicating how much to pad the numbers in transition
#' names.
#' @keywords internal
get_timestep_padding <- function(bf) {
  padding <- bf$metadata$timestep_padding

  # 1. Use metadata value
  if (!is.null(padding))
    return(padding)

  # The rest of this function is for back compatibility

  # 2. Determine padding from the current padding in marginal names
  if (has_marginals(bf)) {
    mn <- bf$marginals$index$marginal[1]
    padding <- gsub("^M_([[:digit:]]+).*$", "\\1", mn, perl = TRUE) |>
      nchar()
    return(padding)
  }

  # 3. Determine padding from the current padding in transition names
  # This is for the rare model that has transitions but not marginals.
  if (has_transitions(bf)) {
    tn <- names(bf$transitions)[1]
    padding <- gsub("^M_([[:digit:]]+).*$", "\\1", tn, perl = TRUE) |>
      nchar()
    return(padding)
  }

  # 4. Fall back to determining padding from the number of timesteps.
  # Shouldn't ever be needed.
  return(nchar(n_timesteps(bf)))

  stop("Couldn't figure out how much padding to use on transitions")

}
