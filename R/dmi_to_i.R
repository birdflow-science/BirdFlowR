
#' Convert between dynamic mask index and static location index
#'
#' `dmi_to_i()` and `i_to_dmi()` are for internal and advanced use;
#' they are not likely to be helpful to most users.
#' See [index conversions](i_to_x) for, likely, more useful conversions.
#' These two functions convert between indices along the cells that are
#' included in the dynamic mask (`dmi`) and standard location
#' indices (`i`) along the cells that are included by the static mask.
#' This conversion requires knowing the `timestep`
#' associated with each `dmi` or `i` value
#' as the mapping between the two is different for each timestep.
#' @param dmi Dynamic mask index values.  These will always be integers between
#' `1` and the sum of the dynamic mask for the given timestep.
#' @param i Location index. This indicates a location based on an index of the
#' cells included by the static mask. These start in the top left location and
#' fill in by row.
#' @param timestep Either a single timestep to be used for all conversions
#' or a vector of the timesteps associated with each input value: `dmi` for
#' `dmi_to_i()` or `i` for `i_to_dmi()`.
#' @param bf A BirdFlowR model. Note the conversion is specific to this
#' model and not valid for any others.
#' @returns The location index,`i`, of each location indicated by `dmi` or
#' vis-versa.
#' @export
#' @examples
#'  bf <- BirdFlowModels::amewoo
#'  dmi <- c(11:20)
#'  timesteps <- c(1, 1, 1, 1, 3, 3, 5,7:9 )
#'
#'  i <- dmi_to_i(dmi, timesteps, bf)
#'  dmi2 <- i_to_dmi(i, timesteps, bf)
#'
#'  isTRUE(all.equal(dmi, dmi2))
dmi_to_i <- function(dmi, timestep, bf) {

  # Expand timestep out to have one element for each input dmi
  stopifnot(length(timestep) == 1 | length(timestep) == length(dmi))
  if(length(timestep) == 1)
    timestep <- rep(timestep, length(dmi))

  result <- numeric(length(dmi))
  dyn_mask <- get_dynamic_mask(bf)
  used_timesteps <- unique(timestep)

  for(i in seq_along(used_timesteps)) {
    this_ts <- used_timesteps[i]

    # static_index is a distribution like vector (n_active() elements)
    # with either the dynamic mask index of the associated cell or
    # NA for cells that aren't in the dynamic mask
    static_index <- rep(NA, n_active(bf))
    static_index[dyn_mask[, this_ts]] <- seq_len(sum(dyn_mask[ , this_ts]))

    sv <- timestep == this_ts
    result[sv] <- match(dmi[sv], static_index)
  }

  return(result)

}


i_to_dmi <- function(i, timestep, bf) {

  # Expand timestep out to have one element for each input i
  stopifnot(length(timestep) == 1 | length(timestep) == length(i))
  if(length(timestep) == 1)
    timestep <- rep(timestep, length(i))

  result <- numeric(length(i))
  dyn_mask <- get_dynamic_mask(bf)
  used_timesteps <- unique(timestep)

  for(j in seq_along(used_timesteps)) {
    this_ts <- used_timesteps[j]

    # static_index is a distribution like vector (n_active() elements)
    # with either the dynamic mask index of the associated cell or
    # NA for cells that aren't in the dynamic mask
    static_index <- rep(NA, n_active(bf))
    static_index[dyn_mask[, this_ts]] <- seq_len(sum(dyn_mask[ , this_ts]))

    sv <- timestep == this_ts
    result[sv] <- static_index[i[sv]]
  }
  return(result)
}


