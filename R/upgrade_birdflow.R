#' Upgrade an older BirdFlow object to the current in-memory format
#'
#' Consolidates the back compatibility fixes that were previously scattered
#' across several functions (`predict()`, `route()`, `route_between()`,
#' `predict_between()`, `truncate_birdflow()`, and others) into one place.
#' Running `upgrade_birdflow()` on an already current object is a no-op.
#'
#' `upgrade_birdflow()`:
#' 1. Fixes the old `birdFlowr_version` metadata name typo.
#' 2. Backfills `metadata$timestep_padding` via [get_timestep_padding()] if
#'    missing.
#' 3. Backfills metadata fields that are missing entirely (added in later
#'    versions of the package) with [new_BirdFlow()]'s schema defaults,
#'    without overwriting any field that's already present.
#' 4. Backfills `dates$week` with `1:52` if missing and the model has 52
#'    timesteps.
#' 5. Adds a dynamic mask via [add_dynamic_mask()] if the object doesn't
#'    already have one.
#'
#' @param bf A BirdFlow object.
#' @return The upgraded BirdFlow object.
#' @export
#' @seealso [add_dynamic_mask()], [validate_BirdFlow()]
#' @examples
#' \donttest{
#' bf <- upgrade_birdflow(BirdFlowModels::amewoo)
#' }
upgrade_birdflow <- function(bf) {

  # Fix (allowed) typo in old models
  names(bf$metadata)[names(bf$metadata) == "birdFlowr_version"] <-
    "birdflowr_version"

  # Add timestep_padding metadata if it doesn't exist.
  # Do this before the general metadata backfill below, since
  # get_timestep_padding() distinguishes "not yet computed" (NULL) from any
  # other value, and the general backfill would otherwise mask that by
  # filling the field with the schema's NA_integer_ default.
  if (is.null(bf$metadata$timestep_padding)) {
    bf$metadata$timestep_padding <- get_timestep_padding(bf)
  }

  # Backfill metadata fields added since bf was created, without
  # overwriting fields that are already present.
  defaults <- new_BirdFlow()$metadata
  missing_fields <- setdiff(names(defaults), names(bf$metadata))
  bf$metadata[missing_fields] <- defaults[missing_fields]

  # Add dates$week if it doesn't exist
  if (is.null(bf$dates$week) && nrow(bf$dates) == 52) {
    bf$dates$week <- 1:52
  }

  # Add dynamic mask if missing
  if (!has_dynamic_mask(bf)) {
    bf <- add_dynamic_mask(bf)
  }

  return(bf)
}
