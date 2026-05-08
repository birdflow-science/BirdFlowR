#' Extract distributions from BirdFlow models
#'
#' `get_distr` will return one or more distributions in their flattened form. A
#' single distribution will be returned as a vector; if multiple they will be
#' columns in a matrix.
#'
#' @section Distribution types:
#'
#' The `type` argument controls how the distribution is computed:
#'
#' \describe{
#'   \item{`"normalized"`}{ The default. Returns the eBird-derived stored
#'     distributions, normalized so each timestep sums to 1. Equivalent to
#'     the previous behavior with `from_marginals = FALSE`.}
#'   \item{`"marginal"`}{ Calculates the distribution from the marginals
#'     instead of the stored distributions. Useful for diagnostics; the
#'     two are very similar in practice. Equivalent to the previous
#'     behavior with `from_marginals = TRUE`.}
#'   \item{`"raw"`}{ Returns the eBird abundance values prior to the
#'     standardize-to-1 normalization, by multiplying the stored normalized
#'     distribution by the per-timestep totals captured during
#'     [preprocess_species()] (`x$metadata$abundance$totals`). This requires
#'     a model that recorded those totals — older models will trigger an
#'     error pointing at re-preprocessing.
#'     Note that quantile trimming via the `trim_quantile` argument is also
#'     a lossy step: `"raw"` recovers values from before normalization but
#'     after any trimming.}
#' }
#'
#' @seealso Distributions can be passed to [predict()][predict.BirdFlow] or
#'   converted to rasters with [expand_distr()] or converted to
#'   [SpatRaster][terra::rast] with
#'   [rasterize_distr()].  [sample_distr()] will convert one cell to 1 and the
#'   rest to 0 probabilistically based on the densities in the distribution.
#' @param x A BirdFlow model
#' @param which Indicates which timesteps to return. Can be one or more
#'   integers indicating timesteps; character dates in the format
#'   year-month-day e.g. `"2019-02-25"`; [`Date`][base::Dates] objects;
#'   or `"all"` which will return distributions for all timesteps.
#' @param type One of `"normalized"` (the default), `"marginal"`, or `"raw"`.
#'   See "Distribution types" for details.
#' @param from_marginals Deprecated. Use `type = "marginal"` instead.
#'   When supplied, `from_marginals = TRUE` is translated to
#'   `type = "marginal"` and `from_marginals = FALSE` to
#'   `type = "normalized"`, with a warning.
#' @return Either a vector with a distribution for a single timestep or a matrix
#'   with a column for each distribution.
#' @export
#'
get_distr <- function(x, which = "all",
                      type = c("normalized", "marginal", "raw"),
                      from_marginals) {

  # Soft-deprecate from_marginals: translate to type and warn.
  if (!missing(from_marginals)) {
    warning("`from_marginals` is deprecated. ",
            "Please use `type = \"marginal\"` instead.")
    type <- if (isTRUE(from_marginals)) "marginal" else "normalized"
  } else {
    type <- match.arg(type)
  }

  # Resolve which into integer timesteps
  which <- lookup_timestep(which, x)

  use_marginals <- type == "marginal"

  if (x$metadata$has_distr && !use_marginals) {
    # Return stored distribution

    d <- x$distr[, which]

    if (type == "raw") {
      totals <- x$metadata$abundance$totals
      if (is.null(totals) || (length(totals) == 1 && is.na(totals))) {
        stop("type = \"raw\" requires that ",
             "`x$metadata$abundance$totals` be populated, which is only ",
             "true for models preprocessed with BirdFlowR >= 0.1.0.9081. ",
             "Re-run preprocess_species() to enable raw recovery.")
      }
      if (any(which > length(totals))) {
        stop("type = \"raw\" cannot be returned for timesteps beyond the ",
             "length of `x$metadata$abundance$totals`.")
      }
      if (length(which) == 1) {
        d <- d * totals[which]
      } else {
        d <- d * matrix(rep(totals[which], each = nrow(d)), nrow = nrow(d))
      }
    }

    if (length(which) == 1) {
      attr(d, "time") <- paste0("t", which)
    }
    return(reformat_distr_labels(d, x))

  } else {
    # Or calculate from marginals
    if (!x$metadata$has_marginals) {
      if (use_marginals) {
        stop("The BirdFlow model has no marginals to ",
             "calculate distribution from.")
      } else {
        stop("No distributions available in the BirdFlow object.")
      }
    }

    if (type == "raw") {
      stop("type = \"raw\" is only available when stored distributions ",
           "are present (has_distr = TRUE).")
    }

    d <- vector(mode = "list", length = length(which))
    if (has_dynamic_mask(x)) {
      for (i in seq_along(which)) {
      id <- which[i]  # Distribution id

      if (id == 1) {  # use marginal after the distribution
        m <- x$marginals[[id]]
        dmd <- Matrix::rowSums(m)
      } else { # use marginal before the distribution
        # (not available for first marginal)
        # marginal column sums are the distribution after that marginal
        m <- x$marginals[[id - 1]] # Prior marginal
        dmd <- Matrix::colSums(m)  # distribution for state after marginal
      }

      # dm_d = dynamically masked distribution
      # need to expand to unmasked distribution (um_d)
      umd <- rep(0, n_active(x))
      umd[get_dynamic_mask(x, id)] <- dmd
      d[[i]] <- umd
      }
    } else {  # no dynamic mask

      ### BACK COMPATABILITY CODE
      for (i in seq_along(which)) {
        id <- which[i]  # Distribution id
        if (id == 1) {  # use marginal after the distribution
          m <- x$marginals[[id]]
          d[[i]] <- Matrix::rowSums(m)
        } else { # use marginal before the distribution
          # (not available for first marginal)
          # marginal column sums are the distribution after that marginal
          m <- x$marginals[[id - 1]] # Prior marginal
          d[[i]] <- Matrix::colSums(m)  # distribution for state after marginal
        }
      }  # end no dynamic mask
    } # end from marginals

    # Return single distribution as vector
    if (length(which) == 1) {
      d <- d[[1]] # reformat as vector
      attr(d, "time") <- paste0("t", which)
      return(reformat_distr_labels(d, x))
    }

    # Return multiple distributions as matrix
    d <- do.call(cbind, args = d)
    dimnames(d) <- list(i = NULL, time = paste0("t", which))
    return(reformat_distr_labels(d, x))
  }
}
