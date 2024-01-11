# Don't flag S3 methods as having bad names when linting:
# nolint start: object_name_linter.
#' Function to validate a BirdFlow object
#'
#' Throw an error if a BirdFlow object is malformed or incomplete.
#'
#' [preprocess_species()] creates a BirdFlow object that lacks both marginals
#' and transitions and thus can't be used to make projections.
#' `validate_BirdFlow()` tags the absence of these with the type "incomplete".
#'  Any other missing or malformed components are tagged "error".
#'
#' Since marginals can be used to calculate both distributions and
#' transition matrices, a BirdFlow object can be complete if it has marginals;
#' or has both transitions and distributions. Having redundancy in these three
#' is not considered an error.
#'
#' Currently metadata and species information is not checked for completeness.
#'
#' Currently dead end transitions are permitted.  See [find_dead_ends()] for
#' checking for those.
#'
#' @param x A BirdFlow object.
#' @param error If TRUE throw an error if there are problems if FALSE return
#' any problems as a data.frame.
#' @param allow_incomplete If TRUE allow the BirdFlow object to be missing
#' both marginals and transitions (but not other components). This allows
#' checking the output of [preprocess_species()].
#'
#' @return  If `error = FALSE` the function returns
#'  a data.frame describing any errors with columns:
#' \describe{
#' \item{problem}{A character description of any problems.}
#' \item{type}{The problem type, either "error" or "incomplete."}
#' }
#' Otherwise, if there are no problems a similar data.frame with no rows is
#' returned invisibly.
#' @export
validate_BirdFlow <- function(x, error = TRUE, allow_incomplete = FALSE) {
  # problem types:
  #   error: BirdFlow object is malformed
  #   incomplete: BirdFlow object is missing core components necessary for
  #     predicting

  # Note: compare_list_item_names() currently doesn't pay any attention to
  #   the order of items.

  stopifnot(error %in% c(TRUE, FALSE),
            allow_incomplete %in% c(TRUE, FALSE))

  # Setup for tracking errors
  p <- data.frame(problem = character(0), type = character(0))
  add_prob <- function(problem, type, problems) {
    stopifnot(type %in% c("error", "incomplete"))
    return(rbind(problems, data.frame(problem = problem, type = type)))
  }

  report_problems <- function() {
    # This function uses p, error,  and allow_incomplete
    # from the parent function to either throw an error
    # OR return from the parent function.
    message <- NA
    if (error) {
      if (allow_incomplete) {
        if (any(p$type == "error"))
          message <- paste0("Problems found by validate_BirdFlow:\n\t",
                            paste(p$problem[p$type == "error"],
                                  collapse = ";\n\t"))
      } else { # Don't allow incomplete:
        if (nrow(p) > 0)
          message <- paste0("Problems found by validate_BirdFlow:\n\t",
                          paste(p$problem, collapse = "; \n\t"))
      }
    }
    if (!is.na(message))
      stop(message, call. =  FALSE)

    do.call("return", args = list(p), envir = parent.frame(n = 1))

  }


  # Check class
  if (!inherits(x, "BirdFlow")) {
    p <- add_prob("x is not a BirdFlow object", "error", p)
    report_problems()
  }

  ### Back compatibility code
  # Fix (allowed) typo in old models
  names(x$metadata)[names(x$metadata) == "birdFlowr_version"] <-
    "birdflowr_version"

  # Look for problems in nested list names, and items by comparing
  # to canonical BirdFlow object returned by new_BirdFlow
  diff <- compare_list_item_names(x, new_BirdFlow())
  problems <- paste(diff$where, diff$differences)

  # Remove differences that are not problems
  problems <- setdiff(
    problems,
    c("x$marginals should not be a list", # ok to have marginals
      "x$dates should not be a list", # ok to have dates
      "x$transitions should not be a list", # ok to have transitions
      "x extra:uci, lci",  # Ok to have these (included in preprocessing output)
      "x extra:lci, uci",  # ok in this order too
      "x missing:marginals", # ok to be missing marginals
      "x missing:marginals, distances",
      "x missing:distances",
      "x$metadata$sparse_stats should not be a list",  # Having them is fine
      "x$metadata$sparse should not be a list",  #
      "x$metadata missing:birdflow_version",
      "x$metadata extra:hyperparameters",
      "x$metadata extra:hyperparameters, loss_values"
    ))


  p <- rbind(p,
             data.frame(problem = problems,
                        type = rep("error", length(problems))))

  # These problems will prevent other checks from working at all
  catastrophic_problems <- c(
    "x$geom should be a list",
    "x missing:dates"
  )
  if (any(p$problem %in% catastrophic_problems))
    report_problems()

  # Check geom
  expected_names <- setdiff(names(new_BirdFlow()$geom), "dynamic_mask")
  if (!all(expected_names %in% names(x$geom))) {
    p <- add_prob(paste0("x$geom is missing: ",
                         paste(setdiff(expected_names, names(x$geom)),
                               collapse = ", ")), "error", p)
    report_problems()
  }




  # check dates
  if (!"dates" %in% names(x) || !is.data.frame(x$dates)) {
    p <- add_prob("x$dates is missing, NA or not a dataframe", "error", p)
    report_problems()
  } else { # dates exists and is data.frame

    if (x$metadata$ebird_version_year < 2022) {
      # 2021 ebirdst models have use older dates format
      required_cols <- c("interval", "date", "doy", "start", "midpoint", "end")
    } else {
      #2022_ ebirdst models use newer dates format
      required_cols <- names(make_dates())
    }
    if (!all(required_cols  %in% names(x$dates))) {
      p <- add_prob(paste0("x$dates is missing columns:",
                           paste(setdiff(required_cols, names(x$dates)))),
                    "error", p)
      report_problems()
    } # end if dates missing columns
    rm(required_cols)


    if ("distr" %in% names(x)) {
      if (is.null(dim(x$distr)) ||
          !length(dim(x$distr)) == 2 ||
          !is.numeric(x$distr)) {
        p <- add_prob("distr has wrong format", "error", p)
        report_problems()
      }

      if (nrow(x$dates) != ncol(x$distr)) {
        p <- add_prob(paste0("x$dates and x$distr do not represent the same ",
                             "number of timesteps."), "error", p)
        report_problems()
      }
    }
  } # end dates is data.frame



  # check consistancy of has_ (transitions, marginals, distr)
  components <- c("transitions", "marginals", "distr")
  for (i in seq_along(components)) {
    has <- x$metadata[[paste0("has_", components[i])]]
    if (!is.logical(has) || is.na(has) || length(has) != 1)
      p <- add_prob(paste0("has_", components[i], " is not TRUE or FALSE"),
                    "error", p)
  }

  if (has_distr(x)) {
    if (!is.matrix(x$distr))
      p <- add_prob("distr is not a matrix", "error", p)
    sums_to_one <- get_distr(x) |>
      apply(2, sum) |>
      sapply(function(x) isTRUE(all.equal(x, 1)))
    if(!all(sums_to_one)){
      p <- add_prob("not all distributions sum to one",
                    "error", p)
    }
  }
  if (has_dynamic_mask(x)) {
    if (!is.matrix(x$geom$dynamic_mask)){
      p <- add_prob("dynamic mask is not a matrix", "error", p)
    } else {
      if(!all(apply(get_dynamic_mask(x), 2, sum) > 0)){
        p <- add_prob("dynamic mask eliminates all cells for some timesteps", "error", p)
      }
    }
  }

  if (has_marginals(x)) {
    if (!is.list(x$marginals))
      p <- add_prob("marginals is not a list", "error", p)
  }
  if (has_transitions(x)) {
    if (!is.list(x$transitions))
      p <- add_prob("transitions is not a list", "error", p)
  }

  # Completeness
  if (!has_transitions(x) && !has_marginals(x))
    p <- add_prob("model has neither transitions nor marginals",
                  "incomplete", p)

  if (!has_distr(x) && !has_marginals(x))
    p <- add_prob("model has neither distr nor marginals", "incomplete", p)




  # Consistent in number of transitions
  nt <- x$metadata$n_transitions
  if (is.na(nt)) {
    if ((has_transitions(x)) || (has_marginals(x))) {
      p <- add_prob(
        "model has transitions or marginals but n_transitions is NA",
        "error", p)
    }
  } else {  # number of transitions is not NA

    if (!is.numeric(nt) || !length(nt) == 1 || is.na(nt) || is.infinite(nt) ||
        !isTRUE(all.equal(as.numeric(nt) %% 1, 0)) || !nt > 0) {
      p <- add_prob("metadata$n_transitions not a single postive integer value",
                    "error", p)
    }
    if (has_transitions(x)) {
      if (length(x$transitions) != nt * 2)
        p <- add_prob("number of transitions not equal to n_transitions()",
                      "error", p)
    }
    if (has_marginals(x)) {
      if (length(setdiff(names(x$marginals), "index")) != nt)
        p <- add_prob(
          "number of stored marginals is not equal to n_transitions()",
          "error", p)
    }
  }  # end consistency checks on n_transitions



  # consistency on n_active
  if (is.na(n_active(x))) {
    if (has_transitions(x) || has_marginals(x) || has_distr(x) ||
        !is.na(x$geom$mask)) {
      p <- add_prob(
        "n_active() is NA but x has elements that relate to n_active()",
        "error", p)
    }
  } else {  # n_active not NA
    if (has_distr(x)  && !dim(x$distr)[1] == n_active(x)) {
      p <- add_prob("distr is not consistent with n_active", "error", p)
    }

    if (has_dynamic_mask(x) && !dim(x$geom$dynamic_mask)[1] == n_active(x)) {
      p <- add_prob("dynamic mask is not consistent with n_active", "error", p)
    }

    if (has_marginals(x) && !has_dynamic_mask(x)) {
      # Check marginal dimensions if there's no dynamic mask
      items <- setdiff(names(x$marginals), "index")
      correct_dim <- rep(NA, length(items))
      for (i in seq_along(items)) {
        correct_dim[i] <- all(dim(x$marginals[[items[i]]]) == n_active(x))
      }
      if (!all(correct_dim)) {
        p <- add_prob("marginal dimensions inconsistent with n_active",
                      "error", p)
      }
    }

    # Consistent marginal dimensions
    if (has_marginals(x) && has_dynamic_mask(x)) {
      items <- setdiff(names(x$marginals), "index")
      # Index of just forward transitions (each marginal in forward order)
      f_index <- x$marginals$index[x$marginals$index$direction == "forward", ]
      if (length(items) != nrow(f_index) || !all(items == f_index$marginal)) {
        p <- add_prob("marginal names don't match index",
                      "error", p)
      }
      # dm_cells: number of cells in dynamic mask for each timestep
      dm_cells <- apply(get_dynamic_mask(x), 2, sum)
      dim_is_correct <- rep(NA, length(items))
      for (i in seq_along(items)) {
        index_row <- which(f_index$marginal == items[i])
        from <- f_index$from[index_row]  # from timestep
        to <- f_index$to[index_row]   # to timestep
        expected_dim <- dm_cells[c(from, to)]
        dim_is_correct[i] <- all(dim(x$marginals[[items[i]]]) == expected_dim)
      }
      if (!all(dim_is_correct)) {
        p <- add_prob("marginal dimensions inconsistent with dynamic_mask",
                      "error", p)
      }
    }

    # Consistent transition dimensions
    if (has_transitions(x) && !has_dynamic_mask(x)) {
      items <- names(x$transitions)
      correct_dim <- rep(NA, length(items))
      for (i in seq_along(items)) {
        correct_dim[i] <- all(dim(x$transitions[[items[i]]]) == n_active(x))
      }
      if (!all(correct_dim)) {
        p <- add_prob("transition dimensions inconsistent with n_active",
                      "error", p)
      }
    }

    if (has_transitions(x) && has_dynamic_mask(x)) {
      items <- names(x$transitions)
      index <- x$marginals$index
      if (!setequal(index$transition, items)) {
        p <- add_prob("transitions names do not match index$transitions", p)
      }
      # dm_cells: number of cells in dynamic mask for each timestep
      dm_cells <- apply(get_dynamic_mask(x), 2, sum)
      dim_is_correct <- rep(NA, length(items))
      for (i in seq_along(items)) {
        index_row <- which(index$transition == items[i])
        from <- index$from[index_row]  # from timestep
        to <- index$to[index_row]   # to timestep
        expected_dim <- dm_cells[c(from, to)]
        dim_is_correct[i] <- all(dim(x$transitions[[items[i]]]) == expected_dim)
      }
      if (!all(dim_is_correct)) {
        p <- add_prob(
          "transition matrix dimensions inconsistent with dynamic_mask",
          "error", p)
      }
    }
  } # end n_active consistency check

  # check geometry contents
  if (!is.matrix(x$geom$mask)) {
    p <- add_prob("x$geom$mask is not a matrix", "error", p)
  } else { # mask is matrix
    if (!is.logical(x$geom$mas))
      p <- add_prob("mask isn't logical", "error", p)
    if (is.na(n_active(x)) || !n_active(x) == sum(x$geom$mask))
      p <- add_prob("mask isn't consistent with n_active", "error", p)
    if (is.null(nrow(x))) {
      p <- add_prob("nrow(x) is NULL", "error", p)
    } else {
      if (is.na(nrow(x)) || ! nrow(x) == nrow(x$geom$mask))
        p <- add_prob("nrow(mask) not equal to nrow(x)", "error", p)
    }
    if (is.null(ncol(x))) {
      p <- add_prob("ncol(x) is NULL", "error", p)
    } else {
      if (is.na(ncol(x)) || !ncol(x) == ncol(x$geom$mask))
        p <- add_prob
    }
  }



  # check marginal names and index
  if ("marginals" %in% names(x) && is.list(x$marginals)) {
    mn <- setdiff(names(x$marginals), "index")
    index <- x$marginals$index
    if (!all(mn %in%  index$marginal))
      p <- add_prob("Not all marginals are indexed.", "error", p)
    if (!all(index$marginal %in% mn))
      p <- add_prob("Not all marginals in index exist", "error", p)

    # Check that all marginals sum to 1
    marginal_sums <- as.numeric(sapply(mn, function(m) sum(x$marginals[[m]])))
    sums_to_one <- sapply(marginal_sums,
                          function(s) isTRUE(all.equal(s, 1, tolerance = 1e-5)))
    if (!all(sums_to_one))
      p <- add_prob("Not all marginals have a sum of one.", "error", p)
  }

  return(report_problems())

} # end validation function
# nolint end
