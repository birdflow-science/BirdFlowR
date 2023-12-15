#' sparsify BirdFlow models
#'
#' Set low values to zero in a BirdFlow model marginals to reduce object size.
#'
#' The BirdFlow model fitting algorithm cannot predict a complete zero, however
#' many of the marginal values are very close to zero and have little
#' impact on the model predictions. `sparsify()` forces small values to
#' zero with the goal of saving memory, reducing file size, and decreasing run
#' time. Marginals are stored as sparse matrices
#' ([Matrix::Matrix(x , sparse = TRUE)][Matrix::Matrix()] ) so only non-zero
#' values consume memory.
#'
#' @section Methods:  There are three sparsification methods that are all
#' based on proportion. They use `p` to control the
#' amount of sparsification; where `p` is the target
#' proportion of the density to retain after eliminating all values below a
#' (calculated) threshold.
#'
#' The thresholds are calculated and applied either to the whole model
#' (`model`)  or repeatedly to its components (`conditional`, `marginal`).
#'
#' \describe{
#'
#' \item{`model`}{ In model sparsification the values from all marginals are
#' pooled and then a threshold is chosen for that entire model such that zeroing
#' values below the threshold results in the target proportion, `p`, of the
#' model's density remaining.}
#'
#' \item{`marginal`}{A threshold is chosen and applied separately to each
#' marginal in the model.  Ultimately, `p` is achieved for the model as a whole
#' but the threshold below which cells are set to zero varies across marginals.
#' }
#'
#' \item{`conditional`}{ This method targets (`1 - p`) of both the forward and
#' backward **conditional probabilities** to be zeroed out but also guarantees
#' that at least `p_protected` proportion of the **cells** in each row
#' and column will not be zeroed out.
#'
#' In this method thresholds are chosen independently for each row and each
#' column of a marginal prior to any zeroing and then the cells that fall
#' below either their row or column thresholds are set to zero as long as they
#' aren't within the  `p_protected` proportion of cells (highest value cells)
#' that are protected from zeroing based on either their row or column.
#' `p_protected` thus prevents the number of transitions in the sparse model
#' from any state falling below the given proportion of the
#' transitions in the full model.  The default value means that for every
#' location at every timestep at least 10% of the transitions to locations in
#' the next timestep are retained.
#'
#' This method does not hit its target `p` density retained. In theory twice
#' as much density as `p` implies could could be cut from the model if the
#' cells targeted based on row and column do not overlap, or much more than
#' `p` could be retained with high values of `p_protected`}
#' }
#'
#' @param x A BirdFlow model.
#' @param method One of ``"conditional"`, `"marginal"`, or `"model"`.
#'   See "Methods" section below for details.
#' @param p Required to control the proportion of the
#'   probability density retained in the sparsification process. See "Methods"
#'   below.
#' @param fix If TRUE call [fix_dead_ends()] to eliminate dead ends
#'   in the sparse model, but only honored if the method produces dead ends.
#' @param p_protected Only used with `"conditional` method.
#' The proportion of **cells** in each row and column that are protected from
#' being zeroed out.  Any value of `p_protected` above 0 protects all the
#' non-dynamically-masked (NDM) locations from being dropped from the model.
#' The default value of `0.10` means that from any NDM location there will
#' always be transitions retained to at least 10% of the next timestep's
#' NDM locations.
#' @return A BirdFlow object with some values in the marginals set to zero. The
#'   metadata will also be updated with sparsification statistics. The
#'   marginals will be standardized so that they sum to 1.
#' @importFrom stats cor
#' @export
#' @examples
#'
#' \dontrun{
#' # Full models are huge so we don't distribute them.
#' # Assuming you have an hdf5 file with a full model you could run:
#' bf <- import_birdflow(hdf5_path)
#' sbf <- sparsify(bf, method = "marginal+state", p = 0.99)
#' }
sparsify <- function(x, method, p = 0.99, fix = TRUE, p_protected = .10) {

  supported_methods <- c("model", "marginal", "conditional")

  # allow for specifying multiple methods in single string
  # e.g. "conditional+state"

  if (!all(method %in% supported_methods)) {
    invalid <- setdiff(method, supported_methods)
    stop(paste(invalid, collapse = ", "),
         ifelse(length(invalid) == 1, " is not a valid mathod.",
                " are not valid methods."))
  }

  if (length(method) > 1)
    stop("Only one method may be used at a time")

  validate_BirdFlow(x)

  if (!has_marginals(x))
    stop("Currently implemented sparsification only works with marginals")
  if (!has_distr(x))
    stop("Distributions required to evaluate model")


  if (missing(p))
    stop("p is required for for all methods")

  if (length(p) != 1 || !is.numeric(p) || ! p > 0 || ! p <= 1)
    stop("p should be a single numeric greater than zero and less than or ",
         "equal to 1.")


  #----------------------------------------------------------------------------#
  # Setup (common to all methods)
  #----------------------------------------------------------------------------#

  bf_msg("Evaluating full model performance\n")
  pre_sparsification_stats <- distribution_performance(x)
  index <- x$marginals$index
  index <- index[index$direction == "forward", ]
  marginal_names <- index$marginal
  stopifnot(all(marginal_names %in% names(x$marginals)))
  if (any(duplicated(marginal_names)))
    stop("Shouldn't have duplicated marginals in index")

  #----------------------------------------------------------------------------#
  #
  # Define methods
  #
  #  Note: Marginal standardization (so they sum to 1) should not be done in
  #   the method's section as unstandardized matrices are needed to calculate
  #   the stats at the end, and they will be standardized there.
  #
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # conditional
  #  p determines what proportion of each row and column to retain
  #
  #  1. Identify cells that are protected from zeroing out based on
  # `p_protected`. This is the union of the cells identified based on
  #  rows and based on columns.
  #
  #  2. Then detrmine which cells to consider for zeroing out based on row and
  #  column specific thresholds that would eliminate a fixed proportion (p)
  #  of the the value in the row or column. This is done independently by row
  #  and column so will likely target more than p when the two are combined.
  #
  #  3. Set to zero the cells identified in 2 as candidates as long as they
  #  weren't identified in (1) as protected.
  #----------------------------------------------------------------------------#
  if (any(method == "conditional")) {
    bf_msg("Starting conditional sparsification with p= ", p, "\n\t", sep = "")
    for (i in seq_along(marginal_names)) {
      m_name <- marginal_names[i]
      mar <- x$marginals[[m_name]]
      protected <- to_zero <- matrix(FALSE, nrow = nrow(mar), ncol = ncol(mar))

      # Calculate which cells need to be zeroed based on retaining p of each row
      zero_thresholds <- apply(mar, 1, find_threshold, p = p)
      protected_thresholds <- apply(mar, 1, find_threshold,
                               p = p_protected, method = "values")
      for (j in seq_len(nrow(mar))) {
        sv <- mar[j, ] < zero_thresholds[j]
        to_zero[j, sv] <- TRUE
        sv <- mar[j, ] >= protected_thresholds[j]
        protected[j, sv] <- TRUE
      }

      # Calculate which cells need to be zeroed based on retaining p of each col
      zero_thresholds <- apply(mar, 2, find_threshold, p = p)
      protected_thresholds <- apply(mar, 2, find_threshold,
                               p = p_protected, method = "values")

      for (j in seq_len(ncol(mar))) {
        sv <- mar[, j] < zero_thresholds[j]
        to_zero[sv, j] <- TRUE
        sv <- mar[, j] >= protected_thresholds[j]
        protected[sv, j] <- TRUE
      }

      to_zero[protected] <- FALSE
      mar[to_zero] <- 0

      x$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)
      bf_msg(".")
    }
    bf_msg(" Done.\n")
  }


  #----------------------------------------------------------------------------#
  # marginal
  #  p determines what proportion of each marginal's density to retain
  #----------------------------------------------------------------------------#
  if (any(method == "marginal")) {
    bf_msg("Starting marginal sparsification with p= ", p, "\n\t")
    for (i in seq_along(marginal_names)) {
      m_name <- marginal_names[i]
      mar <- x$marginals[[m_name]]
      threshold <- find_threshold(mar, p = p)
      mar[mar < threshold] <- 0
      x$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)
      bf_msg(cat("."))
    }
    bf_msg(" Done.\n")
  }

  #----------------------------------------------------------------------------#
  # model
  #  p determines what proportion of the total model's density to retain
  #  (one threshold is used for all marginals)
  #----------------------------------------------------------------------------#
  if (any(method == "model")) {
    bf_msg("Starting model sparsification with p= ", p, "\n\t")

    # Aggregate all the values from all the marginals into one matrix
    # with 1 column per marginal and use it to determine the threshold

    sample_freq <- 2 # set to positive integer. 1/sample.freq = sample rate
    # a value of one means sample every value
    all <- 1:n_active(x)^2
    sv <- all[all %% sample_freq == 0]

    values <- matrix(NA_real_, nrow = length(sv),
                     ncol =  length(marginal_names))

    if (sample_freq == 1) { # more efficient version if using all values
      for (i in seq_along(marginal_names)) {
        m_name <- marginal_names[i]
        values[, i] <- as.vector(x$marginals[[m_name]])
      }
    } else {
      for (i in seq_along(marginal_names)) {
        m_name <- marginal_names[i]
        values[, i] <- as.vector(x$marginals[[m_name]])[sv]
      }
    }
    threshold <- find_threshold(values, p = p)
    rm(values)
    gc(verbose = FALSE)

    for (i in seq_along(marginal_names)) {
      m_name <- marginal_names[i]
      mar <- x$marginals[[m_name]]
      mar[mar < threshold] <- 0
      x$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)
      bf_msg(".")
    }
    bf_msg(" Done.\n")
  } # Done model sparsification

  #----------------------------------------------------------------------------#
  #
  #  save post sparsification statistics
  #
  #----------------------------------------------------------------------------#

  # I need to standarize (so marginals sum to 1) prior to calculating stats
  # but I'm also keeping the non-standaridzed model as if
  # we also fix dead ends we want to calculate the density lost based
  # on the non-standardized object.

  bf_msg("Evaluating post-sparsification performance\n")

  # Make standardized version
  standardized_bf <- x
  for (i in seq_along(marginal_names)) {
    mar <- standardized_bf$marginals[[marginal_names[i]]]
    s <- sum(mar)
    mar <- Matrix(mar / s, sparse = TRUE)  # re-normalize
    standardized_bf$marginals[[marginal_names[i]]] <- mar
  }
  post_sparsification_stats <- distribution_performance(standardized_bf)

  # Calculate density lost and and percent zero on the non-standardized version
  ms <- marginal_stats(x)
  pct_zero <- ms$pct_zero
  pct_density_lost <- (n_transitions(x) - ms$sum) / n_transitions(x) * 100

  # Assemble all the stats in one table
  stats <- cbind(data.frame(model = c("full", "sparse"),
                            pct_zero = c(0, ms$pct_zero),
                            pct_density_lost = c(0, pct_density_lost)),
                 rbind(as.data.frame(pre_sparsification_stats),
                       as.data.frame(post_sparsification_stats)))

  if (fix) {
    bf_msg("Fixing dead ends\n")

    x <- fix_dead_ends(x)

    bf_msg("Evaluating post-fix performance\n")

    ms <- marginal_stats(x)
    pct_zero <- ms$pct_zero
    pct_density_lost <- (n_transitions(x) - ms$sum) / n_transitions(x) * 100

    # Update standardized version
    standardized_bf <- x
    for (i in seq_along(marginal_names)) {
      mar <- standardized_bf$marginals[[marginal_names[i]]]
      s <- sum(mar)
      mar <- Matrix(mar / s, sparse = TRUE)  # re-normalize
      standardized_bf$marginals[[marginal_names[i]]] <- mar
    }

    post_fix_stats <- distribution_performance(standardized_bf)

    # Add row to stats with the stats on the fixed version
    stats <- rbind(stats,
                   cbind(data.frame(model = "fixed",
                                    pct_zero = pct_zero,
                                    pct_density_lost = pct_density_lost),
                         as.data.frame(post_fix_stats)))
  }

  # Update object to latest standardized version
  x <- standardized_bf

  bf_msg("\t", format(pct_zero, digits = 3), "% zero\n\t",
        format(pct_density_lost, digits = 3), "% density lost\n\t",
        "traverse correlation:\n\t\t",
        "full: ", format(stats$st_traverse_cor[1], digits = 3), "\n\t\t",
        "sparse: ", format(stats$st_traverse_cor[2], digits = 3), "\n",
        ifelse(fix,
               paste0("\t\t", "fix: ",
                      format(stats$st_traverse_cor[3], digits = 3), "\n"),
               ""))

  # Build list of used arguments (other than x and method)
  args <- list()
  if (!missing(p))
    args <- c(args, list(p = p))

  if (length(args) == 0)
    args <- NA

  x$metadata$is_sparse <- TRUE
  if (is.na(x$metadata$sparse))
    x$metadata$sparse <- list()
  x$metadata$sparse$method  <- method
  x$metadata$sparse$arguments <-  args
  x$metadata$sparse$stats <- stats
  x$metadata$sparse$pct_zero <- pct_zero
  x$metadata$sparse$pct_density_lost <- pct_density_lost

  validate_BirdFlow(x)  # still good?

  return(x)
}
