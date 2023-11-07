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
#' @section Methods: There are four sparsification methods. The first "state"
#' does not use any parameters and eliminates model states entirely.
#'
#' The three remaining proportion based methods all use `p` to control the
#' amount of sparsification; where `p` is the target
#' proportion of the density to retain after eliminating all values below a
#' (calculated) threshold.
#'
#' In the different proprtional methods the thresholds are calculated and
#' applied either to the whole model (`model`)  or repeatedly to its
#' components (`conditional`, `marginal`).
#'
#' \describe{
#' \item{`state`}{State based sparsification eliminates states (in time and
#' space) that are zero in the training, ebirdst, distributions, from
#'  from the marginals.
#'
#'  For each marginal rows are zeroed out that correspond to zeroes in the
#'  preceding timestep's distribution and columns are zeroed out that correspond
#'  to zeroes in the following timestep's distribution.  Thus all the cells in
#'  the marginal that represent a joint probability involving a state that is
#'  zero in either of the distributions are zeroed out, essentially dropping
#'  those states from the model.
#'  }
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
#' \item{`conditional`}{ This method targets (1 - p) of both the forward and
#' backward conditional probabilities to be zeroed out.
#'
#' Conditional probability is calculating by dividing cells in each row
#' or column by that row or columns total.
#'
#' In this method thresholds are chosen independently for each row and each
#' column of a marginal prior to any zeroing and then the cells that fall
#' below either their row or column thresholds are set to zero.
#'
#' This likely results in each marginal and the model as a whole retaining
#' less than `p` of the density. Theoretically somewhere between p - 1 and
#' 2 * (p - 1) of the density will be lost. The higher value would be
#' achieved if the row and column methods don't overlap at all in the cells
#' they select; and the lower value if there's complete agreement. In practice
#' the dropped proportion will likely be near the middle of that range.
#' Ultimately for all the proportional methods `p` should be tuned based on the
#' performance of the sparsified model, so even though this method does not hit
#' the target precisely it will still work as a tuning parameter.
#' }
#' }
#'
#' It's possible to combine any of the proportional methods with the state based
#' method by passing two methods as a vector. If this is done the state based
#' sparsification is done second as it is not affected by the values in the
#' marginal.
#'
#' @param x A BirdFlow model.
#' @param method One of `"state"`, `"conditional"`, `"marginal"`, or `"model"`;
#'  or `"state"` paired with one of the other methods in a character
#'   vector (`c("state", "marginal")`) or a single string
#'   (`"state+conditional"`). See "Methods" section below for details.
#' @param p Required unless `method = "state"` to control the proportion of the
#'   probability density retained in the sparsification process. See "Methods"
#'   below.
#' @param fix If TRUE call [fix_dead_ends()] to eliminate dead ends
#'   in the sparse model. Defaults to TRUE, unless the method is "state" in
#'   which case it will be forced to FALSE as the state method does not
#'   create dead ends.
#' @return A BirdFlow object with many values in the marginals set to zero. The
#'   metadata will also be updated with sparsification statistics. The
#'   marginals will be standardized so that they sum to 1.
#' @importFrom stats cor
#' @export
#' @examples
#'
#' \dontrun{
#' # Full models are huge so we don't distribute them.
#' # Assuming you have an hdf5 file wit a full model you could run:
#' bf <- import_birdflow(hdf5_path)
#' sbf <- sparsify(bf, method = "marginal+state", p = 0.99)
#' }
sparsify <- function(x, method, p, fix = TRUE) {

  if (has_dynamic_mask(x))
    stop("sparsify() has not yet been updated to work with dynamic masks.")
  supported_methods <- c("model", "marginal", "conditional", "state")
  proportional_methods <- setdiff(supported_methods, "state")

  # allow for specifying multiple methods in single string
  # e.g. "conditional+state"
  if (grepl("+", method, fixed = TRUE))
    method <- strsplit(method, "+", fixed = TRUE)[[1]]

  if (!all(method %in% supported_methods)) {
    invalid <- setdiff(method, supported_methods)
    stop(paste(invalid, collapse = ", "),
         ifelse(length(invalid) == 1, " is not a valid mathod.",
                " are not valid methods."))
  }

  if (length(method) > 1 && sum(method %in% proportional_methods) > 1)
    stop("You can not combine proportional methods.")

  validate_BirdFlow(x)
  if (!has_marginals(x))
    stop("Currently implemented sparsification only works with marginals")
  if (!has_distr(x))
    stop("Distributions required to evaluate model")

  if (!setequal(method, "state")) {
    if (missing(p))
      stop("p is required for for all methods except 'state'")
    if (length(p) != 1 || !is.numeric(p) || ! p > 0 || ! p <= 1) {
      stop("p should be a single numeric greater than zero and less than or ",
           "equal to 1.")
    }
    if (missing(fix))
      fix <- TRUE
  } else {
    fix <- FALSE
  }

  #----------------------------------------------------------------------------#
  # Setup (common to all methods)
  #----------------------------------------------------------------------------#

  verbose <- birdflow_options("verbose")

  if (verbose)
    cat("Evaluating full model performance\n")
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
  #  zero out lowest values from rows and columns while trying to maintain a
  #  fixed proportion of the value in each row and column.
  #
  #  We independently calculate which cells to zero out based on rows and
  #  columns before any are zeroed out, and then zero out all the targeted
  #  cells.  Because cells are zeroed out for both rows and columns we will
  #  likely overshoot our target and end up with a greater pct zero.
  #----------------------------------------------------------------------------#
  if (any(method == "conditional")) {
    if (verbose)
      cat("Starting conditional sparsification with p= ", p, "\n\t", sep = "")
    for (i in seq_along(marginal_names)) {
      m_name <- marginal_names[i]
      mar <- x$marginals[[m_name]]
      to_zero <- matrix(FALSE, nrow = nrow(mar), ncol = ncol(mar))

      # Calculate which cells need to be zeroed based on retaining p of each row
      row_thresholds <- apply(mar, 1, find_threshold, p = p)
      for (j in seq_len(nrow(mar))) {
        sv <- mar[j, ] < row_thresholds[j]
        to_zero[j, sv] <- TRUE
      }

      # Calculate which cells need to be zeroed based on retaining p of each col
      col_thresholds <- apply(mar, 2, find_threshold, p = p)
      for (j in seq_len(ncol(mar))) {
        sv <- mar[, j] < col_thresholds[j]
        to_zero[sv, j] <- TRUE
      }

      mar[to_zero] <- 0

      x$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)
      if (verbose) cat(".")
    }
    if (verbose) cat(" Done.\n")
  }


  #----------------------------------------------------------------------------#
  # marginal
  #  p determines what proportion of each marginal's density to retain
  #----------------------------------------------------------------------------#
  if (any(method == "marginal")) {
    if (verbose)
      cat("Starting marginal sparsification with p= ", p, "\n\t", sep = "")
    for (i in seq_along(marginal_names)) {
      m_name <- marginal_names[i]
      mar <- x$marginals[[m_name]]
      threshold <- find_threshold(mar, p = p)
      mar[mar < threshold] <- 0
      x$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)
      if (verbose) cat(".")
    }
    if (verbose) cat(" Done.\n")
  }

  #----------------------------------------------------------------------------#
  # model
  #  p determines what proportion of the total model's density to retain
  #  (one threshold is used for all marginals)
  #----------------------------------------------------------------------------#
  if (any(method == "model")) {
    if (verbose)
      cat("Starting model sparsification with p= ", p, "\n\t", sep = "")

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
      if (verbose) cat(".")
    }
    if (verbose) cat(" Done.\n")
  } # Done model sparsification

  #----------------------------------------------------------------------------#
  # state sparsification   (has no parameters)
  #
  # Eliminate states that are zero in the training data from the marginals
  #
  # For each marginal set rows to zero that correspond with zero in the
  # preceding timestep's distribution and set columns to zero that correspond
  # with zero in the following timestep's distribution. Thus there will be no
  # transitions into or out of the locations (in time and space) that are
  # zero in the the training (ebirdst) distributions.
  #----------------------------------------------------------------------------#
  if (any(method == "state")) {
    d <- get_distr(x)
    d_is_zero  <- d == 0

    if (verbose) {
      cat("Starting state based sparsification.\n", sep = "")
    }
    for (i in seq_along(marginal_names)) {
      m_name <- marginal_names[i]
      mar <- x$marginals[[m_name]]

      from  <-  index$from[index$marginal == m_name] # previous distr. index
      to <- index$to[index$marginal == m_name] # next distribution index
      mar[d_is_zero[, from], ] <- 0 # zero out rows when previous distr. is zero
      mar[, d_is_zero[, to]] <- 0 # zero out cols when next distr. is zero

      x$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)

    } # end marginal loop
  } # end state based sparsification

  #----------------------------------------------------------------------------#
  #
  #  save post sparsification statistics
  #
  #----------------------------------------------------------------------------#

  # I need to standarize (so marginals sum to 1) prior to calculating stats
  # but I'm also keeping the non-standaridzed model as if
  # we also fix dead ends we want to calculate the density lost based
  # on the non-standardized object.

  if (verbose)
    cat("Evaluating post-sparsification performance\n")

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
    if (verbose)
      cat("Fixing dead ends\n")

    x <- fix_dead_ends(x)

    if (verbose)
      cat("Evaluating post-fix performance\n")

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

  if (verbose)
    cat("\t", format(pct_zero, digits = 3), "% zero\n\t",
        format(pct_density_lost, digits = 3), "% density lost\n\t",
        "traverse correlation:\n\t\t",
        "full: ", format(stats$traverse_cor[1], digits = 3), "\n\t\t",
        "sparse: ", format(stats$traverse_cor[2], digits = 3), "\n",
        ifelse(fix,
               paste0("\t\t", "fix: ",
                      format(stats$traverse_cor[3], digits = 3), "\n"),
               ""),
        sep = "")

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
