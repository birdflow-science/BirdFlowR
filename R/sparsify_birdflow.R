

sparsify_birdflow <- function(bf, method = "zero_state", p){

  stopifnot(all(method %in% c("zero_state", "conditional")))
  validate_BirdFlow(bf)
  if(!has_marginals(bf))
    stop("Currently implemented sparsification only works with marginals")
  if(!has_distr(bf))
    stop("Distributions required to evaluate model")
  if(any(method == "pct_conditional") && missing(p))
    stop("p is required for conditional sparsification")

  #----------------------------------------------------------------------------#
  # Setup (common to all methods)
  #----------------------------------------------------------------------------#
  pre_sparsification_stats <- evaluate_perfomance(bf)
  verbose <- TRUE
  index <- bf$marginals$index
  index <- index[index$direction == "forward", ]
  marginal_names <- index$marginal
  stopifnot(all(marginal_names %in% names(bf$marginals)))
  if(any(duplicated(marginal_names)))
    stop("Shouldn't have duplicated marginals in index")

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
  if(any(method == "conditional")){
    if(verbose)
      cat("Starting conditional sparsification with p= ", p, "\n\t", sep = "")
    for(i in seq_along(marginal_names)){
      m_name <- marginal_names[i]
      mar <- bf$marginals[[m_name]]
      to_zero <- matrix(FALSE, nrow = nrow(mar), ncol = ncol(mar))

      # Calculate which cells need to be zeroed based on retaining p of each row
      row_thresholds <- apply(mar, 1, find_threshold, p = p)
      to_zero_r <- mapply(function(m, thresh) m < thresh, m = mar, thresh = row_thresholds)
      to_zero_r <- matrix(to_zero_r, nrow = nrow(mar), ncol = ncol(mar))

      # Calculate which cells need to be zeroed based on retaining p of each col
      col_thresholds <- apply(mar, 2, find_threshold, p = p)
      to_zero_c <- mapply(function(m, thresh) m < thresh, m = t(mar), thresh = col_thresholds )
      to_zero_c <- t(matrix(to_zero_c, nrow = nrow(mar), ncol = ncol(mar)))

      mar[to_zero_c | to_zero_r] <- 0
      bf$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)
      if(verbose) cat(".")
    }
    if(verbose) cat(" Done.\n")
  }

  #----------------------------------------------------------------------------#
  # zero_state sparsification   (has no parameters)
  #
  # forces zero in the marginals for rows and columns that correspond with zero
  # in the training distribution (states that are zero)
  #----------------------------------------------------------------------------#
  if(any(method == "zero_state")){
    if(!has_marginals(bf)){
      stop("zero_state sparsification requires marginals")
    }
    d <- get_distr(bf = bf)
    dz  <- d == 0 # distribution is zero

    if(verbose){
      p_zero <-  sum(d == 0)/prod(dim(d))
      cat("Starting zero state sparsification.\n\t",
          "Marginals that code joint probabilities involving a state that is ",
          "zero in\n\tthe ebirdst distribution will be forced to zero.\n\t",
          round( p_zero * 100, 2), "% of the distribution states are zero.\n\t",
          "about ", round( ( 1- (1 - p_zero)^2 ) * 100),
          "% of the marginals will be zeroed out.\n", sep = "")
      rm(p_zero)
    }

    for(i in seq_along(marginal_names)){
      m_name <- marginal_names[i]
      m <- bf$marginals[[m_name]]

      from  <-  index$from[index$marginal == m_name] # previous distribution index
      to <- index$to[index$marginal == m_name] # next distribution index

      m[dz[, from], ] <- 0 # zero out rows where previous distribution is zero
      m[ , dz[, to] ] <- 0 # zero out columns where next distribution is zero
      m <- m/sum(m)

      m <- Matrix(m, sparse = TRUE)
      bf$marginals[[m_name]] <- m

    }
  } # end zero_state

  #----------------------------------------------------------------------------#
  #  Save stats and sparsification metadata
  #----------------------------------------------------------------------------#


  # Save stats in BirdFlow object
  post_sparsification_stats <- evaluate_perfomance(bf)
  performance <- cbind(data.frame(model = c("full", "sparse")),
      rbind(as.data.frame(pre_sparsification_stats),
                          as.data.frame(post_sparsification_stats) ) )

  # Renormalize and calculate level of sparsification
  marginal_sums <- n_zero <- numeric(length(marginal_names))
  for(i in seq_along(marginal_names)){
    m <- bf$marginals[[marginal_names[i] ]]
    s <-
    marginal_sums[i] <- sum(m)
    n_zero[i] <- sum(m == 0)

  }
  pct_zero <- sum(n_zero) / (n_active(bf)^2 * length(marginal_names))* 100

  # Build list of used arguments (other than bf and method)
  args <- list()
  if(!missing(p))
    args <- c(args, list(p = p))

  if(length(args) == 0)
    args <- NA

  bf$metadata$is_sparse <- TRUE
  bf$metadata$sparse_stats <- list(performance = performance,
                                   pct_zero = pct_zero,
                                   method  = method,
                                   arguments = args)

  validate_BirdFlow(bf)  # still good?

  return(bf)
}
