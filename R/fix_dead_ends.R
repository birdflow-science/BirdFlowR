


fix_dead_ends <- function(bf, max_attempts = 100){

  verbose = TRUE
  fix_stats <- data.frame(step =0:max_attempts, pct_lost = NA_real_,
                          n_dead_ends = NA_integer_ )

  if(bf$metadata$is_sparse){
    # If this is called independently (and after) sparsify() and thus after
    # the marginals have been re-standardized than we
    # might want to calculate a pseudo initial sum such that the percent lost
    # Is relative to the original sum and not the restandardized sum.
    p_density_lost <- bf$metadata$sparse$pct_density_lost / 100
    initial_sum <- n_transitions(bf) / (1 - p_density_lost)
    stopifnot(isTRUE(all.equal( (initial_sum - n_transitions(bf)) / initial_sum, p_density_lost)))
  } else {
    # If called from within sparsify assume marginals haven't been
    # re-standardized yet. Since the marginals originally summed to one the
    # initial sum is just the number of marginals.
    initial_sum <- n_transitions(bf)
  }


  for(i in 1:(max_attempts+1) ){

    de <- find_dead_ends(bf)
    msum <- sum_marginals(bf)
    fix_stats$pct_lost[i] = (initial_sum - msum) / initial_sum * 100
    fix_stats$n_dead_ends[i] <- nrow(de)
    if(verbose)
      cat("Step ", i - 1, " of ", max_attempts," ",
          nrow(de), " dead ends ",
          fix_stats$pct_lost[i], "% density lost\n"
          )
    if(nrow(de) == 0){
      # If we've fixed it
      fix_stats <- fix_stats[1:i, ]
      break
    }
    bf <- fix_current_dead_ends(bf, de)
  }
  if(!is.list(bf$metadata$sparse) && is.na(bf$metadata$sparse))
    bf$metadata$sparse <- list()
  bf$metadata$sparse$fix_stats <- fix_stats

  return(bf)
}




fix_current_dead_ends <- function(bf, de){
  if(missing(de))
    de <- find_dead_ends(bf)
  mar_names <- sort(unique(de$mar))
  for(i in seq_along(mar_names)){
    mn <- mar_names[i]   # marginal name
    mar <- bf$marginals[[mn]]  # this mariginal (the matrix)
    mde <- de[de$mar == mn, , drop = FALSE]  # mde = this marginals dead ends
    fi <- mde$i[mde$direction == "forward"]  # index of forwared dead ends
    bi <- mde$i[mde$direction == "backward"]
    if(length(fi) > 0)
      mar[ , fi] <- 0
    if(length(bi) > 0)
      mar[bi, ] <- 0
    bf$marginals[[mn]] <- mar
  }
  return(bf)
}
