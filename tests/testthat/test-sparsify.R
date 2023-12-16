test_that("sparsification works", {

  # Temporarily suppress BirdFlowR chatter
  o_verbose <- birdflow_options("verbose")
  birdflow_options(verbose = FALSE)
  on.exit(birdflow_options(verbose = o_verbose))

  p_protected <- .1
  p <- 0.99

  bf <- BirdFlowModels::amewoo

  # Truncation is nonsensical here from a scientific perspective
  # but is done to speed testing
  bf <- truncate_birdflow(bf, start = 6, end = 8)

  # Test marginal
  expect_no_error(marginal <- sparsify(bf, method = "marginal", p = p))
  set.seed(1)
  expect_no_error(rts <- route(marginal, 10, start = 1,
                               end = n_timesteps(marginal)))

  # Test conditional

  expect_no_error(conditional <- sparsify(bf, method = "conditional", p = p,
                                    p_protected = p_protected))
  set.seed(1)
  expect_no_error(rts <- route(conditional, 10, start = 1,
                               end = n_timesteps(conditional)))
  s <- get_metadata(conditional, "sparse")

  # Expect no lost states under conditional with p_protected
  expect_true(all(s$stats$n_states == s$stats$n_states[1]))

  # Expect p_protected transitions to be retained
  # (only checking one dim of first marginal)
  m <- conditional$marginals[[1]]
  p_retained <- apply(m, 1, function(x) sum(x != 0) / length(x))
  expect_true(all(p_retained > p_protected))

  # Model
  expect_no_error(model <- sparsify(bf, "model", p = p))

  # Compare performance
  comparison <- rbind(conditional$metadata$sparse$stats[c(1, 3), ],
        marginal$metadata$sparse$stats[3, , drop = FALSE],
        model$metadata$sparse$stats[3, , drop = FALSE])
  comparison$model[2:4] <- c("conditional", "marginal", "model")
  comparison$md_traverse_cor <- NULL
  comparison$min_step_cor <- NULL
  comparison$min_distr_cor <- NULL
  rownames(comparison) <- comparison$model
  comparison$model <- NULL
  comparison <- t(comparison)
  comparison <- round(comparison, 3)

  expect_snapshot(comparison)


})
