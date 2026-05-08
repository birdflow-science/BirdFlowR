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

test_that("sparsify() uses default p = 0.99 when p is not supplied", {
  o_verbose <- birdflow_options("verbose")
  birdflow_options(verbose = FALSE)
  on.exit(birdflow_options(verbose = o_verbose))

  bf <- truncate_birdflow(BirdFlowModels::amewoo, start = 6, end = 8)

  expect_no_error(default <- sparsify(bf, method = "marginal"))
  explicit <- sparsify(bf, method = "marginal", p = 0.99)

  # Same nonzero structure for both — confirms the default is 0.99,
  # not some other value silently substituted by R's missing()-handling.
  expect_identical(
    lapply(default$marginals[grep("^M_", names(default$marginals))],
           function(m) m != 0),
    lapply(explicit$marginals[grep("^M_", names(explicit$marginals))],
           function(m) m != 0)
  )
})

test_that("sparsify() rejects out-of-range p", {
  bf <- BirdFlowModels::amewoo
  expect_error(sparsify(bf, method = "marginal", p = 0),
               "p should be a single numeric")
  expect_error(sparsify(bf, method = "marginal", p = 1.5),
               "p should be a single numeric")
})
