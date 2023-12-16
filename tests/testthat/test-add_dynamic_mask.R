test_that("Add dynamic mask works", {

          bf <- BirdFlowModels::amewoo
          expect_no_error(bf <- add_dynamic_mask(bf))
          expect_no_error(validate_BirdFlow(bf))
          expect_no_error(dbf <- add_dynamic_mask(bf, dummy_mask = TRUE))
          expect_no_error(validate_BirdFlow(dbf))

})
