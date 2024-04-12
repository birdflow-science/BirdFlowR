
skip_if_wrong_ebirdst_for_snapshot <- function() { # nolint: object_length_linter, line_length_linter
  v <- ebirdst_pkg_ver()
  if (v < "3.2022.0") {
  testthat::skip(paste0(
   "ebirdst version ", as.character(v),
      " produces dated snapshots."))

  } else {
    invisible()
  }
}
