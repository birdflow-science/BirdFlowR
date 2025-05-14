
skip_if_wrong_ebirdst_for_snapshot <- function() { # nolint: object_length_linter, line_length_linter
  v <- ebirdst_pkg_ver()
  if (v < "3.2022.0") {
    skip(paste0(
      "ebirdst version ", as.character(v),
      " produces dated snapshots."))
  }


  if (v >= "3.2023.0") {
    skip(paste0(
      "Birdflow snapshots are not yet updated for ebirdst version ",
      as.character(v)))
  }

    invisible()
}
