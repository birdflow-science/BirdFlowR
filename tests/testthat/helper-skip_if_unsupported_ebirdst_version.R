
skip_if_unsupported_ebirdst_version <- function() { # nolint: object_length_linter, line_length_linter
  v <- packageVersion("ebirdst")

  if (v >= as.package_version("4.2023.0")) {

    #skip(paste0(
    #  "ebirdst version ", as.character(v),
    #     " not supported for preprocessing."))
    warning("ebirdst ", v, "released. BirdFlowR may need to be updated.")

  } else {
    invisible()
  }
}
