
skip_if_unsupported_ebirdst_version <- function() {
  v <- packageVersion("ebirdst")

  if (v > as.package_version("2.2021.3")) {
    skip(paste0(
      "ebirdst version ", as.character(v),
         " not supported for preprocessing."))
  } else {
    invisible()
  }
}
