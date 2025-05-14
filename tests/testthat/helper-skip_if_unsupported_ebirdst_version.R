
skip_if_unsupported_ebirdst_version <- function(use = "package") { # nolint: object_length_linter, line_length_linter
  supported <- ebirdst_ver_supported(use = use, throw_error = FALSE)

  v <- ebirdst_pkg_ver()
  if(!supported) {
    skip(message = paste0("ebirdst version:", v, " not supported by ", use))
  }
  invisible()
}
