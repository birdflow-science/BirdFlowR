#' Suppress warnings that match one or more regular expressions
#'
#' `suppress_specific_warnings()` will suppress warnings that match regular
#' expression patterns that are supplied via
#' the `patterns` argument, without suppressing warnings that don't match the
#' patterns.
#'
#' @keywords internal
#' @param x An expression.
#' @param patterns One or more patterns to check warning messages against.
#'
#' @return Possibly output from `x`
#' @keywords internal
suppress_specific_warnings <- function(x, patterns = NULL) {



  any_match <- function(cnd, patterns) {
    any(sapply(patterns, function(x) grepl(x, cnd)))
  }

  check_warning <- function(w) {
    if (any_match(conditionMessage(w), patterns))
      invokeRestart("muffleWarning")
  }

  withCallingHandlers(x, warning = check_warning)

}
