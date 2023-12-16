#' Internal function to send a message from within BirdFlow functions
#'
#' It checks to see if BirdFlowR is in verbose mode
#' (`birdflow_options("verbose"`) and if so pastes it's arguments together
#' and  prints the result with  `cat()`.
#' In the future it might be updated to use `message()`
#'
#' @param ... Text that will be pasted together to make a message.
#' @param sep (optional) separator between text elements in `...`
#' defaults to no separation.
#'
#' @return Nothing is returned if verbose is TRUE the message is printed.
#'
#' @keywords internal
#' @seealso [birdflow_options()] for changing verbosity.
bf_msg <- function(..., sep = "") {
  m <- paste(..., sep = sep)
  if (birdflow_options("verbose")) {
    cat(m)
  }
}

#' Conditionally suppress messages from expressions in BirdFlowR code
#'
#' This internal functions is used to suppress messages thrown
#' by functions called in BirdFlowR code if `birdflow_options("verbose")`
#' is `FALSE`.
#'
#' @param exp R code that might throw a message (originating outside of
#' \pkg{BirdFlowR}.
#' @keywords internal
#' @seealso [preprocess_species()] uses this when calling \pkg{ebirdst}
#' functions that display messages.
#' When BirdFlowR functions generate messages they should use [bf_msg()] so that
#' `birdflow_options("verbose")` is honored.
#' @examples
#' \dontrun{
#' # bf_suppress_msg isn't exported so can't be run in examples
#' # in internal code or after  devtools::load_all() example will work
#'    ob <- birdflow_options("verbose")
#'    birdflow_options(verbose = FALSE)
#'    bf_suppress_msg( message("hi" ))
#'    birdflow_options(verbose = TRUE)
#'    bf_suppress_msg( message("hi" ))
#'    birdflow_options(ob)
#'  }
#'
bf_suppress_msg <- function(exp) {
  verbose <- birdflow_options("verbose")
  withCallingHandlers(
    message = function(m) {
      if (!verbose)
        tryInvokeRestart("muffleMessage")
    },
    exp
  )
}
