#' pad timesteps from BirdFlow models
#'
#' This function is mostly for internal use but exported for advanced users.
#' It's primary purpose is to pad timesteps with zeros for looking up
#' transition names. It is called from [lookup_transitions()] which is in
#' turn used by [get_transition()].  Padding is generally two digit but the
#' level of padding is stored in the BirdFlow object so that we'll
#' be able to switch to three digit timesteps easily if, for example,
#' someday we decide to have daily timesteps.
#'
#' @param x A vector of timestep integers
#' @param bf A BirdFlow model
#'
#' @return a string with padded versions of x;  `1` becomes `"01"`.
#' @export
#' @keywords internal
#' @examples
#' bf <- BirdFlowModels::amewoo
#' pad_timestep(1:5, bf)
pad_timestep  <- function(x, bf) {
  padding <- get_timestep_padding(bf)
  stringr::str_pad(x, width = padding, pad = "0")
}
