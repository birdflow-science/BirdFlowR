if(FALSE){
  full_bf <- import_birdflow("../Models/Visualization/original/batch1/round1/robgro_2021_80km.hdf5")
  sparse_bf <- sparsify(full_bf, "state+conditional", p = .99)
  bf <-  build_transitions(sparse_bf)
}



#' add transition matrices to a BirdFlow object
#'
#' Given a BirdFlow object with marginals and without transitions return
#' a BirdFlow object with both marginals and transitions.
#'
#' @param x BirdFlow object
#'
#' @return BirdFlow object with transition matrices
#' @export
#'
#' @examples
#' \dontrun{
#' bf1 <- BirdFlowModels::amewoo
#' bf2 <- build_transitions(bf)
#' }
build_transitions <- function(x){
  mi <- x$marginals$index
  tl <- vector(mode = "list", length = nrow(mi))
  names(tl) <- mi$transition
  for(i in 1:nrow(mi)){
    tl[[i]] <- get_transition(x, mi$transition[i])
  }
  x$transitions <- tl
  x$metadata$has_transitions <- TRUE
  return(x)
}
