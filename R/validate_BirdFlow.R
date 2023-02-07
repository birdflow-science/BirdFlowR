#' Function to validate a BirdFlow object
#'
#' Throw an error if a BirdFlow object is malformed or incomplete.
#'
#' [preprocess_species()] creates a BirdFlow object that lacks both marginals
#' and transitions and thus can't be used to make projections.
#' `validate_BirdFlow()` tags the absence of these with the type "incomplete".
#'  Any other missing or malformed components are tagged "error".
#'
#' Since marginals can be used to calculate both distributions and
#' transition matrices, a BirdFlow object can be complete if it has marginals;
#' or has both transitions and distributions. Having redundancy in these three
#' is not considered an error.
#'
#' Currently metadata and species information is not checked for completeness.
#'
#' Currently dead end transitions are permitted.  See [find_dead_ends()] for
#' checking for those.
#'
#' @param x a BirdFlow object
#' @param error if TRUE throw an error if there are problems if FALSE return
#' any problems as a data.frame.
#' @param allow_incomplete if TRUE allow the BirdFlow object to be missing
#' both marginals and transitions (but not other components). This allows
#' checking the output of [preprocess_species()].
#'
#' @return  If `error = FALSE` the function returns
#'  a data.frame describing any errors with columns:
#' \describe{
#' \item{problem}{a character description of any problems}
#' \item{type}{the problem type, either "error" or "incomplete"}
#' }
#' Otherwise, if there are no problems a similar data.frame with no rows is
#' returned invisibly.
#' @export
validate_BirdFlow <- function(x, error = TRUE, allow_incomplete=FALSE){
  # problem types:
  #   error: BirdFlow object is malformed
  # incomplete: BirdFlow object is missing core components necessary for forecasting
  # missing metadata: BirdFlow object is missing

  stopifnot(error %in% c(TRUE, FALSE),
            allow_incomplete %in% c(TRUE, FALSE))


  # Look for problems in nested list names, order, and items by comparing
  # to canonical BirdFlow object returned by new_BirdFlow
  diff <- compare_list_item_names(x, new_BirdFlow() )
  problems <- paste(diff$where, diff$differences)
  # new_BirdFlow doesn't populate these lists so these aren't actually problems
  problems <- setdiff(
    problems,
    c("x$marginals should not be a list", # ok to have marginals
      "x$dates should not be a list", # ok to have dates
      "x$transitions should not be a list", # ok to have transitions
      "x extra:uci, lci",  # Ok to have these (included in preprocessing output)
      "x missing:marginals" ,# ok to be missing marginals
      "x$metadata$sparse_stats should not be a list"
    ) )

  p <- data.frame(problem = problems, type = rep("error", length(problems)) )

  add_prob <- function(problem, type, problems){
    stopifnot(type %in% c("error", "incomplete"))
    return(rbind(problems, data.frame(problem = problem, type = type)))
  }

  # check consistancy of has_ (transitions, marginals, distr)


  components <- c("transitions", "marginals", "distr")
  for(i in seq_along(components)){
    has <- x$metadata[[paste0("has_", components[i])]]
    if(!is.logical(has) || is.na(has) || length(has) != 1)
      p <- add_prob(paste0("has_", components[i], " is not TRUE or FALSE"),
                    "error", p)
  }
  if(has_distr(x)){
    if(!is.matrix(x$distr))
      p <- add_prob("distr is not a matrix", "error", p)
  }
  if(has_marginals(x)){
    if(!is.list(x$marginals))
      p <- add_prob("marginals is not a list", "error", p)
  }
  if(has_transitions(x)){
    if(!is.list(x$transitions))
      p <- add_prob("transitions is not a list", "error", p)
  }

  # Completeness
  if(!has_transitions(x) && !has_marginals(x))
    p <- add_prob("model has neither transitions nor marginals", "incomplete", p)

  if(!has_distr(x) && !has_marginals(x) )
    p <- add_prob("model hs neither distr nor marginals", "incomplete", p)

  # Consistent in number of transitions
  nt <- x$metadata$n_transitions
  if(is.na(nt)){
    if( (has_transitions(x)) || (has_marginals(x)) ){
      p <- add_prob(
        "model has transitions or marginals but n_transitions is NA",
        "error",p)
    }
  } else {  # number of transitions is not NA

    if(!is.numeric(nt) || !length(nt) == 1 || is.na(nt) || is.infinite(nt) ||
       !isTRUE(all.equal(nt %% 1, 0))|| !nt > 0){
      p <- add_prob("metadata$n_transitions not a single postive integer value",
                    "error", p)
    }
    if(has_transitions(x)){
      if(length(x$transitions) != nt)
        p <- add_prob("number of transitions not equal to n_transitions",
                      "error", p)
    }
    if(has_marginals(x)){
      if(length(setdiff(names(x$marginals), "index" ) ) != nt)
        p <- add_prob("number of stored marginals is not equal to n_transitions",
                      "error", p)
    }
  }  # end consistency checks on n_transitions

  # consistency on n_active
  if(is.na(n_active(x))){
    md <- x$metadata
    if( has_transitions(x) || has_marginals(x) || has_distr(x) ||
        !is.na(x$geom$mask) ) {
      p <- add_prob("n_active() is NA but x has elements that relate to n_active()",
                    "error", p)
    }
  } else {  # n_active not NA
    if( has_distr(x)  && !dim(x$distr)[1] == n_active(x) ) {
      p <- add_prob("distr is not consistent with n_active", "error", p)
    }

    if(has_marginals(x)){
      items <- setdiff(names(x$marginals), "index")
      correct_dim <- rep(NA, length(items))
      for(i in seq_along(items) ){
        correct_dim[i] <- all(dim(x$marginals[[items[i]]]) == n_active(x))
      }
      if(!all(correct_dim)){
        p <- add_prob("marginal dimensions inconsistent with n_active",
                      "error", p)
      }
    }
    if(has_transitions(x)){
      items <- names(x$transitions)
      correct_dim <- rep(NA, length(items))
      for(i in seq_along(items) ){
        correct_dim[i] <- all(dim(x$transitions[[items[i]]]) == n_active(x))
      }
      if(!all(correct_dim)){
        p <- add_prob("transition dimensions inconsistent with n_active",
                      "error", p)
      }
    }

  } # end n_active consistency check

  # check geometry
  if(!is.list(x$geom)){
    p <- add_prob("x$geom is not a list", "error", p)
  } else {
    expected.names <- names(new_BirdFlow()$geom)
    if(!all(expected.names %in% names(x$geom) )) {
      p <- add_prob(paste0("x$geom is missing",
                           paste(setdiff(names(x$geom), expected.names),
                                 collapse = ", ") ), "error", p)
    } else {  # has geom list with complete set of names
      if(!is.matrix(x$geom$mask)){
        p <- add_prob("x$geom$mask is not a matrix", "error", p)
      } else { # mask is matrix
        if(!is.logical(x$geom$mas))
          p <- add_prob("mask isn't logical", "error", p)
        if(is.na(n_active(x)) || !n_active(x) == sum(x$geom$mask))
          p <- add_prob("mask isn't consistent with n_active", "error", p)
        if(is.na(nrow(x)) || ! nrow(x) == nrow(x$geom$mask))
          p <- add_prob("nrow(mask) not equal to nrow(x)", "error", p)
        if(is.na(ncol(x)) || !ncol(x) == ncol(x$geom$mask))
          p <- add_prob("ncol(mask) not equal to ncol(x)", "error", p)
      }

    } # end geom list is complete
  } # end geom is list

  # check dates
  if( !"dates" %in% names(x) || !is.data.frame(x$dates)){
    p <- add_prob("x$dates is missing, NA or not a dataframe", "error", p)
  } else { # dates exists and is data.frame

    required.cols <- c( "interval", "date", "doy" )
    if( !all( required.cols  %in% names( x$dates ) ) ){
      p <- add_prob( paste0("dates is missing columns:",
                            paste(setdiff(required.cols, names(x$dates)))),
                     "error", p)
    } # end if dates missing columns
    rm( required.cols )

    if("distr" %in% names(x) && is.data.frame(x$distr)){
      if( nrow(x$dates) != ncol(x$distr)){
        p <- add_prob( "x$dates")
      }
    }

  } # end dates is data.frame


  # check marginal names and index
  if( "marginals" %in% names(x) && is.list(x$marginals) ){

     mn <- setdiff(names(x$marginals), "index")
    index <- x$marginals$index
    if(!all(mn %in%  index$marginal))
      p <- add_prob("Not all marginals are indexed.", "error", p)
    if(!all(index$marginal %in% mn))
      p <- add_prob("Not all marginals in index exist")

    # Check that all marginals sum to 1
    marginal_sums <- as.numeric(sapply(mn, function(m) sum(x$marginals[[m]] ) ))
    sums_to_one <- sapply(marginal_sums,
                          function(s) isTRUE(all.equal(s, 1, tolerance = 1e-6)))
    if(!all(sums_to_one))
      p <- add_prob("Not all marginals have a sum of one.", "error", p)
  }


  # Still need to validate transitions!

  if(error){
    if(allow_incomplete){
      if(any(p$type == "error"))
        stop("Problems found:", paste(p$problem[p$type == "error"], collapse ="; "))
    } else { # Don't allow incomplete:
      if(nrow(p) > 0)
        stop("Problems found:", paste(p$problem, collapse ="; "))
    }
  }

  if(error == TRUE) return(invisible(p))
  return(p)

} # end validation function








