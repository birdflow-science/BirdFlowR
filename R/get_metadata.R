

#' retrieve BirdFlow metadata
#'
#' `get_metadata` retreives information about the BirdFlow model: when it was
#' created, what version of eBird Status & Trends data was used, whether it has
#' been sparsified,  etc.
#'
#' `what` may be used with these options:
#'|`all`| Return the complete list, this is the default |
#'| --- | --- |
#'| `has_marginals` | Logical, indicates whether the model has marginals.  Marginals encode forward and backward transitions, and distributions in an n by n matrix for each pair of adjacent timesteps. |
#'| `has_transitions` | Logical, indicates whether the model has stored transitions (separate from marginal) |
#'|  `has_distr` | Logical, indicates whether the model has stored distributions (separate from marginals); if it does they are from \pkg{ebirdst} and are the distributions the model was trained on |
#'| `n_transitions` | Number of transitions in the model |
#'|  `n_active` | The number of active states in the model. This defines the dimensions of the marginals and transitions matrices and the number of columns in flattened distributions. In a full (not sparse) model all active states could be visited.|
#'|  `n_timesteps` | The number of timesteps in the model. |
#'|  `ebird_version_year` | The version year for the eBird Status and Trends Data the model was trained on see [ebirdst_version()][ebirdst::ebirdst_version()] |
#'|  `ebird_release_year` | The year the eBird S & T data was released [ebirdst_version()][ebirdst::ebirdst_version()] |
#'| `ebird_access_end_date` | The date at which the training eBirds S & T data will no longer be available. This does not prevent continued use of the BirdFlow model |
#'|  `birdflow_preprocess_date` | The date the data was downloaded with \pkg{ebirdst} and formated for BirdFlow model fitting (see [preprocess_species()] |
#'| `birdflow_model_date` | The date the model was fitted and/or exported to hdf5 from python" |
#'| `is_sparse` | Logical, if `TRUE` the model is sparse. See [sparsify()] |
#'|  `sparse_stats` | If the model is sparse this contains sparsification statistics (a list) |
#'
#'
#' @param x BirdFlow model
#' @param what Indicates what metadata to return. The defualt `"all"` returns
#' the complete list. Options are described in details.
#'
#' @return Metadata from the BirdFlow model. Potentially a list.
#' @export
#'
#' @seealso
#'  [has_transitions()], [has_marginals()], [has_distr()], [n_active()],
#'  [n_transitions()], [n_timesteps()] all return metadata components directly.
#'
#'  [species_info()] is similar but for information about the species
#'  represented in the model.
#'
#'  [ebirdst::ebirdst_version()] is called while assembling the model to set
#'  `ebird_version_year`, `ebird_release_year` and `ebird_access_end_date`
#'
#'  [Dimensions][nrow()] documents getting various attributes of a BirdFlow
#'  model, some of which overlap `get_metadata()`

#' @examples
#'library(BirdFlowModels)
#'get_metadata(amewoo)
#'get_metadata(amewoo, "is_sparse")
#'
get_metadata <- function(x, what){
  if(missing(what)) what <- "all"
  what <- tolower(what)
  nbf <- new_BirdFlow()
  valid_metatdata_names  <- c( "all", names(nbf$metadata) )

  if(!what %in% c(valid_metatdata_names)){
    stop("what should be one of ", paste(valid_metatdata_names, collapse = ", "))
  }

  if(what == "all")
    return(bf$metadata)

  return(bf$metadata[[what]])

}
