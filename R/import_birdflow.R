#' Import BirdFlow models from HDF5 files
#'
#' This function imports a BirdFlow model from an HDF5
#' (Hierarchical Data Format version 5) file. The workflow is to
#' first use [preprocess_species()] to download and format the data for
#' model fitting, producing an HDF5 file; then use BirdFlow Python software to
#' fit the model and add marginals and hyperparameters to the HDF5 file; and,
#' finally import the model with `import_birdflow()`.
#'
#' @details After importing you may want to call [sparsify()] to reduce the
#' object size.  If you plan on doing many calls to [route()] or [predict()]
#' you should call [build_transitions()] as well.
#'
#'
#' @section HDF5 file version:
#'
#' The HDF5 BirdFlow model files have an internal version number that is
#' incremented on major changes to the HDF5 file structure. The file
#' version is retained in the imported object.
#'
#' * Version 1 predates [BirdFlowR preprocessing][preprocess_species()] and only
#'   contains marginals and a few other python objects, importing it requires
#'   having an associated TIFF file that has the extent and distribution data.
#'   The version number (1) is kept in "/version" within the HDF5 file.
#'
#' * Version 2 is the first version that includes preprocessing in BirdFlowR.
#'   It does not include dynamic masking.  The version number (2) is stored
#'   in  "/metadata/birdflow_version".  Some version 2 files include
#'   hyper parameters in the HDF5 but these are not read into R.
#'
#' * Version 3 marks the transition to dynamic masking, the HDF5 created during
#'   preprocessing gained "/geom/dynamic_mask" and "/distances"  (great
#'   circle distance matrix).  As in version 2 the version is
#'   stored in "/metadata/birdflow_version" but unlike previous versions the
#'   R package version that did the preprocessing is saved in character
#'   format to "/metdata/birdflowr_version". When version 3 files are imported
#'   into R with model fits the hyperparameters are saved to
#'   `$metadata$hyperparameters` (a list).
#'
#' @param hdf5 Path to an HDF5 file containing a fitted BirdFlow model.
#' @param ... Arguments to be passed to a version specific internal function.
#'    Likely will only be used with
#'    version 1 which takes two additional arguments: \describe{
#'        \item{tiff}{Path to the model geotiff}
#'        \item{species}{An eBird species code.  It should appear in the
#'   `species_code` column of the data.frame returned by
#'   [auk::get_ebird_taxonomy()]}
#'    }
#' @param version (optional) force reading of BirdFlow models as a particular
#'   version. Normally, this will be determined from metadata in the HDF5
#'   file.
#' @return a BirdFlow object
#' @export
#' @importFrom Matrix Matrix
#' @importFrom rhdf5 h5ls
#' @importFrom rhdf5 h5read
import_birdflow <- function(hdf5, ..., version) {

  current_version <- new_BirdFlow()$metadata$birdflow_version

  if (missing(version)) {
    contents <- h5ls(hdf5)
    contents <- paste0(contents$group, "/", contents$name)
    contents <- gsub("^/*", "", contents)

    if ("version" %in% contents) {
      # used in version 1
      version <- as.vector(h5read(hdf5, "version"))
    } else if ("metadata/birdflow_version" %in% contents) {
      # used in version 2+
      version <- as.vector(h5read(hdf5, "metadata/birdflow_version"))
    } else {
      # default to current version
      version <- current_version
    }
  }

  version <- as.character(version)
  return(switch(as.character(version),
                "1" = stop("Importing Version 1  BirdFlow models is no longer",
                "supported."),
                "2" = import_birdflow_v2(hdf5 = hdf5, ...),
                "3" = import_birdflow_v3(hdf5 = hdf5, ...),
                stop("Unrecognized version. ",
                     "Was this model fit with a newer version of BirdFlowR?")))
}
