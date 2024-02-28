#' Export and import BirdFlow models in HDF5 format
#'
#' `import_birdflow()` imports a BirdFlow model from an HDF5
#' (Hierarchical Data Format version 5) file.
#'
#' THe standard workflow for generating a fitted BirdFlow model is:
#' 1. Use [preprocess_species()] to download and format eBird data into an
#' HDF5 file (this calls `export_birdflow()` internally).
#' 2. Fit the model and add marginals and hyperparameters to the HDF5 file
#' with [BirdFlowPy](https://github.com/birdflow-science/BirdFlowPy)
#' 3. Import the model with `import_birdflow()`.
#' 4. Save the model to a new file either with:
#'    * [saveRDS()] to a .rds file, or
#'    * `export_birdflow()` to a new .hdf5 file.
#'
#' @details
#' During the first import after fitting with
#' [BirdFlowPy](https://github.com/birdflow-science/BirdFlowPy)
#' `import_birdflow()` does a fair amount of renaming and reformatting of the
#' data - for example it renames the marginals and adds a marginal index.
#' If the resulting BirdFlow model is exported again with `export_birdflow()`
#' these updates are retained, so the internal structure of the HDF5 file
#' will be different.  `import_birdflow()` can handle either format.
#'
#' @seealso
#' * [sparsify()] to reduce the object size after importing.
#' * [build_transitions()] to add transition matrices for quicker processing
#' at the cost of larger file sizes.
#' * [export_rasters()] to save the distributions and mask from a
#' BirdFlow model to raster files.
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
#'   The version number (1) is kept in `"/version"` within the HDF5 file.
#'
#' * Version 2 is the first version that includes preprocessing in BirdFlowR.
#'   It does not include dynamic masking.  The version number (2) is stored
#'   in  `"/metadata/birdflow_version"`.  Some version 2 files include
#'   hyper parameters in the HDF5 but these are not read into R.
#'
#' * Version 3 marks the transition to dynamic masking, the HDF5 created during
#'   preprocessing gained `"/geom/dynamic_mask"` and `"/distances"`  (great
#'   circle distance matrix).  As in version 2 the version is
#'   stored in `"/metadata/birdflow_version"` but unlike previous versions the
#'   R package version that did the preprocessing is saved in character
#'   format to `"/metdata/birdflowr_version"`. When version 3 files are imported
#'   into R with model fits the hyperparameters are saved to
#'   `$metadata$hyperparameters` (a list).
#'
#' @param hdf5 Path to an HDF5 file containing a fitted BirdFlow model.
#' @param ... Deprectated, arguments to be passed to a version specific
#'  internal functions.
#' @param version (optional) force reading of BirdFlow models as a particular
#'   version. Normally, this will be determined from metadata in the HDF5
#'   file.
#' @return `import_birdflow()` returns a BirdFlow object.
#' @importFrom Matrix Matrix
#' @importFrom rhdf5 h5ls
#' @importFrom rhdf5 h5read
#' @rdname export_import_birdflow
#' @export
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



  if (version == 1)
    stop("Importing this version ", version,
         " BirdFlow model is no longer supported")

  if (version == 2)
    stop("Importing this version ", version,
         " BirdFlow model is no longer supported. ",
         "Use BirdFlowR version 0.1.0.9039 to import it.")

  return(import_birdflow_v3(hdf5 = hdf5, ...))
}
