if(FALSE){
  load_collection_index()
  bf <- load_model("amewoo_prebreeding")
  bf
  dir <- "C:/temp/amewoo_export/"
  crs <-"EPSG:4326"
  export_tifs(bf, dir = dir, singleband = TRUE)

  dir <- "C:/temp/amewoo_export_EPSG3857/"
  crs <-"EPSG:3857"
  export_tifs(bf, dir, singleband = TRUE, crs = crs)

}


#' export distributions or masks from a BirdFlow object to TIFF files.
#'
#' This function exports the distributions and/or dynamic masks from a
#' BirdFlow object to one or more TIFF files.
#'
#' This replaces the old behavior of [preprocess_species()] which
#' allowed exporting TIFF files of the distributions while preprocessing.
#'
#' @param bf A BirdFlow object
#' @param dir the directory where output should be stored
#' @param crs The coordinate reference system to use in the output files,
#'   defaults to  `crs(bf)`
#' @param multiband If TRUE export a multiband file
#' @param singleband If TRUE export files for each week in the model.
#' @param what (character) either "distr", "mask" or, the default, both
#' (as a two element vector). `what` controls what components of `bf` are
#' exported.
#'
#' @return Nothing is returned, but files are created on disk.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'   bf <- load_model("amewoo_prebreeding")
#'   dir <- tempdir()
#'   crs <-"EPSG:4326"
#'   export_tifs(bf, dir = dir, singleband = TRUE, crs = crs)
#' }
#'
export_tifs <- function(bf, dir, crs = NULL,
                        multiband = TRUE,
                        singleband = !multiband,
                        what = c("distr", "mask")) {
  stopifnot(all(what %in% c("distr", "mask")))


  if(is.null(crs))
    crs <- crs(bf)

  for(i in seq_along(what)){

    if(what[i] == "mask"){
      r <- get_dynamic_mask(bf) |>  rasterize_distr(bf)
      format <- "INT1U"

    }
    if(what[i] == "distr"){
      r <- get_distr(bf) |> rasterize_distr(bf)
      format <- "FLT4S"
    }

    names(r) <- 1:n_timesteps(bf)
    terra::values(r) <- as.integer(terra::values(r))
    crs <- terra::crs(crs)
    if (crs(bf) != crs) {
      r <- terra::project(r, crs)
    }

    if (multiband) {
      file <- file.path(dir, paste0(species(bf, "code"),
                                    "_", what[i],
                                    ".tif"))
      dir.create(dir, showWarnings = FALSE)
      terra::writeRaster(r, filename = file)
    }

    if (singleband) {
      files <-  file.path(
        dir,
        paste0(species(bf, "code"),
               "_", what[i], "_",
               pad_timestep(seq_len(n_timesteps(bf)), bf),
               ".tif"))
      for(i in seq_len(n_timesteps(bf))){
       terra::writeRaster(r[[i]], file = files[i], datatype = format)
      }
    } # end if single band
  } # end loop through what (distr, and/or mask)
}  # end function
