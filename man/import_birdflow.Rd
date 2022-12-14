% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_birdflow.R
\name{import_birdflow}
\alias{import_birdflow}
\title{import BirdFlow model from an hdf5 file}
\usage{
import_birdflow(hdf5, tiff, species)
}
\arguments{
\item{hdf5}{Path to an hdf5 file}

\item{tiff}{Path to the model geotiff}

\item{species}{An eBird species code.  It should appear in the
\code{species_code} column of the data.frame returned by
\code{\link[auk:get_ebird_taxonomy]{auk::get_ebird_taxonomy()}}}
}
\value{
a BirdFlow object
}
\description{
This function imports a BirdFlow model data from an HDF5 file written by
python.
}
\details{
The hdf5 file should have
\itemize{
\item \code{date} the date the model was exported from python
\item \code{densities} the training distributions in their vector forms stored in a
matrix
\item \code{dtuple} (metadata list) with items:
\itemize{
\item \code{cells} the number of active (non-masked) cells
\item \code{nan_mask} a factor matrix of the raster extent with 'TRUE' for masked
cells, and 'FALSE' for unmasked. This is the reverse of the BirdFlow
internal representation which uses TRUE for active cells.
\item \code{weeks} the number of timesteps in the model
\item \code{x_dim} the number of columns in the model raster
\item \code{y_dim} the number of rows in the model raster
}
\item \code{marginals} the marginal probabilities between adjacent timesteps in the
birdflow model. R reads these in column dominant format even though
they are stored in row-dominant so they must be transposed.
\item \code{version} an integer indicating the version of the BirdFlow hdf5 file
format.  We will increment this when we change what's in the file.
}

The geoTiff is required for the coordinate reference system (CRS) and the
spatial extent. In the future we hope to add this information, along with the
species code to the hdf5 file.  Currently the mask and some other attributes
from the hdf5 are checked against redundant versions in the geoTiff.
}
