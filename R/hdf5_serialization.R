#' @name hdf5_serialization
#' @rdname hdf5_serialization
#' @aliases write_r_object_h5 read_r_object_h5
#' @aliases read_Rotues write_Rotues
#' @aliases read_BirdFlowRotues write_BirdFlowRotues
#' @aliases read_BirdFlowIntervals write_BirdFlowIntervals
#'
#' @title HDF5-based Serialization and Deserialization of R Objects
#'
#' @description
#' These functions provide a convenient interface for writing arbitrary R
#' objects to HDF5 files and reading them back, preserving S3 class
#' metadata. Specialized helpers are provided for common BirdFlow-related
#' objects (routes and intervals), automatically restoring Date columns.
#'
#' @details
#' - **write_r_object_h5** and **read_r_object_h5** are internal workhorses:
#'   they handle the recursive traversal of R lists (including data.frames,
#'   Date/POSIX/Facter vectors) and store both data and class attributes
#'   in the HDF5 file hierarchy.  
#' - The higher-level helpers (**read_Rotues**, **write_Rotues**,
#'   **read_BirdFlowRotues**, **write_BirdFlowRotues**, 
#'   **read_BirdFlowIntervals**,
#'   **write_BirdFlowIntervals**) wrap these internals to provide
#'   suppressed warnings and post-processing such as coercing date columns
#'   back to `Date` objects.
#'
#' @section Internal functions:
#' These two functions are not intended for direct use and are hidden from
#' the user documentation.  
#' \itemize{
#'   \item \code{write_r_object_h5(obj, file, path = "/")}  
#'   \item \code{read_r_object_h5(file)}  
#' }
#'
#' @param obj
#'   An R object of Routes, BirdFlowRoutes, or BirdFlowIntervals.
#' @param obj_path
#'   Path to the HDF5 file containing a serialized object for reading or
#'   writing with the BirdFlow-specific helper.
#' @importFrom rhdf5 h5createFile h5createGroup h5write h5writeAttribute 
#' H5Fopen H5Fclose H5Lexists h5ls h5read h5readAttributes h5closeAll H5Fis_hdf5
#' @importFrom stats setNames
#' @return
#' - \code{write_*} functions invisibly return their input object (after
#'   writing to disk).  
#' - \code{read_*} functions return the reconstructed R object,
#'   with S3 class attributes and Date columns restored where applicable.
#'
#' @seealso
#' - \code{\link[rhdf5]{h5createFile}}, \code{\link[rhdf5]{h5write}},
#'   \code{\link[rhdf5]{h5read}} for low-level HDF5 operations.  
#'
#' @keywords internal
NULL


write_r_object_h5 <- function(obj, file, path = "/") {
  if (!requireNamespace("rhdf5", quietly = TRUE))
    stop("Please install rhdf5")
  
  # on first call, remove any existing file
  if (identical(path, "/")) {
    if (file.exists(file)) file.remove(file)
    h5createFile(file)
    # write root class via filename-based attribute
    h5writeAttribute(attr = class(obj),
                     h5loc = "/",
                     h5obj = file,
                     name  = "class")
    h5writeAttribute(attr = names(obj),
                     h5loc = "/",
                     h5obj = file,
                     name  = "names")
  }
  
  h5file <- H5Fopen(file)
  on.exit(H5Fclose(h5file), add = TRUE)
  
  write_node <- function(x, loc) {
    rel <- sub("^/", "", loc)
    
    if (!is.list(x) || inherits(x, c("Date","POSIXt")) || is.factor(x)) {
      if (inherits(x, c("Date","POSIXt","factor"))) {x <- as.character(x)}
      parent <- dirname(rel)
      if (parent != "" && parent != "." && !H5Lexists(h5file, parent))
        h5createGroup(h5file, parent)
      h5write(x, h5file, rel)
      
      # 1) record its R class
      h5writeAttribute(
        attr = class(x),
        h5loc = paste0("/", rel),
        h5obj = file,
        name  = "class"
      )
      
      # 2) record element‐names if present
      if (!is.null(names(x))) {
        h5writeAttribute(
          attr = names(x),
          h5loc = paste0("/", rel),
          h5obj = file,
          name  = "names"
        )
      }
      
      # 3) record dims if this was an array/matrix
      if (!is.null(dim(x))) {
        h5writeAttribute(
          attr = dim(x),
          h5loc = paste0("/", rel),
          h5obj = file,
          name  = "dim"
        )
        
        ## record dimnames (row & column names) if present
        dnm <- dimnames(x)
        if (!is.null(dnm)) {
          # names of the two dims (e.g. c("i","time"))
          if (!is.null(names(dnm))) {
            h5writeAttribute(
              attr   = names(dnm),
              h5loc  = paste0("/", rel),
              h5obj  = file,
              name   = "dimnames_names"
            )
          }
          # the actual character vectors
          h5writeAttribute(
            attr   = dnm[[1]],
            h5loc  = paste0("/", rel),
            h5obj  = file,
            name   = "dimnames1"
          )
          if (length(dnm) > 1) {
            h5writeAttribute(
              attr   = dnm[[2]],
              h5loc  = paste0("/", rel),
              h5obj  = file,
              name   = "dimnames2"
            )
          }
        }
      }
    }
    
    # group
    if (loc != "/") {
      if (!H5Lexists(h5file, rel)) {
        h5createGroup(h5file, rel)
      }
      # write group attribute by filename+loc
      h5writeAttribute(attr = names(x), # the items order
                       h5loc = loc, 
                       h5obj = file, 
                       name = "names")
      h5writeAttribute(attr = class(x),
                       h5loc = loc,
                       h5obj = file,
                       name  = "class")
    }
    
    for (nm in names(x)) {
      child <- if (loc == "/") paste0("/", nm) else paste0(loc, "/", nm)
      write_node(x[[nm]], child)
    }
  }
  
  write_node(obj, path)
}


read_r_object_h5 <- function(file) {
  if (!requireNamespace("rhdf5", quietly = TRUE))
    stop("Please install rhdf5")
  if (!file.exists(file) || is.na(H5Fis_hdf5(file)))
    stop("Invalid HDF5 file: ", file)
  
  # 1) Close any stray handles
  h5closeAll()
  
  # 2) Get a flat listing of the file’s hierarchy
  info <- h5ls(file, recursive = TRUE)
  
  # 3) Open a single read handle
  fid <- H5Fopen(file)
  on.exit(H5Fclose(fid), add = TRUE)
  
  # 4) Recursive reader
  read_node <- function(loc) {
    children <- info$name[info$group == loc]
    
    # Recover the naming order of objects
    attrs <- h5readAttributes(file, loc)
    children <- info$name[info$group == loc]
    if (!is.null(attrs$names)) {
      ordered <- intersect(attrs$names, children)
      children <- c(ordered, setdiff(children, ordered))
    }
    
    # Leaf: no children -> dataset
    if (length(children) == 0) {
      x <- h5read(fid, loc)
      
      # grab back the attrs
      attrs <- h5readAttributes(file, loc)
      
      # 1) if we recorded an exact dim(), restore it
      if ("dim" %in% names(attrs)) {
        dim(x) <- as.integer(attrs$dim)
        # 1b) restore dimnames if recorded
        if ("dimnames1" %in% names(attrs) || "dimnames2" %in% names(attrs)) {
          dn1 <- as.character(attrs$dimnames1)
          dn2 <- as.character(attrs$dimnames2)
          dimnames(x) <- list(dn1, dn2)
          # restore the names of each dim (e.g. "i","time")
          if ("dimnames_names" %in% names(attrs)) {
            names(dimnames(x)) <- attrs$dimnames_names
          }
        }
        
      # 2) otherwise, flatten any 1-D array back to an atomic vector
      } else if (!is.null(dim(x)) && length(dim(x)) == 1L) {
        x <- as.vector(x)
        
      # 3) for true multi-D, just drop any singleton dimensions
      } else if (!is.null(dim(x))) {
        x <- drop(x)
      }
      
      # 4) restore element-names if we recorded them
      if ("names" %in% names(attrs)) {
        names(x) <- attrs$names
      }
      
      # 5) now restore the R classes (Date/POSIX/factor/…)
      if (!is.null(attrs$class)) {
        cl <- as.character(attrs$class)
        if ("Date"   %in% cl) x <- as.Date(x)
        if ("POSIXt" %in% cl) x <- as.POSIXct(x)
        if ("factor" %in% cl) x <- factor(x)
        other <- setdiff(cl, c("Date","POSIXt","factor"))
        if (length(other)) class(x) <- other
      }
      
      return(x)
    }
    
    # Otherwise, build a list of child nodes
    out <- setNames(vector("list", length(children)), children)
    for (nm in children) {
      child_loc <- if (loc == "/") paste0("/", nm) else paste0(loc, "/", nm)
      out[[nm]] <- read_node(child_loc)
    }
    
    # Restore S3 class (and data.frame coercion) based on stored attribute
    attrs <- h5readAttributes(file, loc)
    cl_attr <- attrs$class
    if (!is.null(cl_attr) && (is.array(cl_attr) || is.list(cl_attr))) {
      cl_attr <- as.character(cl_attr)
    }
    
    if ("data.frame" %in% cl_attr) {
      # Coerce list → data.frame
      out <- as.data.frame(out, stringsAsFactors = FALSE)
    } else if (!is.null(cl_attr)) {
      class(out) <- cl_attr
    }
    
    out
  }
  
  # 5) Read the root
  root <- read_node("/")
  
  # 6) Restore root’s atomic or custom S3 class
  rattrs <- h5readAttributes(file, "/")
  if ("class" %in% names(rattrs)) {
    cl <- rattrs$class
    if (is.array(cl) || is.list(cl)) cl <- as.character(cl)
    
    # If root was atomic, coerce back
    if (!is.list(root)) {
      if ("Date"   %in% cl) root <- as.Date(root)
      if ("POSIXt" %in% cl) root <- as.POSIXct(root)
      if ("factor"%in% cl)  root <- factor(root)
    }
    # If it was a custom S3 list
    if (is.list(root) && length(setdiff(cl, c("list", "data.frame"))) > 0) {
      class(root) <- cl
    }
  }
  
  return(root)
}



#' @rdname hdf5_serialization
#' @title Generic function for writing Routes and 
#' BirdFlowRoutes objects to HDF5.
#'
#' @param obj The Routes or BirdFlowRoutes object.
#' @param obj_path Path to write the HDF5 file.
#' @export
write_routes <- function(obj, obj_path) {
  UseMethod("write_routes")
}

#' @rdname hdf5_serialization
#' @method write_routes Routes
#' @export
write_routes.Routes <- function(obj, obj_path) {
  validate_Routes(obj)
  suppressWarnings(write_r_object_h5(obj, obj_path))
  invisible(obj)
}

#' @rdname hdf5_serialization
#' @method write_routes BirdFlowRoutes
#' @export
write_routes.BirdFlowRoutes <- function(obj, obj_path) {
  validate_BirdFlowRoutes(obj)
  suppressWarnings(write_r_object_h5(obj, obj_path))
  invisible(obj)
}

#' @rdname hdf5_serialization
#' @method write_routes default
#' @export
write_routes.default <- function(obj, obj_path) {
  warning(sprintf(
    "write_routes only supports objects of class 'Routes' or 
    'BirdFlowRoutes'; If you are writing BirdFlowIntervals object, please use
    'write_BirdFlowIntervals'; got class '%s'", 
    paste(class(obj), collapse = ", ")
  ))
  invisible(obj)
}

#' @rdname hdf5_serialization
#' @title A psudo-generic function for reading Routes and BirdFlowRoutes objects
#' from HDF5 files.
#' 
#' @param obj_path Path to write the HDF5 file.
#' @export
read_routes <- function(obj_path) {
  suppressWarnings(
    obj <- read_r_object_h5(obj_path)
  )
  obj$data$date <- as.Date(obj$data$date)
  if ("BirdFlowRoutes" %in% class(obj)) {
    validate_BirdFlowRoutes(obj)
  } else if ('Routes' %in% class(obj)) {
    validate_Routes(obj)
  } else {
    stop("The loaded object is neither Routes nor BirdFlowRoutes. 
         If you are reading BirdFlowIntervals object, please use
         `read_BirdFlowIntervals`")
  }
  return(obj)
}

# BirdFlowIntervals
#' @rdname hdf5_serialization
#' @param obj_path The BirdFlowIntervals object path to read from
#' @export
read_BirdFlowIntervals <- function(obj_path) {
  suppressWarnings(
    obj <- read_r_object_h5(obj_path)
  )
  obj$data$date1 <- as.Date(obj$data$date1)
  obj$data$date2 <- as.Date(obj$data$date2)
  validate_BirdFlowIntervals(obj)
  return(obj)
}

#' @rdname hdf5_serialization
#' @param obj The BirdFlowIntervals object to write
#' @param obj_path The path to write
#' @export
write_BirdFlowIntervals <- function(obj, obj_path) {
  validate_BirdFlowIntervals(obj)
  suppressWarnings(
    obj <- write_r_object_h5(obj, obj_path)
  )
  invisible(obj)
}
