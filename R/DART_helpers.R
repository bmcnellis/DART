#' @title Helper DART functions
#'
#' @description
#'
#' Short utility functions specific to DART/webDART functionality
#'
#' @details NA
#'
#' @author Steve Fick
#' @author Travis Nauman
#' @author Anna Knight
#' @author Brandon McNellis
#'
#' @name DART_helpers
#' @rdname DART_helpers
NULL
#' @rdname DART_helpers
#' @export
ClearTempDART <- function(DART_obj) {
  stopifnot(validObject(DART_obj))

  fls <- list.files(DART_obj@intermediate_directory, full.names = T)

  if (length(fls) == 0) {
    message('no files to clear')
  } else if (any(!file.remove(fls, recursive = T))) {
    stop('file removing failed')
  } else {
    message('files cleared OK')
  }

  invisible()
}
#' @rdname DART_helpers
#' @export
GetDefaultTopoVars <- function() {
  c('EASTNESS', 'SOUTHNESS', 'ELEVm',
    'TCURV', 'PCURV',
    'MRRTF', 'MRVBF',
    'TWI_TOPMODEL', 'CAlog_10',
    'RELHT1', 'RELHT32', 'RELHT128',
    'RELMNHT1', 'RELMNHT32', 'RELMNHT128')
}
#' @rdname DART_helpers
#' @export
SubsamplePixels <- function(stack0, seed, prop, n_gower) {

  set.seed(seed)
  nonNA_pix <- numeric()
  stack_out <- raster::stack()

  if (nlayers(stack0) > 1) {
    layer0 <- stack0[[1]]
  } else {
    layer0 <- stack0
  }

  size0 <- length(!is.na(raster::getValues(layer0)))
  prop0 <- round(size0 * prop)

  rast0 <- raster::sampleRandom(x = layer0, size = prop0, na.rm = T, asRaster = T)
  stack_out <- suppressWarnings(raster::mask(stack0, layer0))

  return(stack_out)
}
#' @rdname DART_helpers
#' @export
does_raster_exist <- function(name, dir) {
  stopifnot(is.character(name))

  if (!file.exists(name)) {
    name0 <- file.path(dir, name)
    if (!file.exists(name0)) {
      stop('Raster file does not exist - try specifying a full file path')
    }
  } else {
    name0 <- name
  }

  return(name0)
}
#' @rdname DART_helpers
#' @export
splitDART <- function(p, cellsz = 1000){
  require(sf)
  require(sp)

  # BEM: Used to be 'split', renamed to avoid NAMESPACE conflicts
  # Not sure where this function is even called though.

  # split polygons by grid
  x <- sf::st_as_sf(p)
  grid <- sf::st_make_grid(p, cellsize = cellsz)
  int <- sf::st_intersection(x,grid)

  int0 <- as(int, 'Spatial')

  return(int0)
}
#' @rdname DART_helpers
#' @export
masterMask <- function(rasters) {
  # Copied from SEEG-Oxcord/seegSDM
  # License is GPL>2
  # Authors: Nick Golding & Freya Shearer

  master <- rasters[[1]]

  for (i in 1:nlayers(rasters)){
    master <- mask(master, rasters[[i]])
  }

  # make all values equal 0
  master <- master*0

  return(master)

}
#' @rdname DART_helpers
#' @export
raster_mode <- function(x, na.rm = T) {

  stopifnot(inherits(x, 'Raster'))

  na0 <- ifelse(na.rm, 'no', 'ifany')

  xx <- raster::freq(x, useNA = na0)
  xx <- as.integer(xx[which.max(xx[, 2]), 1])

  return(xx)

}
#' @rdname DART_helpers
#' @export
vec_mode <- function(v, na.rm = T) {

  if (na.rm) v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]

}
#' @rdname DART_helpers
#' @export
save_diagnostic_plots <- function(DART_obj, rast_obj, pdf_fname) {

  if (DART_obj@diagnostic_plots) {
    f0 <- file.path(DART_obj@results_directory, paste0(pdf_fname, '.pdf'))
    if (file.exists(f0)) file.remove(f0)
    pdf(file = f0, width = 8.5, height = 11, onefile = T)
    terra::plot(rast_obj)
    dev.off()
  }

}
#' @rdname DART_helpers
#' @export
save_intermediates <- function(DART_obj, rast_obj, grd_fname) {

  if (DART_obj@save_intermediates) {
    f0 <- file.path(DART_obj@intermediate_directory, grd_fname)
    terra::writeRaster(rast_obj, filename = f0,overwrite = T)
  }

}
