#' @title NA
#'
#' @description NA
#'
#' @details NA
#'
#' @name GIS
#' @rdname GIS
NULL
#' @rdname GIS
#' @export
GetRastData <- function(DART_obj, expanded_target) {

  rast_fl <- file.path(DART_obj@intermediate_directory, 'DART_RASTER_DATA.tif')

  # If the file already exists and 'resume' flag is set, return existing data
  if (DART_obj@resume && file.exists(rast_fl)) {
    rast_data <- terra::rast(rast_fl)
    return(rast_data)
  }

  # Otherwise, generate the raster data:
  #     Import the rasters:
  masks <- if (length(DART_obj@other_masks) > 0) terra::rast(lapply(DART_obj@other_masks, terra::rast))
  conts <- if (length(DART_obj@other_controls) > 0) terra::rast(lapply(DART_obj@other_controls, terra::rast))
  #topos <- terra::rast(lapply(DART_obj@topographic_variables, terra::rast))
  topos <- terra::rast(DART_obj@topographic_variables)
  #     Stack, crop, and mask:
  rast_list <- list(masks, conts, topos)
  rast_data <- terra::rast(rast_list[lengths(rast_list) != 0])
  rast_data <- terra::crop(rast_data, expanded_target)
  rast_data <- terra::mask(rast_data, expanded_target)

  # Save and return:
  DART::save_diagnostic_plots(DART_obj,  rast_data, 'DART_RASTER_DATA')
  DART::save_intermediates(DART_obj,  rast_data, 'DART_RASTER_DATA.tif')
  return( rast_data)

}
#' @rdname GIS
#' @export
ApplyMasks <- function(DART_obj, input_raster_stack) {
  require(raster)

  # BEM: This probably needs a catch for when masking eliminates everything,
  # e.g. when a mask is mis-specified

  if (DART_obj@diagnostic_plots) {
    f0 <- file.path(DART_obj@results_directory, 'DART_UNMASKED_DATA.pdf')
    if (file.exists(f0)) file.remove(f0)
    pdf(file = f0, width = 8.5, height = 11, onefile = T)
    raster::plot(input_raster_stack)
    dev.off()
  }

  if (length(DART_obj@other_masks) > 0) {

    cat('\n applying other masks to candidate pixel mask')
    other_masks <- names(DART_obj@other_masks)
    for(i in seq_along(other_masks)) {
      ii <- input_raster_stack[[other_masks[i]]]
      input_raster_stack <- raster::mask(input_raster_stack, ii)

    }

    cat('\n dropping mask layers')
    for (i in seq_along(other_masks)) {
      ii <- which(names(input_raster_stack) == other_masks[i])
      input_raster_stack <- raster::dropLayer(input_raster_stack, ii)
    }

  } else {
    cat('\n no other masks specified, skipping')
  }

  if (DART_obj@diagnostic_plots) {
    f0 <- file.path(DART_obj@results_directory, 'DART_MASKED_DATA.pdf')
    if (file.exists(f0)) file.remove(f0)
    pdf(file = f0, width = 8.5, height = 11, onefile = T)
    raster::plot(input_raster_stack)
    dev.off()
  }

  if (DART_obj@save_intermediates) {
    f0 <- file.path(DART_obj@intermediate_directory, 'DART_MASKED_DATA.tif')
    raster::writeRaster(input_raster_stack, filename = f0,overwrite = T)
    cat('\n wrote masked data OK')
  }

  cat('\ngot masked data OK\n\n')

  return(input_raster_stack)

}
#' @rdname GIS
#' @export
MakeCandidates <- function(DART_obj, raster_data, target) {

  cat('\n masking outside search radius')
  expanded_target <- ExpandTarget(DART_obj, target, to = 'reference')
  raster_data <- raster::mask(raster_data, expanded_target)

  # Edge-effects should be masked when looking for reference pixels
  cat('\n masking for edge-effects using buffer radius')
  edge_mask <- raster::buffer(target, width = DART_obj@buffer_radius)
  edge_masked <- raster::mask(raster_data, edge_mask, inverse = TRUE)

  if (DART_obj@ref_prop < 1) {
    cat('\n sub-sampling reference pixels, polygon')
    edge_masked <- SubsamplePixels(edge_masked, DART_obj@seed, DART_obj@ref_prop, DART_obj@n_topographic_matches)
  }

  if (all(terra::sum(edge_masked) == 0)) {
    stop('No non-NA candidate pixels in masked rasters (i.e., every pixel masked)')
  }

  if (DART_obj@diagnostic_plots) {
    f0 <- file.path(DART_obj@results_directory, 'DART_MASKED_REF.pdf')
    if (file.exists(f0)) file.remove(f0)
    pdf(file = f0, width = 8.5, height = 11, onefile = T)
    raster::plot(edge_masked)
    dev.off()
  }

  if (DART_obj@save_intermediates) {
    # Need to use a native raster format - GeoTIFF doesnt save the stack layer names
    f0 <- file.path(DART_obj@intermediate_directory, 'DART_MASKED_REF.tif')
    raster::writeRaster(edge_masked, filename = f0,  overwrite = T)
    cat('\n wrote ref area data OK')
  }

  cat('\n reference data OK')
  return(edge_masked)

}
#' @rdname GIS
#' @export
MakeTargetData <- function(DART_obj, masked_data, target, i0) {

  # BEM NOTE: If DART_MASKED_TARGET_ is breaking (all data missing), then make
  #           sure the `not_X_pastures.tif` files are the right format of mask,
  #           or whatever the equivalent would be for another project

  # This line throws errors all the time. Not sure why.
  # Doesnt appear to break anything?
  target_data <- raster::mask(masked_data, target)

  if (DART_obj@save_intermediates) {

    f0 <- paste0('DART_MASKED_TARGET_', i0, '.tif')
    f1 <- file.path(DART_obj@intermediate_directory, f0)
    raster::writeRaster(target_data, filename = f1,  overwrite = T)
    cat('\n wrote target data OK, polygon: ', i0)
  }

  return(target_data)
}
#' @rdname GIS
#' @export
setGeneric("GetTarget", function(DART_obj, x, y) {
  standardGeneric("GetTarget")
})
setMethod("GetTarget", signature(x = "numeric", y = "numeric"), function(DART_obj, x, y) {

  r0 <- DART_obj@target_radius
  # convert to reference crs, create polygon, and buffer
  browser()
  #target <- DART::toE5070(x, y)
  target <- terra::buffer(target, r0)

  return(target)

})
setMethod("GetTarget", signature(x = "SpatVector", y = "NULL"), function(DART_obj, x, y) {

  if (terra::crs(x, proj = T) != DART::reference_CRS) {
    stop('Target has improper CRS. Must match reference CRS. ?DART::reference_CRS')
  }

  ref <- terra::buffer(x, width = DART_obj@search_radius)

  return(list(target = x, reference = ref))

})
