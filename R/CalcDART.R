#' @title Calculates DART
#'
#' @description
#'
#' # TODO: somewhere in the CI calculation loop it is silently failing on some
#' #       iterations. This skips the iteration, but very slowly, and re-running
#' #       CalcDART when % finished is less than numValues takes just as long
#' #       but does not add completed pixels. Would be good if this was sped up.
#'
#' End-user interface for calculating DART
#'
#' Replaces dart_fn and get_art
#'
#' # notes from SF:
#'
#'  Generate dart pixels
#'
#'  X = x coordinate in EPSG 5070
#'   Y = y coordinate in EPSG 5070
#'    buffer = distance to buffer target in m
#'    searchRadius = distance to search for matches (m) == DART_obj@search_radius
#'    nControl = number of matches to look for
#'    targetDim = edge length in pixels of target area (1x1, 2x2, 3x3)
#'    x and y within bounding area
#'    buffer >= 0
#'    searchRadius >= 0 & > buffer
#'    nControl > 0
#'    targetDim > 0 & less than MAX
#'
#' @details NA
#'
#' @name CalcDART
#' @rdname CalcDART
NULL
#' @rdname CalcDART
#' @export
CalcDART <- function(DART_obj, x, y = NULL, nControl = 100) {

  # Generate 'treatment' and 'reference' pixel target area polygons.
  target <- GetTarget(DART_obj, x = x, y = y)

  # Get all raster data and crop to target area polygons.
  full_data <- GetRastData(DART_obj, expanded_target = target[['reference']])

  # Mask all the raster data, reference + target
  # Generate the mask file from saved/loaded raster data
  # -OR- reload saved mask file to save time
  mask_fl <- file.path(DART_obj@intermediate_directory, 'DART_MASKED_DATA.grd')
  if (DART_obj@resume && file.exists(mask_fl)) {
    cat('\n** mask data exists; resuming using intermediate')
    masked_data <- raster::stack(mask_fl)
  } else {
    cat('\n** applying masks to full study area')
    masked_data <- ApplyMasks(DART_obj, full_data)
  }

  # Mask reference pixels for target + edge effects
  # Generate the reference file from saved/loaded raster data
  # -OR- reload saved reference file to save time
  ref_fl <- file.path(DART_obj@intermediate_directory, 'DART_MASKED_REF.grd')
  if (DART_obj@resume && file.exists(ref_fl)) {
    cat('\n** reference data exists; resuming using intermediate')
    ref_data <- raster::stack(ref_fl)
  } else {
    cat('\n** generating reference data')
    ref_data <- MakeCandidates(DART_obj, masked_data, target)
  }

  results <- data.frame(stringsAsFactors = F)

  for (i in seq(nrow(x))) {
    xi <- x[i, ]
    iID <- DART_obj@ID[i]

    # Make target data
    itarg <- GetTarget(DART_obj, x = xi, y = y)
    ifl1 <- paste0('DART_MASKED_TARGET_', iID, '.tif')
    ifl1 <- file.path(DART_obj@intermediate_directory, ifl1)

    if (DART_obj@resume && file.exists(ifl1)) {
      cat('\n** target data exists; resuming using intermediate, polygon: ', iID)
      itreat <- raster::stack(ifl1)
    } else {
      cat('\n** generating target data, polygon: ', iID)
      itreat <- MakeTargetData(DART_obj, masked_data, itarg$target, i0 = iID)
    }

    DART::save_diagnostic_plots(DART_obj, itreat, paste0('DART_TARGET_DATA', iID))

    # Get control subset for ith polygon
    ifl2 <- paste0('DART_CONTROL_REF_', iID, '.grd')
    ifl2 <- file.path(DART_obj@intermediate_directory, ifl2)
    if (DART_obj@resume && file.exists(ifl2)) {
      cat('\n** subset data exists; resuming using intermediate, polygon: ', iID)
      iref <- raster::stack(ifl2)
    } else {
      cat('\n** crop reference to target study area, polygon: ', iID)
      iref_area <- ExpandTarget(DART_obj, itarg, to = 'reference')
      iref <- raster::mask(ref_data, iref_area)
      cat('\n** subset target data, polygon: ', iID)
      iref <- ControlSubset(DART_obj, itreat, iref, i0 = iID)
    }

    # Make a master mask for the treatments
    # TODO: should probably do this with everything, up above?
    # TODO: save this file? make it its own function?
    cat('\n** master-masking target data, polygon: ', iID)
    itreat_mask <- DART::masterMask(itreat)
    itreat_sub <- raster::mask(itreat, itreat_mask)
    # Apply the treatment-only mask
    # TODO: make this part of the same masking function above?
    if (length(DART_obj@target_masks) > 0) {
      cat('\n** applying target-specific masks, polygon: ', iID)
      for (j in seq_along(DART_obj@target_masks)) {

        jj <- DART_obj@target_masks[[j]]
        jmask <- raster::raster(file.path(DART_obj@spatial_directory, jj))
        if (!raster::compareRaster(jmask, itreat_sub, stopiffalse = F)) {
          jmask <- raster::resample(jmask, itreat_sub, method = 'ngb')
        }
        itreat_sub <- raster::mask(itreat_sub, jmask)
      }
    }

    DART::save_diagnostic_plots(DART_obj, itreat_sub, paste0('DART_TARGET_PRESUB', iID))

    # Cut down the target pixels to reduce computation time
    ifl4 <- paste0('DART_SUBSAMP_TARGET_', iID, '.grd')
    ifl4 <- file.path(DART_obj@intermediate_directory, ifl4)
    if (DART_obj@resume && file.exists(ifl4)) {
      cat('\n** sub-sample data exists; resuming using intermediate, polygon: ', iID)
      itreat_sub <- raster::stack(ifl4)
    } else {
      if (DART_obj@target_prop < 1) {
        # this doesnt really work - there's just too many pixels
        # subsampling needs to be done in the CI stage
        cat('\n** sub-sampling target pixels, polygon: ', iID)
        itreat_sub <- SubsamplePixels(itreat, DART_obj@seed, DART_obj@target_prop, DART_obj@n_topographic_matches)
      } else {
        cat('\n** skipping sub-sample, polygon: ', iID)
        itreat_sub <- itreat

      }

      raster::writeRaster(itreat_sub, filename = ifl4, format = "raster", overwrite = T)
      cat('\n\twrote target sub-sample OK, polygon: ', iID)
      DART::save_diagnostic_plots(DART_obj, itreat_sub, paste0('DART_TARGET_SUB', iID))

    }

    # Get topographic subset for ith polygon
    ifl5 <- paste0('DART_TOPO_', iID, '.rda')
    ifl5 <- file.path(DART_obj@intermediate_directory, ifl5)
    if (DART_obj@resume && file.exists(ifl5)) {
      cat('\n** subset topo exists; resuming using intermediate, polygon: ', iID)
      topo_obj <- eval(load(ifl5))
      itopo <- get(topo_obj)
      do.call('rm', list(topo_obj)); gc()
    } else {
      cat('\n** subset topographic matches, polygon: ', iID)
      itopo <- TopoSubset(DART_obj, itreat_sub, iref, i0 = iID)
    }

    # Synthetic control analysis step
    if (DART_obj@synthetic_control) {

      # Check to see if there's enough pixels already
      n_done_pixels <- length(list.files(file.path(DART_obj@intermediate_directory, 'CI_PIX')))
      n_avai_pixels <- raster::cellStats(!is.na(itreat_sub[[1]]), 'sum')

      if (n_done_pixels >= n_avai_pixels) {
        cat('\n\tCI temporary directory has enough pixels processed, skipping CI analysis')
      } else if (DART_obj@skip_CI_for_debug) {
        cat('\n\tDEBUG: skipping causal impact analysis')
      } else {
        cat('\n\tpercent pixels finished in CI analysis: ', round(n_done_pixels / n_avai_pixels * 100))

        if (DART_obj@n_cores > 1) {
          cat('\n\tstarting parallel CI analysis')
          DoParallelImpact(DART_obj, ID = iID, target = itreat_sub, control = iref, gower = itopo)
        } else {
          cat('\n\tstarting sequential CI analysis')
          DoCausalImpact(DART_obj, ID = iID, target = itreat_sub, control = iref, gower = itopo)
        }
      }


    } else {

      # BEM: This is where the non-CI calculations can go.

      if (DART_obj@super_pixel) {
        # this might also be the right way to go for non-super pixels

        # probably need some check to make sure the size of itopo is correct
        itopo <- as.data.frame(lapply(itopo, cbind))
        itopo$ID <- rep(iID, nrow(itopo))
        itopo <- within(itopo, rm(index))

        super_dir <- file.path(DART_obj@results_directory, 'SUPER_PIX')
        if (!dir.exists(super_dir)) dir.create(super_dir)
        f_xx <- paste0(super_dir, '/GOWER_', iID, '.rds')

        saveRDS(itopo, file = f_xx)

      } else {

        browser()

        stop('gotta fix this')
      }

    }

  } # end i

  cat('\n\nfinished CalcDart OK')

  # BEM: I removed the results-return portion of this function cause in all liklihood this is going to need to be
  #      heavily parallelized. At the very least, this works well right now as an array batch job on Yeti.
  #
  #      To get CI from the next function, you need:
  #        *Pixel ID/location key: itreat_sub, saved as a raster in ('DART_SUBSAMP_TARGET_', iID, '.grd'),
  #                                where iID is DART_obj@ID[i]
  #        *CI results: GetCausalImpact(DART_obj, target = itreat_sub)

  return(DART_obj)

}
