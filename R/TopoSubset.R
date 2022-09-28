#' @title NA
#'
#' @description NA
#'
#' @details NA
#'
#' @rdname TopoSubset
#' @export
TopoSubset <- function(DART_obj, treatment, reference, i0) {

  # treatment is itreat_sub: paste0('DART_SUBSAMP_TARGET_', iID, '.grd')
  # reference is iref: paste0('DART_CONTROL_REF_', iID, '.grd')

  # BEM: This is different than TopographicSubset because it exactly
  #      matches based on PSCS and EC. Can be expanded to other variables..

  if (DART_obj@skip_topo_for_debug) {
    cat('\n\tskipping similarity index, polygon: ', i0)
    return(DART::toposim_debug)
  } else {
    cat('\n\tgetting similarity index, polygon: ', i0)
  }

  require(gower, quietly = T)
  stopifnot(validObject(DART_obj))

  cat('\n\tgetting topographic subset, polygon: ', i0)

  # Get the names for the relevant columns
  PSCS_name <- tools::file_path_sans_ext(basename(DART_obj@PSCS_control))
  EC_name <- tools::file_path_sans_ext(basename(DART_obj@EC_control))
  toponames <- names(DART_obj@topographic_variables)
  other_names <- names(DART_obj@other_controls)
  all_names <- c(toponames, PSCS_name, EC_name, other_names)

  # Convert to SPDF for pixel selection
  spdf_trt <- treatment[[all_names]]
  spdf_trt <- as(spdf_trt, 'SpatialPixelsDataFrame')
  spdf_ref <- reference[[all_names]]
  spdf_ref <- as(spdf_ref, 'SpatialPixelsDataFrame')

  # Check for missing values in selected pixels. If present, report:
  spdf_trt_NA <- apply(spdf_trt@data, 1, function(x) sum(is.na(x)))
  cat('\n\t\t# of (treatment) topographic pixels with missing values: ', sum(spdf_trt_NA))
  cat('\n\t\t% of (treatment) topographic pixels with missing values: ', round(sum(spdf_trt_NA) / nrow(spdf_trt) * 100))
  spdf_ref_NA <- apply(spdf_ref@data, 1, function(x) sum(is.na(x)))
  cat('\n\t\t# of (reference) topographic pixels with missing values: ', sum(spdf_ref_NA))
  cat('\n\t\t% of (reference) topographic pixels with missing values: ', round(sum(spdf_ref_NA) / nrow(spdf_ref) * 100))
  cat('\n\t\tdropping pixels with missing values')
  if (length(which(spdf_trt_NA > 0)) > 0) spdf_trt <- spdf_trt[-which(spdf_trt_NA > 0), ]
  if (length(which(spdf_ref_NA > 0)) > 0) spdf_ref <- spdf_ref[-which(spdf_ref_NA > 0), ]

  cat('\n\t\tpulling pixel data from spatial object')
  dat1 <- spdf_trt@data
  rownames(dat1) <- apply(coordinates(spdf_trt), 1, FUN = toString)
  dat2 <- spdf_ref@data
  rownames(dat2) <- apply(coordinates(spdf_ref), 1, FUN = toString)

  if (DART_obj@super_pixel) {
    # Super-pixel method
    cat('\n\t\trunning SuperPixel method')
    PSCS_col <- tools::file_path_sans_ext(basename(DART_obj@PSCS_control))

    # Need to take mode of PSCS, median of all else, and mode of severity
    dat0 <- as.data.frame(lapply(dat1, median))
    dat0[PSCS_col] <- DART::vec_mode(dat1[PSCS_col])

    for (i in seq_along(DART_obj@other_controls)) {
      ii <- names(DART_obj@other_controls)[i]

      # TODO: this should maybe be not mode? or add a control?
      dat0[, ii] <- DART::vec_mode(dat1[, ii])
    }

    dat1 <- dat0

  }

  if (any(nrow(dat1) == 0, nrow(dat2) == 0)) {
    stop("lost all rows in treatment and/or reference data in ExactTopoSubset(), cant run gowers")
  }

  cat('\n\t\tstarting gowers dopar loop\n\n')
  # Loop over the treatment pixels, get gower_topn, and add to results
  require(doParallel, quietly = T)
  require(foreach, quietly = T)
  cl0 <- parallel::makeCluster(DART_obj@n_cores)
  doParallel::registerDoParallel(cl0)

  #for (i in seq(nrow(treatment@data))) {
  toposim_list <- foreach(i = seq(nrow(dat1)),
                          .export = ls(environment()),
                          .errorhandling = 'pass', .packages = c('gower'), .verbose = T
  ) %dopar% {

    # Get the treatment pixel
    idat1 <- dat1[i, ]

    # Subset the reference pixels for PSCS
    if (DART_obj@exact_PSCS_match) {
      idat2 <- dat2[which(dat2[[PSCS_name]] %in% idat1[[PSCS_name]]), ]
    } else {
      idat2 <- dat2
    }
    # Subset the reference pixels for EC
    idat2 <- idat2[which(idat2[[EC_name]] >= (idat1[[EC_name]] * DART_obj@EC_min)), ]
    idat2 <- idat2[which(idat2[[EC_name]] <= (idat1[[EC_name]] * DART_obj@EC_max)), ]

    # Subset the reference pixels for other controls
    for (j in seq_along(DART_obj@other_controls)) {
      # Loop through the names of the other control variables...
      jj <- names(DART_obj@other_controls)[j]
      # Find the relevant value...
      jval <- idat1[, jj]

      # And match it exactly. Needs some other functionality for min/max
      # or whatever.
      idat2 <- idat2[which(idat2[, jj] == jval), ]
    }

    # Drop control columns
    idat1 <- idat1[, -which(colnames(idat1) %in% c(PSCS_name, EC_name))]
    idat2 <- idat2[, -which(colnames(idat2) %in% c(PSCS_name, EC_name))]
    if (length(DART_obj@other_controls) > 0) {
      idat1 <- idat1[, -which(colnames(idat1) %in% names(DART_obj@other_controls))]
      idat2 <- idat2[, -which(colnames(idat2) %in% names(DART_obj@other_controls))]
    }

    if (any(nrow(idat1) == 0, nrow(idat2) == 0)) {
      stop('missing topographic variables from rasters@data, this will
          cause gower::topn to crash R - DART_core.R/TopographicSubset()')
    }

    # Run gower
    itopo <- gower::gower_topn(
      x = idat1,
      y = idat2,
      n = DART_obj@n_topographic_matches,
      nthread = 1
    )

    # Add reference coordinates to gower's object
    itopo$coord <- row.names(idat2)[itopo$index]

    return(itopo)

  }

  # BEM 7/26/21
  # RunTopoSimRoutine() is TN's old ART code, refactored in a function below
  # I dont know why it runs the way it does, but this can test to see if the results are the same.
  if (DART_obj@test_old_ART) {
    toposim_ART <- DART::RunTopoSimRoutine(toposim_list = toposim_list)
    ART_pix <- lapply(toposim_ART[[1]], function(x) suppressWarnings(spdf_ref[as.numeric(na.omit(x)), ]))
    ART_avedist <- lapply(toposim_ART[[3]], function(x) mean(x[!is.infinite(x)], na.rm = T))

    for (i in seq_along(toposim_ART)) {

      ART_ind <- as.numeric(na.omit(toposim_ART[[1]][[i]]))
      ART_ind <- ART_ind[which(ART_ind > 0)]
      gow_ind <- toposim_list[[i]]$index
      gow_ind <- gow_ind[which(gow_ind > 0)]

      if (any(
        !all(ART_ind %in% gow_ind),
        !all(gow_ind %in% ART_ind)
      )) {
        stop('ART pixel selection routine check failed')
      }
    }
  }

  # BEM: This is a horrific kludge and should be fixed...
  # Problem: If any elements of `toposim_list` are NULL, then the `do.call()` will ignore them
  # for `index` and `distance`, but not for `coord`. This will cause the `ncol` of
  # `toposim$coord` to be greater than either of the other variables, and it will
  # fail the `stopifnot` check before the `cellFromXY` call.
  which_null_index <- which(sapply(toposim_list, function(x) is.null(x$index)))
  which_null_distance <- which(sapply(toposim_list, function(x) is.null(x$distance)))
  if (all(length(which_null_index) > 0, length(which_null_index) > 0)) {
    if (identical(which_null_index, which_null_distance)) {
      # use this index to shorten toposim$coord the appropriate amount, below
      drop_coord_index <- which_null_index
    } else {
      # No idea what the problem is if this is the case, this really should never happen
      stop("NULL present in both gower's index and gower's distance, but they are not the same list entires")
    }
  } else {
    # This is here to prevent a weird non-informative error on an `ifelse` check below
    drop_coord_index <- integer()
  }

  # Smash it all together
  toposim <- list(index = integer(), distance = numeric(), coord = character())
  toposim$index <- do.call('cbind', lapply(toposim_list, function(x) x$index))
  toposim$distance <- do.call('cbind', lapply(toposim_list, function(x) x$distance))
  nmax_row <- ifelse(is.null(nrow(toposim$index)), length(toposim$index), nrow(toposim$index))
  toposim$coord <- do.call('cbind', lapply(toposim_list, function(x) {
    as.matrix(c(x$coord, rep(NA, nmax_row - length(x$coord))))
  }))
  # Check to make sure the column lengths for toposim line up - if not, apply fix from above
  toposim_n_cols <- sapply(toposim, ncol)
  if (toposim_n_cols['coord'] > toposim_n_cols['index']) {
    toposim_col_diff <- toposim_n_cols['coord'] - toposim_n_cols['index']
    if (toposim_col_diff == length(drop_coord_index)) {
      toposim$coord <- toposim$coord[, -drop_coord_index]
    }
  }
  # Check again - fail if it fails this time
  stopifnot(length(unique(sapply(toposim, nrow))) == 1, length(unique(sapply(toposim, ncol))) == 1)
  # BEM NOTE: `toposim` needs the target pixel IDs in the reference pixel outputs to be
  # able to extract for CI
  # These are the pixel IDs in `itreat_sub`, which here is `treatment`
  # to pass to CI, set the column names of the `gower` object to be the treatment pixel IDs
  pix_ID <- raster::cellFromXY(treatment, apply(t(do.call('cbind', strsplit(rownames(dat1), ', '))), c(1, 2), as.numeric))
  # pix_ID is the wrong length if there was NULLs returned by gower's, need to correct
  if (length(drop_coord_index) > 0) pix_ID <- pix_ID[-drop_coord_index]
  toposim <- lapply(toposim, function(x) structure(x, dimnames = list(NULL, pix_ID)))

  # Check for bad entries in super-pixel method
  # the non-super pixel version of this error handling is done in synthetic_control, but should
  # probably all be in one place i guess
  if (DART_obj@super_pixel) {
    stop('hasnt been qced at all')
    toposim$index <- toposim$index[which(toposim$index > 0)]
    toposim$distance <- toposim$distance[!is.infinite(toposim$distance)]
    toposim$coord <- toposim$coord[!is.na(toposim$coord)]
    stopifnot(length(toposim$index) == length(toposim$distance), length(toposim$distance) == length(toposim$coord))
  }

  # Save outputs and return
  if (DART_obj@save_intermediates) {
    fl <- paste0('DART_TOPO_', i0, '.rda')
    fl <- file.path(DART_obj@intermediate_directory, fl)
    save(toposim, file = fl)
    cat('\n\twrote topo subset OK, polygon: ', i0)
  }

  cat('\n\tgot topographic index OK, polygon: ', i0)

  return(toposim)

}
#' @rdname TopoSubset
#' @export
RunTopoSimRoutine <- function(toposim_list, reference_pixels) {

  out_rows <- vector('list', length(toposim_list))
  out_ind <- vector('list', length(toposim_list))
  out_dist <- vector('list', length(toposim_list))

  for (i in seq_along(toposim_list)) {
    ii <- toposim_list[[i]]

    ii_f1 <- as.data.frame(factor(ii$index[1, ], levels = unique(ii$index[1, ])))
    colnames(ii_f1) <- 'rows'
    ii_f1 <- plyr::count(ii_f1, 'rows')
    ii_f1$rows <- factor(ii_f1$rows, levels = ii_f1$rows[order(ii_f1$freq, decreasing = T)])

    ii_rows <- as.character(ii_f1$rows)
    ii_rows <- unique(ii_rows)

    ii_togo <- 100 - length(ii_rows)

    ii_f25 <- as.data.frame(as.factor(ii$index[2:25, ]))
    colnames(ii_f25) <- 'rows'
    ii_f25 <- plyr::count(ii_f25, 'rows')
    ii_f25$rows <- factor(ii_f25$rows, levels = ii_f25$rows[order(ii_f25$freq, decreasing = T)])

    if (length(ii_f25$rows) < ii_togo) {
      ii_rows <- append(ii_rows, as.character(ii_f25$rows))
      ii_rows <- unique(ii_rows)
      ii_togo <- 100 - length(ii_rows)
    } else {
      ii_rows <- append(ii_rows, names(summary(ii_f25$rows)[1:ii_togo]))
      ii_rows <- unique(ii_rows)
      ii_togo <- 100 - length(ii_rows)
    } # end ifelse

    if (length(ii_togo) < 100) {

      ii_f100 <- as.data.frame(as.factor(ii$index[26:100, ]))
      colnames(ii_f100) <- 'rows'
      ii_f100 <- plyr::count(ii_f100, 'rows')
      ii_f100$rows <- factor(ii_f100$rows, levels = ii_f100$rows[order(ii_f100$freq, decreasing = T)])

      ii_rows <- append(ii_rows, names(summary(ii_f100$rows)[1:ii_togo]))
      ii_rows <- unique(ii_rows)

      ii_togo <- 100 - length(ii_rows)

      while_break_count <- 0L

      while(ii_togo > 0) {

        while_break_count <- while_break_count + 1L # stops from running forever

        ii_f100 <- subset(ii_f100, !(as.character(rows) %in% ii_rows))
        colnames(ii_f100) <- 'rows'
        ii_f100 <- plyr::count(ii_f100, 'rows')
        ii_f100$rows <- factor(ii_f100$rows, levels = ii_f100$rows[order(ii_f100$freq, decreasing = T)])

        ii_rows <- append(ii_rows, names(summary(ii_f100$rows)[1:ii_togo]))
        ii_rows <- unique(ii_rows)
        ii_togo <- 100 - length(ii_rows)

        if (while_break_count > 1000L) {
          break
        }
      } # end while

    } else {
      next
    } # end ifelse

    out_rows[[i]] <- ii_rows
    out_ind[[i]] <- which(ii$index %in% ii_rows, arr.ind = T)
    out_dist[[i]] <- ii$distance[out_ind[[i]]]

  } # end for

  return(list(out_rows, out_ind, out_dist))

}
