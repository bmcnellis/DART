#' @title Synthetic control functions
#'
#' @description
#'
#' These functions run the synethetic control routines for DART-class
#' objects.
#'
#' They take place AFTER the ART-pixel selection
#'
#' @details NA
#'
#' @author Steve Fick
#' @author Brandon McNellis
#'
#' @name synthetic_control
#' @rdname synthetic_control
NULL
#' @rdname synthetic_control
#' @export
GetCausalImpact <- function(DART_obj, var, ID, chunks = 1) {

  stopifnot(validObject(DART_obj))

  # Get the CI intermediate filenames from the intermediate directory
  CI_inter_dir <- file.path(DART_obj@intermediate_directory, 'CI_PIX')
  if (!dir.exists(CI_inter_dir)) stop('cant find intermediate CI files')
  CI_fls <- list.files(CI_inter_dir, full.names = T)
  CI_fls <- CI_fls[grep(pattern = var, x = CI_fls)]
  CI_fls <- CI_fls[grep(pattern = ID, x = CI_fls)]

  if (length(CI_fls) == 0) {
    return(data.frame())
  }

  # The `lapply` strategy below used WAY too much memory for large
  # amounts of pixels. The `chunk` parameter splits it up and processes
  # it a little bit at a time. If its still failing, use more chunks.
  #
  # This can probably be automatic if need be, depending on the number of
  # files as well as available memory.
  CI_chunks <- split(CI_fls, cut(seq_along(CI_fls), chunks, labels = F))
  ser_df <- data.frame()

  for (i in seq_along(CI_chunks)) {

    ii <- CI_chunks[i]
    i_CI_fls <- as.character(unlist(ii))

    # Get the pixel values for each var/ID combination...
    # ... by first loading the models from the .rda intermediates and giving them pixel names...
    i_mod <- lapply(i_CI_fls, function(x) get(eval(load(x))))
    i_pix <- sapply(strsplit(tools::file_path_sans_ext(basename(i_CI_fls)), '_'), function(x) x[3])
    i_ser <- lapply(i_mod, function(x) as.data.frame(x$series))
    names(i_ser) <- i_pix
    # ... then vectorized-iterate over the series and add metadata...
    i_ser <- lapply(i_ser, function(x) cbind(x, ID = rep(var, nrow(x))))
    i_ser <- lapply(i_ser, function(x) cbind(x, var = rep(ID, nrow(x))))
    i_ser <- lapply(i_ser, function(x) cbind(x, year = DART_obj@years[as.numeric(row.names(x))]))
    # ... and smash them together, with pixel ID taken from row names
    i_ser_df <- do.call('rbind', i_ser)
    i_ser_df$pixel <- sapply(strsplit(row.names(i_ser_df), '\\.'), function(x) x[1])

    if (i == 1) {
      ser_df <- i_ser_df
    } else {
      ser_df <- rbind(ser_df, i_ser_df)
    }

  } # end chunker

  if (length(CI_fls) != length(unique(ser_df$pixel))) {
    warning(paste0(
      "CI data data.frame does not match input CI files, length files and unique pixels: ",
      length(CI_fls), length(unique(ser_df$pixel))
    ))

  }

  return(ser_df)

}
#' @rdname synthetic_control
#' @export
DoParallelImpact <- function(DART_obj, ID, target, control, gower) {

  stopifnot(
    validObject(DART_obj)
  )

  init0 <- DART_obj@CI_init_index
  cat('\n** starting causal impact analysis')

  pixel_ids <- which(!is.na(target[][, 1]))
  # if there was NULL in gower's, pixel ids won't match
  if (ncol(gower$coord != length(pixel_ids))) {
    pixel_ids <- pixel_ids[pixel_ids %in% colnames(gower$coord)]
  }

  # Subsample the available target pixels to speed up computation time
  if (DART_obj@CI_prop < 1) {
    pixel_ids <- sample(pixel_ids[order(pixel_ids)], ceiling(length(pixel_ids) * DART_obj@CI_prop), replace = F)
    gower <- lapply(gower, function(x) x[, which(colnames(x) %in% pixel_ids)])
  }

  for (i in seq_along(DART_obj@response_variables)) {
    ii <- DART_obj@response_variables[[i]]
    ii0 <- basename(ii)
    i0 <- tools::file_path_sans_ext(basename(ii))
    itag <- names(DART_obj@response_variables)[i]

    cat(paste0('\n\tgetting causal impact data for response: ', ii0))

    iresp <- raster::stack(ii)

    cat('\n\trunning causal impact analysis for response: ', ii0)
    cat('\n\nPARALLEL OUTPUT: \n\n')

    CI_inter_dir <- file.path(DART_obj@intermediate_directory, 'CI_PIX')
    if (!dir.exists(CI_inter_dir)) dir.create(CI_inter_dir)

    require(doParallel, quietly = T)
    require(foreach, quietly = T)
    cl0 <- parallel::makeCluster(DART_obj@n_cores)
    doParallel::registerDoParallel(cl0)

    foreach(j = seq_along(pixel_ids),
            .export = ls(environment()),
            .errorhandling = 'pass', .packages = c('CausalImpact'), .verbose = T
    ) %dopar% {

      # Set up file for each pixel's model output
      set.seed(DART_obj@seed)
      jj <- pixel_ids[j]
      jfl <- file.path(CI_inter_dir, paste0('CI_PIX_', jj, '_VAR_', itag, '_ID_', ID, '.rda'))
      if (file.exists(jfl)) next
      jgow <- lapply(gower, function(x) x[, which(colnames(x) == jj)])

      cat('\n\t\tpixel: ', jj)

      # Construct CI input data. First column must be response variable, while
      # other columns must be the covariates (i.e., control pixels). Rows
      # are the ordered time-series of observations.

      jdat <- matrix(data = NA, ncol = nrow(gower$coord) + 1, nrow = nlayers(iresp))
      # Extract the treatment-pixel response variable time-series
      cat('\n\t\t\textract treatment-pixel response time-series')
      jdat[, 1] <- as.numeric(raster::extract(iresp, raster::xyFromCell(target, jj)))
      # Extract the reference-pixel response variable time-series
      cat('\n\t\t\textract reference-pixel response time-series')
      jdat[, -1] <- t(raster::extract(iresp, apply(do.call('rbind', strsplit(jgow$coord, ', ')), c(1, 2), as.numeric)))

      # Remove missing values from input data, otherwise CausalImpact fails
      cat('\n\t\t\tremove missing values')
      m0 <- sapply(jdat, function(x) sum(is.na(x)))
      if (m0[1] > 1) {
        warning(paste0('missing treatment data for pixel ', jj), '; skipping')
        next
      } else if (any(m0 > 0)) {
        jdat <- jdat[, !as.logical(m0)]
      }

      cat('\\n\t\t\tget CausalImpact... ')
      j_ci <- CausalImpact::CausalImpact(
        jdat,
        pre.period = c(1, init0),
        post.period = c((init0 + 1), nrow(jdat)),
        alpha = (1 - (DART_obj@confidence / 100))
      )
      cat('ok')
      # to improve storage, see this SO:
      # https://stackoverflow.com/questions/1395115/storing-r-objects-in-a-relational-database
      cat('\n\t\tsave CausalImpact...')
      save(j_ci, file = jfl)
      cat('ok\n')
    }

    cat('\n\nEND PARALLEL, RESUME DART:\n\n')
    parallel::stopCluster(cl0)
    cat('\n\tgot parallel impact OK, variable:', ii)

  } # end i

  cat('\n\tprocessed CI ok')

  invisible()

}
#' @rdname synthetic_control
#' @export
DoCausalImpact <- function(DART_obj, ID, target, control, gower) {

  stopifnot(
    validObject(DART_obj)
  )

  init0 <- DART_obj@CI_init_index
  cat('\n** starting causal impact analysis')

  pixel_ids <- which(!is.na(target[][, 1]))

  # Subsample the available target pixels to speed up computation time
  if (DART_obj@CI_prop < 1) {
    pixel_ids <- sample(pixel_ids[order(pixel_ids)], ceiling(length(pixel_ids) * DART_obj@CI_prop), replace = F)
    gower <- lapply(gower, function(x) x[, which(colnames(x) %in% pixel_ids)])
  }

  for (i in seq_along(DART_obj@response_variables)) {
    ii <- DART_obj@response_variables[[i]]
    ii0 <- basename(ii)
    i0 <- tools::file_path_sans_ext(basename(ii))
    itag <- names(DART_obj@response_variables)[i]

    cat(paste0('\n\tgetting causal impact data for response: ', ii0))

    iresp <- raster::stack(ii)

    cat('\n\trunning causal impact analysis for response: ', ii0)

    CI_inter_dir <- file.path(DART_obj@intermediate_directory, 'CI_PIX')
    if (!dir.exists(CI_inter_dir)) dir.create(CI_inter_dir)

    for (j in seq_along(pixel_ids)) {

      # Set up file for each pixel's model output
      set.seed(DART_obj@seed)
      jj <- pixel_ids[j]
      jfl <- file.path(CI_inter_dir, paste0('CI_PIX_', jj, '_VAR_', itag, '_ID_', ID, '.rda'))
      if (file.exists(jfl)) next
      jgow <- lapply(gower, function(x) x[, which(colnames(x) == jj)])

      cat('\n\t\tpixel: ', jj)

      # Construct CI input data. First column must be response variable, while
      # other columns must be the covariates (i.e., control pixels). Rows
      # are the ordered time-series of observations.

      jdat <- matrix(data = NA, ncol = nrow(gower$coord) + 1, nrow = nlayers(iresp))
      # Extract the treatment-pixel response variable time-series
      cat('\n\t\t\textract treatment-pixel response time-series')
      jdat[, 1] <- as.numeric(raster::extract(iresp, raster::xyFromCell(target, jj)))
      # Extract the reference-pixel response variable time-series
      cat('\n\t\t\textract reference-pixel response time-series')
      jdat[, -1] <- t(raster::extract(iresp, apply(do.call('rbind', strsplit(jgow$coord, ', ')), c(1, 2), as.numeric)))

      # Remove missing values from input data, otherwise CausalImpact fails
      cat('\n\t\t\tremove missing values')
      m0 <- sapply(jdat, function(x) sum(is.na(x)))
      if (m0[1] > 1) {
        warning(paste0('missing treatment data for pixel ', jj), '; skipping')
        next
      } else if (any(m0 > 0)) {
        jdat <- jdat[, !as.logical(m0)]
      }

      cat('\\n\t\t\tget CausalImpact... ')
      j_ci <- CausalImpact::CausalImpact(
        jdat,
        pre.period = c(1, init0),
        post.period = c((init0 + 1), nrow(jdat)),
        alpha = (1 - (DART_obj@confidence / 100))
      )
      cat('ok')
      # to improve storage, see this SO:
      # https://stackoverflow.com/questions/1395115/storing-r-objects-in-a-relational-database
      cat('\n\t\tsave CausalImpact...')
      save(j_ci, file = jfl)
      cat('ok\n')

    }

    cat('\n\tgot all CausalImpact OK, variable:', ii)

  } # end i

  cat('\n\tprocessed CI ok')

  invisible()

}
#' @rdname synthetic_control
GS <- function(oi){
  require(gsynth)
  g <- gsynth(y ~ D, data = na.omit(oi), index = c("id","time"), force = "two-way", CV = TRUE,
              r = c(0, 2), se = FALSE, inference = "parametric", nboots = 200, parallel = FALSE)

  input <- sort(unique(oi$time))
  outpt <- rep(NA, length(input))
  ef <- g$att
  outpt[ match(names(ef), input) ] <- ef


  cumul <- outpt
  cumul[input < 1] <- 0
  cumul <- cumsum(cumul)


  list( effect = data.table( 'point.effect' =outpt, 'point.effect.upper' = NA, point.effect.lower= NA,
                             cum.effect = cumul, cum.effect.upper = NA,
                             cum.effect.lower = NA))
}
