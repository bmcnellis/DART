#' @title S4 definition for DART-class objects
#'
#' @description
#'
#' NA
#'
#' @slot ID Identification/name of this DART object. See details.
#' @slot years Years that response variable is desired. Optional.
#' @slot spatial_directory Directory path containing spatial files. Optional.
#' @slot intermediate_directory Directory to save intermediate files
#' @slot results_directory Directory path for results files. Optional.
#' @slot save_intermediates Logical flag - save intermediate rasters?
#' @slot resume Logical flag - resume at last intermediate?
#' @slot diagnostic_plots Logical flag - plot diagnostic plots for the GIS work?
#' @slot synthetic_control Logical flag - do synthetic control?
#' @slot skip_CI_for_debug Logical flag - skip CI for debug?
#' @slot skip_topo_for_debug Logical flag - skip CI for debug?
#' @slot target_prop Proportion of pixels in each polygon to select for CI
#'               calculations. Must be > 0 and <= 1, where 1 is all pixels
#' @slot ref_prop Proportion of pixels in reference area to subset. Useful for debugging.
#' @slot CI_prop Proportion of eligible pixels to use for CI analysis
#' @slot seed value for set.seed()
#' @slot test_old_ART Logical flag for testing against old ART routine results
#'
#' @slot search_radius Radius in meters to search for reference pixels.
#' @slot target_radius Radius in meters of target area
#' @slot buffer_radius Radius in meters of target-reference buffer zone
#' @slot n_topographic_matches Number of topographic matches to search for.
#'                             Optional. See details.
#' @slot n_cores Number of cores to use for parallelization. Optional.
#' @slot NLCD_filter Numeric of NLCD codes to filter by. Optional.
#'
#' @slot mask Study area mask. SpatVector. Required.
#' @slot nlcd_mask Land cover mask. Filename. Optional.
#' @slot other_masks Other mask layers. List of filenames. Optional.
#' @slot target_masks Masks just for target layer. Optional.
#'
#' @slot other_controls Other control covariates. List of filenames. Optional.
#'
#' @slot response_variables List of response variable filenames
#'
#' @slot result_data Dataframe with result summary data
#'
#' @details
#'
#' NA
#'
#' @import terra
#'
#' @name DART
#' @rdname DART
NULL
#' @rdname DART
#' @export
DART <- setClass(
  'DART',
  slots = list(

    # Meta-data slots
    ID = 'character',
    years = 'numeric',
    spatial_directory = 'character',
    intermediate_directory = 'character',
    results_directory = 'character',
    save_intermediates = 'logical',
    resume = 'logical', # T/F resume from last intermediate product?
    diagnostic_plots = 'logical',
    synthetic_control = 'logical',
    super_pixel = 'logical',


    # Parameter slots
    search_radius = 'numeric', # in meters
    target_radius = 'numeric', # in meters
    buffer_radius = 'numeric', # in meters
    n_topographic_matches = 'numeric',
    n_cores = 'numeric',
    target_prop = 'numeric',
    ref_prop = 'numeric',
    CI_prop = 'numeric',
    NLCD_filter = 'numeric', # NLCD codes to filter by
    CI_init_index = 'integer', # index to start Causal Impact analysis
    confidence = 'numeric', # confidence limit for CI
    seed = 'numeric',

    # Mask slots
    mask = 'SpatVector',
    NLCD_mask = 'character',
    other_masks = 'list',
    target_masks = 'list',

    # Control variable slots
    other_controls = 'list',

    # Topographic variable slots
    topographic_variables = 'list',

    # Landsat variable slots
    landsat_variables = 'list',

    # Response variable slots
    response_variables = 'list',

    # Return variable slots
    result_data = 'data.frame',

    # Internal slots
    skip_CI_for_debug = 'logical',
    skip_topo_for_debug = 'logical',
    test_old_ART = 'logical'

  )
)
#' @export
setValidity('DART', function(object) {
  errors <- character()

  if (FALSE) {
    msg <- paste0('error message')
    errors <- c(errors, msg)
  }

  # returns
  if (length(errors) == 0) {
    TRUE
  } else {
    errors
  }
})
#' @rdname DART
#' @export
setMethod('initialize',
          signature(.Object = 'DART'),
          function (.Object, ...) {
            params <- list(...)

            if ('ID' %in% names(params)) {
              .Object@ID <- params$ID
            } else {
              .Object@ID <- as.character(as.integer(Sys.time()))
            }

            if ('save_intermediates' %in% names(params)) {
              .Object@save_intermediates <- params$save_intermediates
            } else {
              .Object@save_intermediates <- TRUE
            }

            if ('resume' %in% names(params)) {
              .Object@resume <- params$resume
            } else {
              .Object@resume <- TRUE
            }

            if ('diagnostic_plots' %in% names(params)) {
              .Object@diagnostic_plots <- params$diagnostic_plots
            } else {
              .Object@diagnostic_plots <- TRUE
            }

            if ('synthetic_control' %in% names(params)) {
              .Object@synthetic_control <- params$synthetic_control
            } else {
              .Object@synthetic_control <- FALSE
            }

            if ('confidence' %in% names(params)) {
              .Object@confidence <- params$confidence
            } else {
              .Object@confidence <- 90
            }

            if ('CI_init_index' %in% names(params)) {
              .Object@CI_init_index <- params$CI_init_index
            } else {
              .Object@CI_init_index <- 15L
            }

            if ('seed' %in% names(params)) {
              .Object@seed <- params$seed
            } else {
              .Object@seed <- 1
            }

            if ('test_old_ART' %in% names(params)) {
              .Object@test_old_ART <- params$test_old_ART
            } else {
              .Object@test_old_ART <- FALSE
            }

            if ('spatial_directory' %in% names(params)) {
              .Object@spatial_directory <- params$spatial_directory
            } else {
              .Object@spatial_directory <- getwd()
            }

            if ('intermediate_directory' %in% names(params)) {
              .Object@intermediate_directory <- params$intermediate_directory
            } else {
              .Object@intermediate_directory <- getwd()
            }

            if ('results_directory' %in% names(params)) {
              .Object@results_directory <- params$results_directory
            } else {
              .Object@results_directory <- getwd()
            }

            if ('search_radius' %in% names(params)) {
              .Object@search_radius <- params$search_radius
            } else {
              .Object@search_radius <- 2000
            }

            if ('target_radius' %in% names(params)) {
              .Object@target_radius <- params$target_radius
            } else {
              .Object@target_radius <- 30
            }

            if ('buffer_radius' %in% names(params)) {
              .Object@buffer_radius <- params$buffer_radius
            } else {
              .Object@buffer_radius <- 90
            }

            if ('n_topographic_matches' %in% names(params)) {
              .Object@n_topographic_matches <- params$n_topographic_matches
            } else {
              .Object@n_topographic_matches <- 100
            }

            if ('n_cores' %in% names(params)) {
              .Object@n_cores <- params$n_cores
            } else {
              .Object@n_cores <- (parallel::detectCores() - 1)
            }

            if ('target_prop' %in% names(params)) {
              .Object@target_prop <- params$target_prop
            } else {
              .Object@target_prop <- 1
            }

            if ('ref_prop' %in% names(params)) {
              .Object@ref_prop <- params$ref_prop
            } else {
              .Object@ref_prop <- 1
            }

            if ('CI_prop' %in% names(params)) {
              .Object@CI_prop <- params$CI_prop
            } else {
              .Object@CI_prop <- 1
            }

            if ('n_topographic_matches' %in% names(params)) {
              .Object@n_topographic_matches <- params$n_topographic_matches
            } else {
              .Object@n_topographic_matches <- 100
            }

            if ('other_masks' %in% names(params)) {
              .Object@other_masks <- params$other_masks
            } else {
              .Object@other_masks <- list()
            }

            if ('other_controls' %in% names(params)) {
              .Object@other_controls <- params$other_controls
            } else {
              .Object@other_controls <- list()
            }

            if ('skip_CI_for_debug' %in% names(params)) {
              .Object@skip_CI_for_debug <- params$skip_CI_for_debug
            } else {
              .Object@skip_CI_for_debug <- FALSE
            }

            if ('skip_topo_for_debug' %in% names(params)) {
              .Object@skip_topo_for_debug <- params$skip_topo_for_debug
            } else {
              .Object@skip_topo_for_debug <- FALSE
            }

            .Object <- callNextMethod()
            mt <- validObject(.Object)
            if (isTRUE(mt)) {
              return(.Object)
            } else {
              return(mt)
            }
          }
)
