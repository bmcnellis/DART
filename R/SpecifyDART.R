#' @title Function to specify a DART object for analysis
#'
#' @description
#'
#' This is a wrapper function that initializes a `DART-class` object using parameters
#' inputs from the user. If an inappropriate parameter is passed, an informative warning is given.
#'
#' @param ID Identification/name of this DART object. See details.
#'           BEM: maybe make it so ID is a column name in the SPDF???
#' @param mask Study area mask. SpatVector.
#' @param response List of response variable filenames.
#' @param other_masks Character vector of other mask filenames to apply
#' @param other_controls List of other control variable filenames.
#' @param topo_variables List of topographic variable filenames.
#'
#' @param years Years that response variable is desired
#' @param spatial_directory Directory path containing spatial files.
#' @param results_directory Directory path for results files.
#' @param intermediate_directory Directory for intermediate files
#'
#' @param use_synthetic_control Should the synthetic control method be used? T/F
#'                              Will fail if years < 6 or no intervention year set.
#'
#' @param search_radius Radius in meters to search for reference pixels.
#' @param target_radius Radius in meters of target area
#' @param buffer_radius Radius in meters of target-reference buffer zone
#' @param n_matches Number of topographic matches to search for.
#'                             Optional. See details.
#' @param SC_intervention Year of synthetic control intervention.
#'                        Required if use_synthetic_control is TRUE
#' @param fresh if TRUE, remove all files in results and intermediate directory before run
#'
#' @details
#'
#' NA
#'
#' @name SpecifyDART
#' @rdname SpecifyDART
NULL
#' @rdname SpecifyDART
#' @export
SpecifyDART <- function(ID, mask, response,
                        other_masks = NULL, other_controls = list(), topo_variables = list(),
                        target_masks = list(),
                        years = c(1984:2018),
                        spatial_directory = getwd(), results_directory = getwd(), intermediate_directory = getwd(),
                        use_synthetic_control = F, SC_intervention = NULL,
                        super_pixel = T, fresh = F,
                        search_radius = 2000, target_radius = numeric(), buffer_radius = 90, n_matches = 100) {

  out_DART <- new('DART')

  # Directory checks
  if (!dir.exists(spatial_directory)) {
    # This -should- have all the rasters, so it needs to exist
    stop('Spatial directory does not exist.')
  }
  if (!dir.exists(results_directory)) {
    dir.create(results_directory)
  }
  if (!dir.exists(intermediate_directory)) {
    dir.create(intermediate_directory)
  }
  if (fresh) {
    file.remove(list.files(results_directory, full.names = T))
    file.remove(list.files(intermediate_directory, full.names = T))
  }

  # Check 'mask' parameter
  if (!inherits(mask, 'SpatVector')) {
    stop('`mask` must be a SpatVector-class object')
  }

  # Check to see if raster filenames exist
  response0 <- lapply(response, function(x) does_raster_exist(x, spatial_directory))

  if (!is.null(other_masks)) {
    if (!is.null(names(other_masks)) | any(names(other_masks) == '')) {
      # this just has an error should probably fix
      other0 <- lapply(other_masks, function(x) does_raster_exist(x, spatial_directory))
    } else {
      stop('other masks must be a named vector - no empty names allowed')
    }
  } else {
    other0 <- list()
  }

  # Check to see if response variables are formatted correctly
  if (length(names(response)) == 0) {
    stop('response variables must be a named vector - no empty names allowed')
  }

  # Check to see if other mask files exist
  out_DART@other_controls <- other_controls
  missing_other <- sapply(out_DART@other_controls, file.exists)
  if (!(all(missing_other))) {
    message('cant find additional control variables: ')
    print(names(missing_other[which(missing_other == 'FALSE')]))
  }

  # Check to see if the synthetic control parameters were set correctly
  if (!is.logical(use_synthetic_control)) {
    stop('use_synthetic_control must be TRUE/FALSE')
  } else {
    if (use_synthetic_control) {
      if (!is.numeric(SC_intervention)) stop('synthetic control intervention must be numeric')
      if (length(years) < 6) stop('must have more than 6 years of data to use synthetic control')
      if (!(SC_intervention %in% years)) stop('synthetic control intervention must be present in provided years')
      if ((SC_intervention - 6) < min(years)) stop('need at least 6 years of pre-intervention data for synthetic control')
    }
  }

  # Check to make sure the target radius is compatible with the super_pixel flag
  if (super_pixel) {
    if (length(target_radius) != 1) {
      stop("target radius malformed for super pixel method")
    }
  } else {
    if (length(target_radius) != 0) {
      stop("cant have a target radius without using super pixel method")
    }
  }

  # If slots change, this will break. But, it can be fixed without breaking
  # testing scripts.

  # Available slots:
  out_DART@ID <- ID
  out_DART@years <- years
  out_DART@spatial_directory <- spatial_directory
  out_DART@results_directory <- results_directory
  out_DART@intermediate_directory <- intermediate_directory
  out_DART@search_radius <- search_radius
  out_DART@target_radius <- target_radius
  out_DART@buffer_radius <- buffer_radius
  out_DART@n_topographic_matches <- n_matches
  out_DART@mask <- mask
  out_DART@response_variables <- response0
  out_DART@synthetic_control <- use_synthetic_control
  out_DART@CI_init_index <- which(years == SC_intervention)
  out_DART@target_masks <- target_masks
  out_DART@super_pixel <- super_pixel
  #out_DART@other_controls <- other_controls

  # Defaults/not implemented yet:
  out_DART@target_prop <- 0.1
  out_DART@seed <- 1

  # Response variables

  # Topo variables:
  if (length(topo_variables) == 0) {
    #topo_list <- as.list(file.path(spatial_directory, paste0(DART::GetDefaultTopoVars(), '.tif')))
    topo_list <- file.path(spatial_directory, paste0(DART::GetDefaultTopoVars(), '.tif'))
    names(topo_list) <- DART::GetDefaultTopoVars()
    out_DART@topographic_variables <- topo_list
  } else {
    out_DART@topographic_variables <- topo_variables
  }

  missing_topo <- sapply(out_DART@topographic_variables, file.exists)
  if (!(all(missing_topo))) {
    message('cant find topographic variables: ')
    print(names(missing_topo[which(missing_topo == 'FALSE')]))
  }

  stopifnot(validObject(out_DART))

  return(out_DART)

}
