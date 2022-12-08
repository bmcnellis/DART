#' @title Expand DART target
#'
#' @description
#'
#' Putting this back in until the other can be fixed
#'
#' @details NA
#'
#' @rdname DART_helpers
#' @export
ExpandTarget <- function(DART_obj, target, to) {

  require(raster)
  stopifnot(validObject(DART_obj))

  # TODO: `method` is a placeholder for pixel/polygon methodology
  # TODO: `prime_proj` calculation is slow, can it be stored somehow?

  if (to == 'reference') {
    ref_radius <- DART_obj@search_radius
  } else if (to == 'buffer') {
    ref_radius <- DART_obj@buffer_radius
  } else {
    stop('bad `to` input, must be `reference` or `buffer`')
  }

  prime_proj <- projection(raster(DART_obj@prime_mask))
  expanded_target <- raster::buffer(target, width = ref_radius)
  expanded_target <- sp::spTransform(expanded_target, prime_proj)
  expanded_target$rastval <- 1

  return(expanded_target)

}
