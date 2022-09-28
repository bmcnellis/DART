#' @title Gets DART
#'
#' @description
#'
#' NA
#'
#' @details NA
#'
#' @name GetDART
#' @rdname GetDART
NULL
#' @rdname GetDART
#' @export
GetDART <- function(DART_obj, nchunks = 1) {

  stopifnot(validObject(DART_obj))
  cat('\n** getting DART results')

  # Create iterator vectors
  IDs <- DART_obj@ID
  var <- names(DART_obj@response_variables)

  # Create enclosure-external output dataframe
  out_df <- data.frame(stringsAsFactors = F)

  # Set up progress counter
  p0 <- 0
  pN <- length(IDs) * length(var)
  cat('\n')

  # Set up skip vector for returning skip warns
  skip <- list()

  # Work loop
  for (i in seq_along(IDs)) {
    ii <- IDs[i]

    for (j in seq_along(var)) {
      jj <- var[j]

      p0 <- p0 + 1
      cat('\r\t progress: ', round(p0 / pN), '    ')

      if (DART_obj@synthetic_control) {

        # Get the ith/jth CI object in data.frame form with associated metadata
        ij_CI <- GetCausalImpact(DART_obj = DART_obj, var = jj, ID = ii, chunks = nchunks)

        # Check for bad returns
        if (nrow(ij_CI) == 0) {
          ij_skip <- paste0('var: ', jj, ', ID: ', ii, '; ')
          skip <- c(skip, ij_skip)
          next
        }

        # Slap it on the output dataframe
        if (nrow(out_df) == 0) {
          out_df <- ij_CI
        } else {
          out_df <- rbind(out_df, ij_CI)
        }

      } else if (DART_obj@super_pixel) {

        super_dir <- file.path(DART_obj@results_directory, 'SUPER_PIX')
        super_fls <- list.files(super_dir, pattern = '.rds', full.names = T)

        out_df <- data.frame(stringsAsFactors = F)

        for (k in seq_along(super_fls)) {
          kk <- super_fls[k]

          krds <- readRDS(kk)

          if (k == 1) {
            out_df <- krds
          } else {

            if (ncol(out_df) != ncol(krds)) browser()
            out_df <- rbind(out_df, krds)
          }

        }

      } else {
        stop('implement this')
      }
    }
  }

  if (length(skip > 0)) {
    cat('\nskipped because no files found: ')
    lapply(skip, 'cat')
  }

  row.names(out_df) <- NULL
  write.csv(out_df, file = file.path(DART_obj@results_directory, 'RESULTS.csv'), row.names = F)
  DART_obj@result_data <- out_df

  cat('\n\nfinished GetDart OK')

  return(DART_obj)

}
