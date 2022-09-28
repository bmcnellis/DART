
# R implementation of DART 3.0

# AUT: Travis Nauman
# CTB: Brandon McNellis

# This script generates DART data for testing purposes.

set.seed(1)

library(DART)
library(rgdal)
library(raster)
library(snowfall)

snow_req_packages <- list(
  "plyr", "sp", "rgdal", "snow", "ncdf4", "maptools",
  "rgeos", "stats", "gower"
)
# Packages should be loaded/checked in DESCRIPTION

# Workspaces folders and files for BEM
resultfldr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/results"
DARTvarsfldr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/inputs"
pads_dr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/inputs/Well_pads_TVDART_StudyArea"
pads_layer <- "well_pad_selection_CO_River_watershed_albnad83"

# Input polygons
pads <- rgdal::readOGR(pads_dr, pads_layer) # in right projection
pads$wid <- seq(1:length(pads@data[,1]))
wids <- as.numeric(pads$wid)

# Number of cores
n0 <- 4
# TZ variable, required I guess?
TZ <- ''

# Initial checks
stopifnot(
  dir.exists(resultfldr),
  dir.exists(DARTvarsfldr),
  dir.exists(pads_dr),
  all(unlist(snow_req_packages) %in% installed.packages())
)

parallel_DART_function <- function(w, pads) {

  # Set up DART object:
  parallel_DART <- InitializeDART(DARTvarsfldr, 2L)
  parallel_DART@results_directory <- resultfldr
  parallel_DART@ID <- as.character(w)
  parallel_DART@return_rasters <- FALSE

  # Input polygons:
  input_polygon <- pads[pads$wid %in% as.numeric(parallel_DART@ID), ]

  # Run DART:
  DART_object <- DART::GetDart(parallel_DART, input_polygon)

  DART_df <- DART_object@result_data
  DART_df$ec <- DART_df$ec_mean / 100
  DART_df$ec <- DART_df$ec_min / 100
  DART_df$ec <- DART_df$ec_max / 100

  # Output should be one row of a dataframe
  return(DART_df)
}

# Set up parallel options
raster::rasterOptions(maxmemory = 7e+09, chunksize = 2e+09)
snowfall::sfInit(parallel = TRUE, cpus = n0)
snowfall::sfExport(
  "pads",
  "wids",
  "resultfldr",
  "DARTvarsfldr",
  "parallel_DART_function"
)
snowfall::sfLibrary(DART)
snowfall::sfLibrary(plyr)
snowfall::sfLibrary(raster)
snowfall::sfLibrary(sp)
snowfall::sfLibrary(rgdal)
snowfall::sfLibrary(snow)
snowfall::sfLibrary(ncdf4)
snowfall::sfLibrary(maptools)
snowfall::sfLibrary(rgeos)
snowfall::sfLibrary(stats)
snowfall::sfLibrary(gower)

t1 <- Sys.time()
DART_FORT_quantiles_2 <- snowfall::sfLapply(wids, function(w){
  try(parallel_DART_function(w = w, pads = pads))
  # lapply will return a list of one-row dataframes
})
t2 <- Sys.time()
print(difftime(t2, t1))
snowfall::sfStop()

# Collapse the list of dataframes to a single dataframe
df0 <- data.frame(matrix(ncol = length(DART_FORT_quantiles_2), nrow = 0))
for (i in seq_along(DART_FORT_quantiles_2)) {
  ii <- DART_FORT_quantiles_2[[i]]

  if (inherits(ii, 'try-error')) {
    df0 <- rbind(df0, rep(NA, length(ii)))
  } else {
    df0 <- rbind(df0, ii)
  }
}

DART_FORT_quantiles_2_raw <- DART_FORT_quantiles_2
DART_FORT_quantiles_2 <- df0

# Save result and cleanup
f0 <- file.path(resultfldr, 'DART_FORT_quantiles_2_raw.rda')
save(DART_FORT_quantiles_2_raw, file = f0, overwrite = T)
f1 <- file.path(resultfldr, 'DART_FORT_quantiles_2.rda')
#save(DART_FORT_quantiles_2, file = f1, overwrite = T)
#usethis::use_data(DART_FORT_quantiles_2, overwrite = T)
