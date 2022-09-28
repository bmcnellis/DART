
# R implementation of DART 2.0
# polygon edition

# AUT: Travis Nauman
# CTB: Brandon McNellis

# This script generates DART data for testing purposes.

#proj_dir <- getwd()
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

# Sourcing is done in tests/testthat/helper-FORT.R
# Automatic tets should pick it up from there
source("C:/Users/bmcnellis/Documents/R_workspace_USGS/DART/tests/testthat/helper-FORT.R")


# Workspaces folders and files for BEM
resultfldr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/results"
resultpixfldr <- "C://Users/bmcnellis/Documents/DART/test_data/FORT/results"
DARTvarsfldr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/inputs"
Topovarsfldr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/inputs"
pads_dr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/inputs/Well_pads_TVDART_StudyArea"
pads_layer <- "well_pad_selection_CO_River_watershed_albnad83"

# Input polygons
pads <- rgdal::readOGR(pads_dr, pads_layer) # in right projection
pads$wid <- seq(1:length(pads@data[,1]))
wids <- as.numeric(pads$wid)

# Other parameters
# External polygon identifier
ext_id <- "fort_uid"
# Name of response variable
respname <- "soil cover"
# Neighborhood radius (m)
rad <- 2000
# Number of cores
n0 <- 4

# Initial checks
stopifnot(
  dir.exists(resultfldr),
  dir.exists(resultpixfldr),
  dir.exists(DARTvarsfldr),
  dir.exists(Topovarsfldr),
  dir.exists(pads_dr),
  all(unlist(snow_req_packages) %in% installed.packages())
)

# Single wids testing
#test0 <- dart_fn(wids[1], pads = pads)

# Set up parallel options
raster::rasterOptions(maxmemory = 7e+09, chunksize = 2e+09)
snowfall::sfInit(parallel = TRUE, cpus = n0)
snowfall::sfExport("pads","wids", "dart_fn", "resultfldr", "DARTvarsfldr", "resultpixfldr", "Topovarsfldr", "respname", "rad")
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
DART_FORT_quantiles_1 <- snowfall::sfLapply(wids, function(w){
  try(dart_fn(w, pads = pads))
})
t2 <- Sys.time()
print(difftime(t2, t1))
# 45 minutes for 4,192 pads, on 30-threads (15-cores @ 2.1 GHz)
# 4.63 hours for 4,192 on 4 threads (Intel Core i5-8365U @ 1.6 Ghz)
snowfall::sfStop()

df0 <- data.frame(matrix(ncol = length(DART_FORT_quantiles_1), nrow = 0))
for (i in seq_along(DART_FORT_quantiles_1)) {
  ii <- DART_FORT_quantiles_1[[i]]

  if (inherits(ii, 'try-error')) {
    df0 <- rbind(df0, rep(NA, length(ii)))
  } else {
    df0 <- rbind(df0, ii)
  }
}

df0[is.na(df0$wid), "wid"] <- row.names(df0)[is.na(df0$wid)]
stopifnot(all.equal(row.names(df0)))

DART_FORT_quantiles_1_raw <- DART_FORT_quantiles_1
DART_FORT_quantiles_1 <- df0

# Save result and cleanup
f0 <- file.path(resultfldr, 'DART_FORT_quantiles_1_raw.rda')
save(DART_FORT_quantiles_1_raw, file = f0, overwrite = T)
f1 <- file.path(resultfldr, 'DART_FORT_quantiles_1.rda')
save(DART_FORT_quantiles_1, file = f1, overwrite = T)
usethis::use_data(DART_FORT_quantiles_1, overwrite = T)
#setwd(proj_dir)
