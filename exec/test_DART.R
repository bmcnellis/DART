

# This script generates DART data for testing purposes.

set.seed(1)

library(DART)
library(rgdal)
library(raster)

# Workspaces folders and files for BEM
resultfldr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/results"
DARTvarsfldr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/inputs"
pads_dr <- "C:/Users/bmcnellis/Documents/DART/test_data/FORT/inputs/Well_pads_TVDART_StudyArea"
pads_layer <- "well_pad_selection_CO_River_watershed_albnad83"

# Input polygons
pads <- rgdal::readOGR(pads_dr, pads_layer) # in right projection
pads0 <- pads[1, ]

# TZ variable, required I guess? I think this is a windows problem
TZ <- ''

# Set up DART object:

DART_init <- DART::SpecifyDART(
  ID = '1',
  response = 'bareground_mask.tif',
  spatial_directory = DARTvarsfldr,
  results_directory = resultfldr,
  search_radius = 2000,
  mask = 'refrast.tif',
)

# Get DART data with specification:
DART_result <- DART::GetDart(DART_init, pads0)

# Pull out results:
result_df <- DART::GetResultsData(DART_result)
result_pixels <- DART::GetResultsPixels(DART_result)


