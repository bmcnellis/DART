# BEM 10 Nov 2020

library(DART)

DART_test_data_dir <- 'V:/PROJECTS/BRANDON_MCNELLIS/DART/test_data/TDE_PSW/all_spatial'
#DART_test_data_dir <- 'Z:/'

DART_test_obj <- InitializeDART(DART_test_data_dir, 1L)
#DART_test_obj <- new('DART')

mapTilesUrlTemplate = '//server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
x = -110.22
y = 38.478
debugMessage = TRUE
cols <- "BrBG" # this changes to a brown/green palette, require RColorBrewer

d <- DART::GetDart(DART_test_obj, -110.22, 38.478)

RunWebDART(debugMessage = T)
