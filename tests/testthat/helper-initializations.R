# Shortcuts to initialize DART objects in testing directories
#
# Projects are from:
#
#     SWEDD   : A Knight
#     FORT    : T Nauman
#     webDART : S Chambers

DART_test_init_swedd <- function() {
  require(DART)

  new_DART <- new('DART',
                  method = 'normal',
                  #outdir = file.path("/home/bem/Documents/DART/SWEDD_outputs"),
                  #outdir = file.path("/lustre/projects/ecosystems/sbsc/ucrb/swedd/outputs", paste0('dart', Sys.Date())),
                  outdir = "/lustre/projects/regions/southwest/sbsc/bmcnellis/DART/swedd_tests/outputs",
                  out_format = 'ESRI Points',
                  ##log file
                  logfile = paste0('~/dart_logfile_', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'), '.txt'),
                  ## Output Prefix
                  prefix = 'ID_',
                  ## Neighborhood radius (m)
                  rad = 3000,
                  ## Number of times to expand search radius if no candidates found
                  tries = 4,
                  ## Inner buffer radius (shrink treatment area to avoid edge effects) - must be >0
                  innerRad = 50,
                  buffer = 200,
                  #vars to filter by
                  filterVars = list(
                    soilec = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/ec_0to60cm_100xInt_ucrb.tif",
                    soilps = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/UCRB_mPSC_RFE_10plus.tif"
                  ),
                  # masking variables: 1 = mask, 0 = ok
                  maskVars = list(
                    refrast = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/refrast.tif",
                    roadrast = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/TIGER_2018_ucrb_mask.tif",
                    otherpads = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WYCOriv_Nopads.tif",
                    oilgas = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/oil_gas_buf_ucrb_mask.tif",
                    fires= "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/mtbs_mask.tif",
                    utblmfires= '/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/utfire_mask.tif',
                    wind = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WY_COriv_nowind.tif",
                    nlcd = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/NLCDcl.tif"
                  ),
                  # variables for distance matrix
                  topoVars = list(
                    ELEVm = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/ELEVm.tif",
                    PCURV = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/PCURV.tif",
                    TCURV = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/TCURV.tif",
                    RELHT1 = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT1.tif",
                    RELHT32 = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT32.tif",
                    RELHT128 = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT128.tif",
                    RELMNHT1 = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT1.tif",
                    RELMNHT32 = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT32.tif",
                    RELMNHT128 = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT128.tif",
                    MRRTF = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/MRRTF.tif",
                    MRVBF = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/MRVBF.tif",
                    SLOPE = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/SLOPE.tif",
                    SOUTHNESS = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/SOUTHNESS.tif",
                    EASTNESS = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/EASTNESS.tif",
                    TWI_TOPMODEL = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/TWI_TOPMODEL.tif",
                    CAlog_10 = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/CAlog_10.tif",
                    LFELEMS = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/LFELEMS.tif"
                  ),
                  ## response variables
                  respVars = list(
                    satvi = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/EE/satvi-median-MarNov/median.vrt"
                  ),
                  ## Treatments
                  # polygons must be in UTMs
                  polygons_path = '/lustre/projects/ecosystems/sbsc/ucrb/swedd/SelectedWells_20200624_utm',
                  polygons_file = 'SelectedWells_20200624_utm',
                  polygon_id_column = 'BLM_API',

                  interiorMaskVars = c('nlcd'),
                  refrast_projection = ''
  )

  return(new_DART)

}

DART_test_init_webDART <- function(DART_dir) {
  new_DART <- new('DART',
                  #vars to filter by
                  filterVars = list(
                    soilec = paste0(DART_dir, "/UCRB_Covariates/ec_0to60cm_100xInt_ucrb.tif"),
                    soilps = paste0(DART_dir, "/UCRB_Covariates/UCRB_mPSC_RFE_10plus.tif")
                  ),

                  # masking variables: 1 = mask, 0 = ok
                  maskVars = list(
                    refrast =  paste0(DART_dir, "/MASKS/refrast.tif"),
                    roadrast =  paste0(DART_dir, "/MASKS/TIGER_2018_ucrb_mask.tif"),
                    otherpads = paste0(DART_dir, "/MASKS/WYCOriv_Nopads.tif"),
                    oilgas = paste0(DART_dir, "/MASKS/oil_gas_buf_ucrb_mask.tif"),
                    fires = paste0(DART_dir, "/MASKS/mtbs_mask.tif"),
                    utblmfires = paste0(DART_dir, "/MASKS/utfire_mask.tif"),
                    nlcd = paste0(DART_dir, "/MASKS/NLCDcl.tif")
                  ),

                  # variables for distance matrix
                  topoVars = list(
                    PCURV = paste0(DART_dir, "/UCRB_Covariates/PCURV.tif"),
                    TCURV = paste0(DART_dir, "/UCRB_Covariates/TCURV.tif"),
                    #RELHT1 = paste0(DART_dir, "/UCRB_Covariates/RELHT1.tif"),
                    RELHT32 = paste0(DART_dir, "/UCRB_Covariates/RELHT32.tif"),
                    RELHT128 = paste0(DART_dir, "/UCRB_Covariates/RELHT128.tif"),
                    MRRTF = paste0(DART_dir, "/UCRB_Covariates/MRRTF.tif"),
                    MRVBF = paste0(DART_dir, "/UCRB_Covariates/MRVBF.tif"),
                    #SLOPE = paste0(DART_dir, "/UCRB_Covariates/SLOPE.tif"),
                    EASTNESS = paste0(DART_dir, "/UCRB_Covariates/EASTNESS.tif"),
                    #TWI_TOPMODEL = paste0(DART_dir, "/UCRB_Covariates/TWI_TOPMODEL.tif"),
                    LFELEMS = paste0(DART_dir, "/UCRB_Covariates/LFELEMS.tif")
                  ),


                  ## response variables
                  respVars = list(
                    SATVI = paste0(DART_dir, "/SATVI.vrt")
                  ),

                  ## BEM: other variables apparently critical to execution

                  # from S Fick's NRCS-master params.R file

                  # SF: masking variables for inside treated area
                  #interiorMaskVars = c('roadrast', 'oilgas', 'oilgas4corners', 'nlcdBuf')
                  # BEM: 'oilgas4corners' and 'nlcdBuf' removed, not present in provided files
                  interiorMaskVars = c('roadrast', 'oilgas')
  )

  return(new_DART)
}

DART_test_init_FORT <- function(DART_dir) {

  new_DART <- new('DART',
                  #vars to filter by
                  filterVars = list(
                    soilec = file.path(DART_dir, "ec_0to60cm_100xInt_ucrb.tif"),
                    soilps = paste0(DART_dir, "UCRB_mPSC_RFE_10plus.tif")
                  ),

                  # masking variables: 1 = mask, 0 = ok
                  maskVars = list(
                    refrast =  paste0(DART_dir, "/MASKS/refrast.tif"),
                    roadrast =  paste0(DART_dir, "/MASKS/TIGER_2018_ucrb_mask.tif"),
                    otherpads = paste0(DART_dir, "/MASKS/WYCOriv_Nopads.tif"),
                    oilgas = paste0(DART_dir, "/MASKS/oil_gas_buf_ucrb_mask.tif"),
                    fires = paste0(DART_dir, "/MASKS/mtbs_mask.tif"),
                    utblmfires = paste0(DART_dir, "/MASKS/utfire_mask.tif"),
                    nlcd = paste0(DART_dir, "/MASKS/NLCDcl.tif")
                  ),

                  # variables for distance matrix
                  topoVars = list(
                    PCURV = paste0(DART_dir, "/UCRB_Covariates/PCURV.tif"),
                    TCURV = paste0(DART_dir, "/UCRB_Covariates/TCURV.tif"),
                    #RELHT1 = paste0(DART_dir, "/UCRB_Covariates/RELHT1.tif"),
                    RELHT32 = paste0(DART_dir, "/UCRB_Covariates/RELHT32.tif"),
                    RELHT128 = paste0(DART_dir, "/UCRB_Covariates/RELHT128.tif"),
                    MRRTF = paste0(DART_dir, "/UCRB_Covariates/MRRTF.tif"),
                    MRVBF = paste0(DART_dir, "/UCRB_Covariates/MRVBF.tif"),
                    #SLOPE = paste0(DART_dir, "/UCRB_Covariates/SLOPE.tif"),
                    EASTNESS = paste0(DART_dir, "/UCRB_Covariates/EASTNESS.tif"),
                    #TWI_TOPMODEL = paste0(DART_dir, "/UCRB_Covariates/TWI_TOPMODEL.tif"),
                    LFELEMS = paste0(DART_dir, "/UCRB_Covariates/LFELEMS.tif")
                  ),


                  ## response variables
                  respVars = list(
                    SATVI = paste0(DART_dir, "/SATVI.vrt")
                  ),

                  ## BEM: other variables apparently critical to execution

                  # from S Fick's NRCS-master params.R file

                  # SF: masking variables for inside treated area
                  #interiorMaskVars = c('roadrast', 'oilgas', 'oilgas4corners', 'nlcdBuf')
                  # BEM: 'oilgas4corners' and 'nlcdBuf' removed, not present in provided files
                  interiorMaskVars = c('roadrast', 'oilgas')
  )

  return(new_DART)
}
