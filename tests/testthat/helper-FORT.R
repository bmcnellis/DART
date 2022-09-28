
# Original DART 2.0 workhorse function
# AUT: T Nauman
# CTB: B McNellis

dart_fn <- function(w, pads){ # wids will be list for list apply and w the iterator
  padpoly <- pads[pads$wid %in% w,] # query wellpad polygon
  ## Bring in rasters to use in DART
  #on.exit(setwd(proj_dir))
  setwd(DARTvarsfldr)
  soilec <- raster("ec_0to60cm_100xInt_ucrb.tif")
  soilps <- raster("UCRB_mPSC_smote_30m_nm.tif")
  refrast <- raster("refrast.tif")
  roadrast <- raster("wy_roads_bldgs_mask_isnull.tif")
  otherpads <- raster("WYCOriv_Nopads.tif")
  # irrigated <- raster("WYCOrivnotIrrigated.tif")
  # coal <- raster("NoCoalCOriv.tif")
  # wind <- raster("WY_COriv_nowind.tif")
  # ddct <- raster("DDCT_mask_isnull.tif")
  ltdl <- raster("LTDL_mask_isnull.tif")
  burn <- raster("burn_mask_84_15_isnull.tif")
  resp <- raster("bareground_mask.tif") # response variable e.g. 2016 bareground model
  rast.proj <- projection(refrast)
  setwd(Topovarsfldr)
  nlcd <- raster("NLCDcl.tif")
  topovarlst <- c(
    "ELEVm.tif",
    "PCURV.tif",
    "TCURV.tif",
    "RELHT1.tif",
    "RELHT32.tif",
    "RELHT128.tif",
    "RELMNHT1.tif",
    "RELMNHT32.tif",
    "RELMNHT128.tif",
    "MRRTF.tif",
    "MRVBF.tif",
    "SLOPE.tif",
    "SOUTHNESS.tif",
    "EASTNESS.tif",
    "TWI_TOPMODEL.tif",
    "CAlog_10.tif",
    "LFELEMS.tif"
  )

  missing_topo <- sapply(topovarlst, file.exists)
  if (any(!missing_topo)) {
    stop("missing topo vars:", topovarlst[!missing_topo])
  }

  if (FALSE) {
    cat('\n')
    for (i in seq_along(topovarlst)) {
      ii <- topovarlst[i]
      cat("read topo var:", ii, '\n')
      i0 <- raster(ii)
    }
    stop("topo variable read OK")
  }

  topovars <- stack(topovarlst)
  toponames <- gsub(".tif","", topovarlst)
  ## Prepare neighborhood
  padpolybuf <- buffer(padpoly,width=rad)
  refpadbufrast <- crop(refrast, padpolybuf)
  padpolybuf <- raster::erase(padpolybuf, padpoly)
  padpolybuf$rastval <- 1
  padpoly$rastval <- 1
  ## Create rasters
  padbufrast <- rasterize(padpolybuf, refpadbufrast,field=padpolybuf$rastval, datatype='INT1U')
  padrast <- rasterize(padpoly, refpadbufrast,field=padpoly$rastval, datatype='INT1U')
  ## Screen out unwanted disturbances in buffer zone
  e <- extent(padbufrast)
  padroadrast <- raster::crop(roadrast,e)
  f_mask <- function(a,b) a*b
  padbufrast <- overlay(padbufrast,padroadrast,fun=f_mask)
  padotherpads <- raster::crop(otherpads,e)
  padbufrast <- overlay(padbufrast,padotherpads,fun=f_mask)
  padnlcd <- raster::crop(nlcd,e)
  nlcd_fn <- function(nlcd,padbufrast) {ind <- ifelse(nlcd!=21&nlcd!=22&nlcd!=24&nlcd!=81&nlcd!=82&nlcd!=11&nlcd!=12&padbufrast==1,1,NA)
  return(ind)
  }
  padbufrast <- overlay(padnlcd,padbufrast,fun=nlcd_fn)
  # padcoal <- raster::crop(coal,e)
  # padbufrast <- overlay(padbufrast,padcoal,fun=f_mask)
  # padirrig <- raster::crop(irrigated,e)
  # padbufrast <- overlay(padbufrast,padirrig,fun=f_mask)
  # padwind <- raster::crop(wind,e)
  # padbufrast <- overlay(padbufrast,padwind,fun=f_mask)
  # padddct <- raster::crop(ddct,e)
  # padbufrast <- overlay(padbufrast,padddct,fun=f_mask)
  padltdl <- raster::crop(ltdl,e)
  padbufrast <- overlay(padbufrast,padltdl,fun=f_mask)
  padburn <- raster::crop(burn,e)
  padbufrast <- overlay(padbufrast,padburn,fun=f_mask)
  ## Now prep soils and topo rasters
  ecpadbuf <- raster::crop(soilec,e)
  ecpadbuf <- overlay(padbufrast,ecpadbuf,fun=f_mask)
  names(ecpadbuf) <- "ec"
  soilpspadbuf <- raster::crop(soilps,e)
  soilpspadbuf <- overlay(padbufrast,soilpspadbuf,fun=f_mask)
  names(soilpspadbuf) <- "psc"
  resppadbuf <- raster::crop(resp,e)
  resppadbuf <- overlay(padbufrast,resppadbuf,fun=f_mask)
  names(resppadbuf) <- "resp"
  topopadbuf <- raster::crop(topovars,e)
  topopadbuf <- overlay(padbufrast,topopadbuf,fun=f_mask)
  names(topopadbuf) <- toponames
  padbufstk <- stack(ecpadbuf,soilpspadbuf,topopadbuf, resppadbuf)
  ## Create pad raster layers
  ecpad <- raster::crop(soilec,e)
  ecpad <- overlay(padrast,ecpad,fun=f_mask)
  names(ecpad) <- "ec"
  soilpspad <- raster::crop(soilps,e)
  soilpspad <- overlay(padrast,soilpspad,fun=f_mask)
  names(soilpspad) <- "psc"
  resppad <- raster::crop(resp,e)
  resppad <- overlay(padrast,resppad,fun=f_mask)
  names(resppad) <- "resp"
  topopad <- raster::crop(topovars,e)
  topopad <- overlay(padrast,topopad,fun=f_mask)
  names(topopad) <- toponames
  padstk <- stack(ecpad,soilpspad,topopad,resppad)

  ## Covert cropped rasters to Spatial Pixels DFs
  padpixels <- as(padstk, "SpatialPixelsDataFrame")
  padpixels@data$ec <- padpixels@data$ec / 100 # Rescale back to actual ec units from scaled integer used to store raster
  padbufpixels <- as(padbufstk, "SpatialPixelsDataFrame")
  padbufpixels@data$ec <- padbufpixels@data$ec / 100 # Rescale back to actual ec units from scaled integer used to store raster

  #return(padbufpixels)

  ## Summarize pad pixels
  padpscclasses <- as.numeric(names(summary(as.factor(padpixels@data$psc))))
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  } # Fn to calculate mode if that is desirable
  padpscmode <- getmode(padpixels@data$psc)
  ecave <- mean(padpixels@data$ec)
  ecmin <- min(padpixels@data$ec) - min(padpixels@data$ec) * 0.05
  ecmax <- max(padpixels@data$ec) + max(padpixels@data$ec) * 0.05

  ## Select pad buffer pixels with same PSC and EC
  #padbufpixels@data$art <- ifelse(padbufpixels@data$psc %in% padpscclasses & padbufpixels@data$ec < ecmax & padbufpixels@data$ec > ecmin,1,0)
  padbufsoilpixels <- subset(padbufpixels, padbufpixels@data$psc %in% padpscclasses & padbufpixels@data$ec < ecmax & padbufpixels@data$ec > ecmin)
  padbufsoilpixels@data$LFELEMS <- as.character(padbufsoilpixels@data$LFELEMS)
  padpixels@data$LFELEMS <- as.character(padpixels@data$LFELEMS)

  ## Set up topographic similarity routine and select ART pixels

  # padpixels is the target pixels
  # padbufpixels is the (2km) buffered target
  # padbufsoilpixels is the candidate pixels, AFTER ControlSubset equivalent line above

  # now, select the buffered target pixels (after controlling for soils)
  # that are closest to the pad pixels:

  # this part is nuts and I don't know why it does what it does

  if(length(padbufsoilpixels) > 100){
    dat1 <- padpixels@data[,c(toponames)]
    dat2 <- padbufsoilpixels@data[,c(toponames)]
    toposim <- gower_topn(x=dat1, y=dat2, n=100, nthread = 1)
    toposimf1 <- as.data.frame(factor(toposim$index[1,], levels = unique(toposim$index[1,])))
    colnames(toposimf1) <- "rows"
    toposimf1 <- plyr::count(toposimf1, "rows")
    toposimf1$rows<-factor(toposimf1$rows, levels=toposimf1$rows[order(toposimf1$freq, decreasing=TRUE)])
    toposimrows <- as.character(toposimf1$rows)
    toposimrows <- unique(toposimrows)
    toposimrowstogo <- 100-length(toposimrows)
    toposimf25 <- as.data.frame(as.factor(toposim$index[2:25,]))
    colnames(toposimf25) <- "rows"
    toposimf25 <- plyr::count(toposimf25, "rows")
    toposimf25$rows<-factor(toposimf25$rows, levels=toposimf25$rows[order(toposimf25$freq, decreasing=TRUE)])
    if(length(toposimf25$rows)<toposimrowstogo){
      toposimrows <- append(toposimrows, as.character(toposimf25$rows))
      toposimrows <- unique(toposimrows)
      toposimrowstogo <- 100-length(toposimrows)
    } else {
      toposimrows <- append(toposimrows, names(summary(toposimf25$rows)[1:toposimrowstogo]))
      toposimrows <- unique(toposimrows)
      toposimrowstogo <- 100-length(toposimrows)
    }
    if (length(toposimrowstogo)<100){
      toposimf100 <- as.data.frame(as.factor(toposim$index[26:100,]))
      colnames(toposimf100) <- "rows"
      toposimf100 <- plyr::count(toposimf100, "rows")
      toposimf100$rows<-factor(toposimf100$rows, levels=toposimf100$rows[order(toposimf100$freq, decreasing=TRUE)])
      toposimrows <- append(toposimrows, names(summary(toposimf100$rows)[1:toposimrowstogo]))
      toposimrows <- unique(toposimrows)
      toposimrowstogo <- 100-length(toposimrows)
      while(toposimrowstogo > 0){
        toposimf100 <- subset(toposimf100,!(as.character(rows) %in% toposimrows))
        colnames(toposimf100) <- "rows"
        toposimf100 <- plyr::count(toposimf100, "rows")
        toposimf100$rows<-factor(toposimf100$rows, levels=toposimf100$rows[order(toposimf100$freq, decreasing=TRUE)])
        toposimrows <- append(toposimrows, names(summary(toposimf100$rows)[1:toposimrowstogo]))
        toposimrows <- unique(toposimrows)
        toposimrowstogo <- 100-length(toposimrows)
      }
    } else {
      next
    }
    artpixels <- padbufsoilpixels[as.numeric(toposimrows),]
    rowindex <- which(toposim$index %in% toposimrows, arr.ind = T)
    distances <- toposim$distance[rowindex]
    avedist <- mean(distances)

  } else {
    if(length(padbufsoilpixels) > 0){
      artpixels <- padbufsoilpixels
      dat1 <- padpixels@data[,c(toponames)]
      dat2 <- artpixels@data[,c(toponames)]
      toposim <- gower_topn(x=dat1, y=dat2, n=length(padbufsoilpixels), nthread = 1)
      avedist <- mean(toposim$distance)
    }
    next
  }

  ## Calculate Quantiles
  # Response variable
  artpixels@data$cover <- try(100 - artpixels@data$resp)
  padpixels@data$cover <- try(100 - padpixels@data$resp)
  padcovave <- mean(padpixels@data$cover)
  # quan = quantile that the average value of the response variable in
  #        the treatment polygons falls in, relative to the distribution
  #        of the response variable in the 100 most-matching masked,
  #        controlled, and topographically subsetted reference pixels.
  quan <- try(ecdf(artpixels@data$cover)(as.numeric(padcovave)), silent = TRUE)

  ## Pull together wanted data into DF
  n_artpixels <- try(length(artpixels))
  # how many DART pixels were finally selected?
  n_padpixels <- try(length(padpixels))
  # how many pixels were in the original treatment polygon?
  n_soilpixels <- length(padbufsoilpixels)
  # how many pixels were in the buffered, masked, and controlled polygon?
  radkm <- rad/1000
  quantdf <- try(
    data.frame(
      w, # id of current polygon
      avedist,
      # mean of the toposim/gower_topn distances for the top-N pixels
      # selected by the ART subroutine
      n_artpixels,
      # number of ART pixels, should be = n
      n_padpixels,
      # number of pixels in the treatment polygon
      n_soilpixels,
      # number of pixels in the buffered, masked, controlled polygon
      quan, # as above
      respname, # not defined in this scope,
                # evaluates to "soil cover"
      padpscmode, # mode of the PSC variable in the treatment polygons
      ecave, # average EC -yg diagnostics
      ecmin, # minimum EC - diagnostics
      ecmax, # maximum EC - diagnostics
      radkm  # radius of the search polygons, converted to km
    ))
  names(quantdf) <- c("wid", "avedist", "n_artpx","n_padpx","n_slpx","quant", "respons","padpsc","padec","pdecmin","pdecmax","radkm")

  # adding metadata to the output dataframe
  artpixels@data$wid <- w
  artrast <- raster(artpixels[c("wid")])
  padpixels@data$wid <- w
  padrastexp <- raster(padpixels[c("wid")])

  # RETURNS: two rasters (one of the selected reference pixels, one of
  #          the treatment polygon pixels) and a data.frame that has some
  #          summary statistics in it

  ## Saving reference pixels
  setwd(resultpixfldr)
  # writeGDAL(artpixels,paste(w,"artpixels.tif",sep="_"),drivername="GTiff", options=NULL)#option for saving SpatPixelsDF with all layers
  # writeGDAL(artpixels[22],paste(w,"artpixels.tif",sep="_"),drivername="GTiff", options=NULL, type="Int32")#option for saving SpatPixelsDF with just art pixel labeled with wid

  writeRaster(
    artrast,
    overwrite = TRUE,
    filename = paste(w, "artpixels.tif", sep = "_"),
    datatype = 'INT4U',
    options = c("COMPRESS=DEFLATE", "TFW=YES")
  )

  writeRaster(
    padrastexp,
    overwrite = TRUE,
    filename = paste(w, "padpixels.tif", sep = "_"),
    datatype = 'INT4U',
    options = c("COMPRESS=DEFLATE", "TFW=YES")
  )

  #Might also want to covert to raster package format for saving with better filesize
  return(quantdf)
  gc()
}
