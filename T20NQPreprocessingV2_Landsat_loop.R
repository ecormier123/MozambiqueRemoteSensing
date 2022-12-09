rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
library("sf")
library("RStoolbox")

out.folder = "D:/output"
sat.images.list2 = (list.files(out.folder))
sat.images.list2  = sat.images.list2[-1]

for (ii in 1:length(sat.images.list2))
{
  
  sat.images.list2 = (list.files(out.folder))
  sat.images.list2  = sat.images.list2[-1]
  
  #import depth raster from NOAA
  depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
  #import created landmask
  land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
  #need this shapefile, created in Snap, to be able to crop NAs out of depth file after reprojection
  sat.data = read.csv("D:/SatelliteImagesDownloaded2012_2013.csv")
  sat.images.list = sat.data$Landsat.Product.Identifier.L1


  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  #import satellite data nc file
  nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")

  
  #create Output file
  write.data = paste("D:/finaloutput", "/", sat.images.list[ii], ".tif", sep="")

  #createoutputfileforinverse
  #write.data = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmask.tif"
  #write.data2 = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmaskx10000.tif"


  ####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
  nc = nc_open(nc.dat)
  nc_atts <- ncatt_get(nc, 0)
  ##
  
if (nc_atts$sensor == "L5_ETM")
  
  {   blue1 = t(ncvar_get(nc, nc$var$rhos_486))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_571))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_660))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_839))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1678))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2217))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      depth = crop(depth,cropdepth)
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
      
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = crop(blue, depth)
      green = crop (green, depth)
      red = crop(red, depth)
      nir = crop(nir, depth)
      swir1 = crop(swir1, depth)
      swir2 = crop(swir2, depth)
      
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red,nir,swir1, swir2, depth)
      
      ##makeroom
      rm(blue,green,red,nir,swir1, swir2)
      
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2", "depth")
      
      
      #cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      #cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      #cloud = raster(cloud)
      #proj4string(cloud)  = crs(nc_atts$proj4_string)
      #extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      #cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      #cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #same process with cloud is followed for neg mask
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      neg.mask = raster(neg.mask)
      proj4string(neg.mask)  = crs(nc_atts$proj4_string)
      extent(neg.mask) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      neg.mask = crop(neg.mask, depth)
      neg.mask = as.matrix(neg.mask)
      
      #makeroom
      rm(blue1,green1,red1,nir1)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      neg.mask1 = dat.stack[[1]]
      neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      #landmask
      #project depth to same crs and extent as other layers
      land = projectRaster(land, depth, method="bilinear")
      land = crop(land,cropdepth)
      
      #rmaskrs = cloud1*neg.mask1*land
      rmaskrs = neg.mask1*land
      
      ##remove deep water
      offshore=depth<=-31
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      #rm(cloud,neg.mask,cloud1,neg.mask1)
      rm(neg.mask,neg.mask1)
      
      
      #maskdat.stack and crop
      dat.stack = mask(x=dat.stack, mask = rmaskrs, inverse=T)#land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
      
      #to create reverse land mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs,offshoremask)
      
      ##createndvilayer
      ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
      
      ##what is gndvi?
      gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
      
      #create stack with ndvi and gndvi
      dat.stack = stack(dat.stack,ndvi,gndvi)
      rm(ndvi,gndvi)
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2","depth", "ndvi","gndvi")
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)  
      
    }
 
  else if (nc_atts$sensor == "L7_ETM")
  
  {
      blue1 = t(ncvar_get(nc, nc$var$rhos_479))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
  
   
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_561))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
  
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_661))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
  
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_835))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
  
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1650))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
  
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2208))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
    
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      depth = crop(depth,cropdepth)
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
  
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = crop(blue, depth)
      green = crop (green, depth)
      red = crop(red, depth)
      nir = crop(nir, depth)
      swir1 = crop(swir1, depth)
      swir2 = crop(swir2, depth)
  
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red,nir,swir1, swir2, depth)
  
      ##makeroom
      rm(blue,green,red,nir,swir1, swir2)
  
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2", "depth")
    
  
      #cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      #cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      #cloud = raster(cloud)
      #proj4string(cloud)  = crs(nc_atts$proj4_string)
      #extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      #cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      #cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #same process with cloud is followed for neg mask
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      neg.mask = raster(neg.mask)
      proj4string(neg.mask)  = crs(nc_atts$proj4_string)
      extent(neg.mask) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      neg.mask = crop(neg.mask, depth)
      neg.mask = as.matrix(neg.mask)
      
      #makeroom
      rm(blue1,green1,red1,nir1)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      neg.mask1 = dat.stack[[1]]
      neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      #landmask
      #project depth to same crs and extent as other layers
      land = projectRaster(land, depth, method="bilinear")
      land = crop(land,cropdepth)
      
      #rmaskrs = cloud1*neg.mask1*land
      rmaskrs = neg.mask1*land
      
      ##remove deep water
      offshore=depth<=-31
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      #rm(cloud,neg.mask,cloud1,neg.mask1)
      rm(neg.mask,neg.mask1)
  
  
      #maskdat.stack and crop
      dat.stack = mask(x=dat.stack, mask = rmaskrs, inverse=T)#land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
  
      #to create reverse land mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
  
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs,offshoremask)
  
      ##createndvilayer
      ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
  
      ##what is gndvi?
      gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
  
      #create stack with ndvi and gndvi
      dat.stack = stack(dat.stack,ndvi,gndvi)
      rm(ndvi,gndvi)
  
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2","depth", "ndvi","gndvi")
  
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
  
  } 
  
    else if (nc_atts$sensor == "L8_OLI")
      
  {
      blue1 = t(ncvar_get(nc, nc$var$rhos_483))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_561))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_655))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_865))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1609))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2201))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      depth = crop(depth,cropdepth)
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
      
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = crop(blue, depth)
      green = crop (green, depth)
      red = crop(red, depth)
      nir = crop(nir, depth)
      swir1 = crop(swir1, depth)
      swir2 = crop(swir2, depth)
      
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red,nir,swir1, swir2, depth)
      
      ##makeroom
      rm(blue,green,red,nir,swir1, swir2)
      
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2", "depth")
      
      
      cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      cloud = raster(cloud)
      proj4string(cloud)  = crs(nc_atts$proj4_string)
      extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #same process with cloud is followed for neg mask
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      neg.mask = raster(neg.mask)
      proj4string(neg.mask)  = crs(nc_atts$proj4_string)
      extent(neg.mask) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      neg.mask = crop(neg.mask, depth)
      neg.mask = as.matrix(neg.mask)
      
      #makeroom
      rm(blue1,green1,red1,nir1)
      
      ##what do the empty square brackets do?
      cloud1 = dat.stack[[1]]
      cloud1[] = cloud
      neg.mask1 = dat.stack[[1]]
      neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      #landmask
      #project depth to same crs and extent as other layers
      land = projectRaster(land, depth, method="bilinear")
      land = crop(land,cropdepth)
      
      rmaskrs = cloud1*neg.mask1*land
      
      ##remove deep water
      offshore=depth<=-31
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      rm(cloud,neg.mask,cloud1,neg.mask1)
      
      
      
      #maskdat.stack and crop
      dat.stack = mask(x=dat.stack, mask = rmaskrs, inverse=T)#land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
      
      #to create reverse land mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs,offshoremask)
      
      ##createndvilayer
      ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
      
      ##what is gndvi?
      gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
      
      #create stack with ndvi and gndvi
      dat.stack = stack(dat.stack,ndvi,gndvi)
      rm(ndvi,gndvi)
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2","depth", "ndvi","gndvi")
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    }
    
    else (nc_atts$sensor=="L9_OLI")
    
    {
      blue1 = t(ncvar_get(nc, nc$var$rhos_482))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_561))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_654))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_865))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1608))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2201))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      depth = crop(depth,cropdepth)
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
      
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = crop(blue, depth)
      green = crop (green, depth)
      red = crop(red, depth)
      nir = crop(nir, depth)
      swir1 = crop(swir1, depth)
      swir2 = crop(swir2, depth)
      
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red,nir,swir1, swir2, depth)
      
      ##makeroom
      rm(blue,green,red,nir,swir1, swir2)
      
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2", "depth")
      
      
      cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      cloud = raster(cloud)
      proj4string(cloud)  = crs(nc_atts$proj4_string)
      extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #same process with cloud is followed for neg mask
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      neg.mask = raster(neg.mask)
      proj4string(neg.mask)  = crs(nc_atts$proj4_string)
      extent(neg.mask) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      neg.mask = crop(neg.mask, depth)
      neg.mask = as.matrix(neg.mask)
      
      #makeroom
      rm(blue1,green1,red1,nir1)
      
      ##what do the empty square brackets do?
      cloud1 = dat.stack[[1]]
      cloud1[] = cloud
      neg.mask1 = dat.stack[[1]]
      neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      #landmask
      #project depth to same crs and extent as other layers
      land = projectRaster(land, depth, method="bilinear")
      land = crop(land,cropdepth)
      
      rmaskrs = cloud1*neg.mask1*land
      
      ##remove deep water
      offshore=depth<=-31
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      rm(cloud,neg.mask,cloud1,neg.mask1)
      
      
      
      #maskdat.stack and crop
      dat.stack = mask(x=dat.stack, mask = rmaskrs, inverse=T)#land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
      
      #to create reverse land mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs,offshoremask)
      
      ##createndvilayer
      ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
      
      ##what is gndvi?
      gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
      
      #create stack with ndvi and gndvi
      dat.stack = stack(dat.stack,ndvi,gndvi)
      rm(ndvi,gndvi)
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red","nir","swir1", "swir2","depth", "ndvi","gndvi")
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
}
}
