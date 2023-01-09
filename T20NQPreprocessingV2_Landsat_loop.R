rm(list=ls())
gc()

##
folder = "D:/20122013"
#the list of satellite images that are in your folder (just their L1 product identifier)
sat.data = read.csv("D:/SatelliteImagesDownloaded2012_2013.csv")
#turns the csv file of images and other details into just a list of satellite names
sat.images.list = sat.data$Landsat.Product.Identifier.L1
#this was another optiojn I tried, but I couldn't figure out how to get it to work
#sat.images.list = (list.dirs(folder))
#sat.images.list  = sat.images.list[-1]

rm(sat.data)

#this starts a loop which runs through each satellite in the folder specified above
#based on the list of satellite images from sat.images.list, everything within the {}
# will be run

for (ii in 1:length(sat.images.list))
{
  #this code gives you updates of which image is being processed, the text in quotes can be changed to whatever you like
  print(paste("Running ", ii, " of ", length(sat.images.list), " in satellite images list", sep = ""))
  
  #this changes the in.folder for ewach lop to match to whichever satellite image you are working on
  in.folder = paste(folder,"/", sat.images.list[ii], sep = "")
  out.folder = "D:/output"
  #change the date to match today's date
  date<-paste(format(Sys.time(), "acolite_settings_%Y%m%d_%I%p."),"txt", sep = "")
  #changes the name of the out.settings to be the name of the satellite image being processed and the date
  out.settings = paste (sat.images.list[ii], date, sep="")
  ##
  
  #Read in settings file (this is blank file which you fill in with the info below)
  set.file = read.delim("C:/Users/cormi/Documents/acolite_py_win/config/landsat.txt",header=F)#Acolite default
  #define input file (sets input file to the image [ii])
  set.file[1,1] = paste("inputfile=",in.folder,sep="")
  #define output file (sets where the output image will be put)
  set.file[2,1] = paste("output=", out.folder,sep="")
  #crops the image to the coordinates that you want, in this case for Bimini it had to be cropped a lot
  #this decreases processing time
  set.file[3,1] = "limit=25.663219,-79.324705,25.801764,-79.198116"
  # a very important fix for the BImini image because it is a very small image and acolite overfits dsf 
  #if it is tiled instead of fixed and near-infrared values become negative
  set.file[4,1] = "dsf_aot_estimate=fixed"
  #the last few lines just remove extra files thta we don't need for the loop and allow the next step of processes in
  #the preprocessing file to run smoothly
  set.file[5,1] = "l1r_delete_netcdf=True"
  set.file[6,1] = "l2r_pans_delete_netcdf=True"
  set.file[7,1] = "delete_acolite_run_text_files=True" 
  set.file[8,1] = "rgb_rhos=False" 
  set.file[9,1] = "rgb_rhot=False"
  #set.file[5,1] = l2w_mask_threshold=0.035
  #set.file[6,1] = l2w_mask_high_toa_threshold=0.31
  #do a sunglint correction - seems this is all already in the file I have so no need to run code
  #set.file[33,1] = "glint_mask_rhos_band=1600"#change mask band
  #set.file[34,1] = "glint_mask_rhos_threshold=0.05"#change mask default of 0.05
  #set.file[35,1] = "glint_write_rhog_ref=False"
  #set.file[37,1] = "glint_write_rhog_all=False"
  #write l2w map
  #set.file[82,1] =  "map_l2w=True"
  
  #to fix error "ValueError: The requested sample points xi have dimension 6, but this RegularGridInterpolator has dimension 5" based on website help 
  #from https://odnature.naturalsciences.be/remsem/acolite-forum/viewtopic.php?t=321
  #set.file[98,1]="dsf_interface_reflectance=True"
  
  #write out the settings file
  
  write.table(set.file, paste(folder,"/",out.settings,sep=""),row.names=F,col.names=F,quote=F)
  
  
  #Acolite call, have to provide the full paths for it to work
  cmd = paste("C:/Users/cormi/Documents/acolite_py_win/dist/acolite/acolite.exe --cli --settings=",folder,"/",out.settings,sep="")
  shell(cmd)
  
  
}


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
#sat.images.list2  = sat.images.list2[-1]

for (ii in 1:length(sat.images.list2)) {
  
  out.folder = "D:/output"
  sat.images.list2 = (list.files(out.folder))
  #sat.images.list2  = sat.images.list2[-1]

  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  
  #import satellite data nc file
  nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
  nc = nc_open(nc.dat)
  nc_atts <- ncatt_get(nc, 0)
  ##
  
      if (nc_atts$sensor == "L5_ETM"){       
  
      out.folder = "D:/output"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/finaloutput", "/", sat.images.list2[ii], ".tif", sep="")
  
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created landmask
      land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
  
      blue1 = t(ncvar_get(nc, nc$var$rhos_486))
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
      
      rm(depth, land, offshore, dat.stack, swir11, swir21, cropdepth)
      
} else if (nc_atts$sensor == "L7_ETM"){
      
       out.folder = "D:/output"
       sat.images.list2 = (list.files(out.folder))
       write.data = paste("D:/finaloutput", "/", sat.images.list2[ii], ".tif", sep="")
         
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created landmask
      land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      
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
  
} else if (nc_atts$sensor == "L8_OLI") {  
    
      out.folder = "D:/output"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/finaloutput", "/", sat.images.list2[ii], ".tif", sep="")
      
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created landmask
      land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      
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
      
} else {
      
      out.folder = "D:/output"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/finaloutput", "/", sat.images.list2[ii], ".tif", sep="")
      
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created landmask
      land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      
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
