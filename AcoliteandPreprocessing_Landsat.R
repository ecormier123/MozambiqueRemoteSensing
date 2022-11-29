rm(list=ls())
gc()

#
in.folder = "D:/SeptMay/LC09_L1TP_014042_20220501_20220501_02_T1"
out.folder = "D:/SeptMay"
#change the date to match today's date
date<-paste(format(Sys.time(), "acolite_settings_%Y%m%d_%I%p."),"txt", sep = "")
out.settings = date
##

#Read in settings file
set.file = read.delim("C:/Users/cormi/Documents/acolite_py_win/config/landsat.txt",header=F)#Acolite default
#define input file
set.file[1,1] = paste("inputfile=",in.folder,sep="")
#define output file
set.file[2,1] = paste("output=", out.folder,sep="")
set.file[3,1] = "limit=25.663219,-79.324705,25.801764,-79.198116"
set.file[4,1] = "dsf_aot_estimate=fixed"
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
write.table(set.file, paste(out.folder,"/",out.settings,sep=""),row.names=F,col.names=F,quote=F )


#Acolite call, have to provide the full paths for it to work
cmd = paste("C:/Users/cormi/Documents/acolite_py_win/dist/acolite/acolite.exe --cli --settings=",out.folder,"/",out.settings,sep="")
shell(cmd) #run acolite

rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
library("sf")


beginCluster()


#Mask offshore depth NA NEED TO FIND/how to make?
#offshore = shapefile("U:\\SatelliteData\\Fixed-20160913\\Processed\\ExtraFile\\Nullwaterdepth3_MultiPolygon_MultiPolygon.shp")
#havetousefullimagewhenusingfullraster
#import depth raster from NOAA
depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
#import created landmask
land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
#need this shapefile, created in Snap, to be able to crop NAs out of depth file after reprojection
cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
#import satellite data nc file
#nc.dat = "C:/Users/Cormi/Documents/test/subset_1_of_S2A_MSI_2021_06_14_07_56_37_T36KYU_L2R.nc"
#nc.dat ="C:\\Users\\cormi\\Documents\\test\\L8_OLI_2016_05_08_15_43_25_014042_L2R.nc"
#new file created with fixed DSF fitting rather than tiled which produced negative near-infrared values
nc.dat = "D:/SeptMay/L9_OLI_2022_05_01_15_43_35_014042_L2R.nc"

#create Output file
write.data = "D:/SeptMay/Landsat9_May2022.tif"
#don't think I need this
#write.data2 = "C:/Users/cormi/Documents/ImageProcessing/Reference/PreprocessingOuput/Landsat8_Sept2016x10000.tif"

#createoutputfileforinverse
#write.data = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmask.tif"
#write.data2 = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmaskx10000.tif"


####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
nc = nc_open(nc.dat)
nc_atts <- ncatt_get(nc, 0)
##
blue1 = t(ncvar_get(nc, nc$var$rhos_482))
#blue1 = t(ncvar_get(nc, nc$var$sza))
blue = raster(blue1)
##changeing projection with project4string
proj4string(blue)  = crs(nc_atts$proj4_string)
##clipping the blue to nc atts extent
extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])

#Depth Data 
#project depth to same crs as blue
depth = projectRaster(depth, blue, method="bilinear")
depth = crop(depth,cropdepth)
##change all NA to -30 to get rid of NAs in depth file
depth[is.na(depth[])] = -0.1
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



#had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
blue = crop(blue, depth)
green = crop (green, depth)
red = crop(red, depth)
nir = crop(nir, depth)
swir1 = crop(swir1, depth)
swir2 = crop(swir2, depth)

##stackalllayerstogether, don't include depth because this is not needed for aasg
dat.stack = stack(blue,green,red,nir,swir1, swir2)

##makeroom
rm(blue,green,red,nir,swir1, swir2, cropdepth)

##name the layers in data stack
names(dat.stack) = c("blue","green","red","nir","swir1", "swir2")

###ifelse returns a value with the same shape as test which is filled with elements selected from either yes or no depending on whether the element of test is TRUE or FALSE."ifelse(test, yes, no)" 
##testing to see if this works
#landmask = t(ncvar_get(nc, nc$var$rhot_1614))
#landmask = ifelse(landmask<="thresh.land",1,NA)#land mask finalized

##cloud mask written by kristen
#create map for cloud mask of layer 1373, because now the other data is cropped and this is not

cloud = t(ncvar_get(nc, nc$var$rhot_1374))
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
plot(rmaskrs)

##remove deep water
offshore=depth<=-31
plot(offshore)
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

#don't bother with ndvi and gndvi, not needed for the images that  are not being processed
#through random forest
##createndvilayer
#ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)

##what is gndvi?
#gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)

#create stack with ndvi and gndvi
#dat.stack = stack(dat.stack,ndvi,gndvi)
#rm(ndvi,gndvi)

##name layers in dat stack, did not include, depth, ndvi and gndvi becaus these 
#ae=re not needed for aasg
names(dat.stack) = c("blue","green","red","nir","swir1", "swir2")


##make the final raster!
writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)

##what does this do?
for(i in 1:4){dat.stack[[i]] = dat.stack[[i]]*10000}
writeRaster(dat.stack, write.data2, format="GTiff",NAflag = NaN, overwrite=T)
##




