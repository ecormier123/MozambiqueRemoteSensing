rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
install.packages("snow")

beginCluster()

#Define thresholds how do you decide?
thresh.cloud = 0.005 # cloud threshold
thresh.land = 0.0215
##did I miss this somewhere?
ex.crp = 4930000 #what to crop by
#Mask offshore depth NA NEED TO FIND/how to make?
offshore = shapefile("U:\\SatelliteData\\Fixed-20160913\\Processed\\ExtraFile\\Nullwaterdepth3_MultiPolygon_MultiPolygon.shp")
#Depth data NEED to FIND
depth = raster("U:\\GroundTruthData\\Depth\\dem35c_5c.tif")
#Land mask
land = raster("U:\\GroundTruthData\\LandMasks\\T20NQ.tif")
#satellite data
nc.dat = "C:/Users/Cormi/Documents/test/subset_1_of_S2A_MSI_2021_06_14_07_56_37_T36KYU_L2R.nc"
#Output file
##write.data = "U:\\SatelliteData\\Fixed-20160913\\Processed\\20160913Level2A.tif"
write.data = "C:/Users/Cormi/Documents/test/subset_20210614Level2A.tif"
write.data2 = "C:/Users/Cormi/Documents/test/subset_20210614Level2Ax10000.tif"
##

####
nc = nc_open(nc.dat)
nc_atts <- ncatt_get(nc, 0)
##
blue1 = t(ncvar_get(nc, nc$var$rhos_492))
#blue1 = t(ncvar_get(nc, nc$var$sza))
blue = raster(blue1)
##changeing projection with project4string, is proj4string a variable in nc_atts?
proj4string(blue)  = crs(nc_atts$proj4_string)
##clipping the blue to nc atts extent
extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
#Depth Data NEED TO FIND
depth = crop(depth,blue)
depth = projectRaster(depth, blue, method="bilinear")
#depth = raster("U:\\SatelliteData\\Fixed-20160913\\Processed\\ExtraFile\\TestDepth.tif")
##does this remove all depths less than -10?
depth[is.na(depth[])] = -10 
##
###
green1 = t(ncvar_get(nc, nc$var$rhos_560))
green = raster(green1)
proj4string(green)  = crs(nc_atts$proj4_string)
extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
##
red1 = t(ncvar_get(nc, nc$var$rhos_665))
red = raster(red1)
proj4string(red)  = crs(nc_atts$proj4_string)
extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
##
nir1 = t(ncvar_get(nc, nc$var$rhos_833))
nir = raster(nir1)
proj4string(nir)  = crs(nc_atts$proj4_string)
extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
##dat.stack = stack(blue,green,red,nir,depth)
dat.stack = stack(blue,green,red,nir)
##rm(blue,green,red,nir,depth)
rm(blue,green,red,nir)
##names(dat.stack) = c("blue","green","red","nir","depth")
names(dat.stack) = c("blue","green","red","nir")
###ifelse returns a value with the same shape as test which is filled with elements selected from either yes or no depending on whether the element of test is TRUE or FALSE."ifelse(test, yes, no)" 
##testing to see if this works
landmask = t(ncvar_get(nc, nc$var$rhot_1614))
landmask = ifelse(landmask<="thresh.land",1,NA)#land mask finalized
##cloud mask written by kristen
cloud = t(ncvar_get(nc, nc$var$rhot_1373))
cloud = ifelse(cloud<="thresh.cloud",1,NA)#cloud mask finalized
neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0, NA, 1)#set to NA if any are negative
rm(blue1,green1,red1,nir1)
##what do the empty square brackets do?
landmask1 = dat.stack[[1]]
landmask1[] = landmask
cloud1 = dat.stack[[1]]
cloud1[] = cloud
neg.mask1 = dat.stack[[1]]
neg.mask1[] = neg.mask
##rmaskrs = cloud1*neg.mask1*land
rmaskrs = cloud1*neg.mask1*landmask1
rm(cloud,landmask1,neg.mask,thresh.land,thresh.cloud,cloud1,neg.mask1)
##
##
dat.stack = mask(x=dat.stack, mask = rmaskrs)#land, cloud, neg mask
dat.stack = crop(x = dat.stack, y = extent(c(nc_atts$xrange,ex.crp,nc_atts$yrange[1])))
##
nc_close(nc)
rm(nc,nc.dat,nc_atts, rmaskrs,ex.crp)
##
ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
##what is gndvi?
gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
dat.stack = stack(dat.stack,ndvi,gndvi)
rm(ndvi,gndvi)
##names(dat.stack) = c("blue","green","red","nir","depth","ndvi","gndvi")
names(dat.stack) = c("blue","green","red","nir","ndvi","gndvi")
##
offshore  = spTransform(offshore ,crs(dat.stack))
dat.stack = mask(x=dat.stack,mask=offshore,inverse=T)
##
writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
##what does this do?
for(i in 1:4){dat.stack[[i]] = dat.stack[[i]]*10000}
writeRaster(dat.stack, write.data2, format="GTiff",NAflag = NaN, overwrite=T)
##
rm(i,offshore,write.data,write.data2)
