(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")


beginCluster()


#Mask offshore depth NA NEED TO FIND/how to make?
#offshore = shapefile("U:\\SatelliteData\\Fixed-20160913\\Processed\\ExtraFile\\Nullwaterdepth3_MultiPolygon_MultiPolygon.shp")
#havetousefullimagewhenusingfullraster
#import depth raster from NOAA
depth = raster("C:\\Users\\cormi\\Documents\\test\\bathymetry\\Bathymetry_Bimini_NOAA.tif")
#import created landmask
land = raster("C:\\Users\\cormi\\Documents\\test\\landmask\\landmaskBimini.tif")

#import satellite data nc file
#nc.dat = "C:/Users/Cormi/Documents/test/subset_1_of_S2A_MSI_2021_06_14_07_56_37_T36KYU_L2R.nc"
nc.dat ="C:\\Users\\cormi\\Documents\\test\\L8_OLI_2016_05_08_15_43_25_014042_L2R.nc"

#create Output file
write.data = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016.tif"
write.data2 = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016x10000.tif"

#createoutputfileforinverse
#write.data = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmask.tif"
#write.data2 = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmaskx10000.tif"


####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
nc = nc_open(nc.dat)
nc_atts <- ncatt_get(nc, 0)
##
blue1 = t(ncvar_get(nc, nc$var$rhos_483))
#blue1 = t(ncvar_get(nc, nc$var$sza))
blue = raster(blue1)
##changeing projection with project4string
proj4string(blue)  = crs(nc_atts$proj4_string)
##clipping the blue to nc atts extent
extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])

#Depth Data 
#project depth to same crs as blue
depth = projectRaster(depth, blue, method="bilinear")

##change all NA to -30 to get rid of NAs in depth file
depth[is.na(depth[])] = -30 
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

##stackalllayerstogether
dat.stack = stack(blue,green,red,nir,depth)

##makeroom
rm(blue,green,red,nir)

##name the layers in data stack
names(dat.stack) = c("blue","green","red","nir","depth")

###ifelse returns a value with the same shape as test which is filled with elements selected from either yes or no depending on whether the element of test is TRUE or FALSE."ifelse(test, yes, no)" 
##testing to see if this works
#landmask = t(ncvar_get(nc, nc$var$rhot_1614))
#landmask = ifelse(landmask<="thresh.land",1,NA)#land mask finalized

##cloud mask written by kristen
cloud = t(ncvar_get(nc, nc$var$rhot_1373))
cloud = ifelse(cloud<="thresh.cloud",1,NA)#cloud mask finalized

neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0, NA, 1)#set to NA if any are negative

#makeroom
rm(blue1,green1,red1,nir1)
##what do the empty square brackets do?
cloud1 = dat.stack[[1]]
cloud1[] = cloud
neg.mask1 = dat.stack[[1]]
neg.mask1[] = neg.mask

##create final mask
rmaskrs = cloud1*neg.mask1*land
plot(rmaskrs)

##TEST removing offshore deep water - need to create this mask!!!!testing this
#offshore=depth<=-31
#plot(offshore)
#write.data2= "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_offshore_test.tif"
#dat.stack2 = mask(x=dat.stack,mask=offshore)
#writeRaster(dat.stack2, write.data2, format="GTiff",NAflag = NaN, overwrite=T)

#makeroom
rm(cloud,landmask1,neg.mask,cloud1,neg.mask1)



#maskdat.stack and crop
dat.stack = mask(x=dat.stack, mask = rmaskrs, inverse=T)#land, cloud, neg mask

#to create reverse land mask
#dat.stack = mask(x=dat.stack, mask = rmaskrs)


##
nc_close(nc)
#makeroom
rm(nc,nc.dat,nc_atts,rmaskrs)

##createndvilayer
ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)

##what is gndvi?
gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)

#create stack with ndvi and gndvi
dat.stack = stack(dat.stack,ndvi,gndvi)
rm(ndvi,gndvi)

##name layers in dat stack
names(dat.stack) = c("blue","green","red","nir","depth", "ndvi","gndvi")

##make the final raster!
writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)

##what does this do?
for(i in 1:4){dat.stack[[i]] = dat.stack[[i]]*10000}
writeRaster(dat.stack, write.data2, format="GTiff",NAflag = NaN, overwrite=T)
##



