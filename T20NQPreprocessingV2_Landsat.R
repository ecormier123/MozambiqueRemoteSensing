rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
library("sf")
library("RStoolbox")



#Mask offshore depth NA NEED TO FIND/how to make?
#offshore = shapefile("U:\\SatelliteData\\Fixed-20160913\\Processed\\ExtraFile\\Nullwaterdepth3_MultiPolygon_MultiPolygon.shp")
#havetousefullimagewhenusingfullraster
#import depth raster from NOAA
depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
#import created landmask
water = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/watermaskBimini.tif")
#need this shapefile, created in Snap, to be able to crop NAs out of depth file after reprojection
#cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")

#have changed the crop depth to crop the AOI instead, because the AOI covers the area that crop depth would anyways
#this way no need to add extra layer of cropping and no need to change code yay!
cropdepth =st_read("C:/Users/cormi/Documents/ImageProcessing/bathymetry/AOI_crop.shp")

#import satellite data nc file
#nc.dat = "C:/Users/Cormi/Documents/test/subset_1_of_S2A_MSI_2021_06_14_07_56_37_T36KYU_L2R.nc"
#nc.dat ="C:\\Users\\cormi\\Documents\\test\\L8_OLI_2016_05_08_15_43_25_014042_L2R.nc"
#new file created with fixed DSF fitting rather than tiled which produced negative near-infrared values
nc.dat = "C:/Users/cormi/Documents/ImageProcessing/Reference/L8_OLI_2016_05_08_15_43_25_014042_L2R.nc"
#nc.dat = "D:/output/L8_OLI_2016_02_18_15_43_44_014042_L2R.nc"
#create Output file
write.data = "C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_May2016.tif"
#write.data = "C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Wnter2016.tif"
write.data2 = "C:/Users/cormi/Documents/ImageProcessing/Reference/PreprocessingOuput/Landsat8_May2016x10000.tif"

#createoutputfileforinverse
#write.data = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmask.tif"
#write.data2 = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016_reverselandmaskx10000.tif"

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
#nir1 = t(ncvar_get(nc, nc$var$rhos_839))
#nir = raster(nir1)
#proj4string(nir)  = crs(nc_atts$proj4_string)
#extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])

#added in the 2 swir bands because I think this is what aasg wants
#swir1
#swir11 = t(ncvar_get(nc, nc$var$rhos_1678))
#swir1 = raster(swir11)
#proj4string(swir1)  = crs(nc_atts$proj4_string)
#extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])

#swir2
#swir21 = t(ncvar_get(nc, nc$var$rhos_2217))
#swir2 = raster(swir21)
#proj4string(swir2)  = crs(nc_atts$proj4_string)
#extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])

#Depth Data 
#project depth to same crs as blue
depth = projectRaster(depth, blue, method="bilinear")
depth = mask(depth,cropdepth, inverse=T)
##change all NA to -30 to get rid of NAs in depth file
depth[is.na(depth[])] = -0.1

#had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
blue = crop(blue, depth)
green = crop (green, depth)
red = crop(red, depth)
#nir = crop(nir, depth)
#swir1 = crop(swir1, depth)
#swir2 = crop(swir2, depth)

##stackalllayerstogether
dat.stack = stack(blue,green,red)

##makeroom
rm(blue,green,red)

##name the layers in data stack
names(dat.stack) = c("blue","green","red")


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
#neg.mask = ifelse(blue1<0 | green1<0 | red1<0 , NA, 1)#set to NA if any are negative
#neg.mask = raster(neg.mask)
#proj4string(neg.mask)  = crs(nc_atts$proj4_string)
#extent(neg.mask) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
#neg.mask = crop(neg.mask, depth)
#neg.mask = as.matrix(neg.mask)

#Kristen gave me a new more fool proof way to create negative mask:
#create one layer which is just the minimum value out of all of the data stack layer
#for each of the pixels of the dat stack
neg.mask=min(dat.stack, na.rm=T)
#use the negative mask to remove anypixels that are overlayed with negative pixels in the neg.mask
dat.stack[neg.mask<0]=NA

#continue with water mask as expected

#makeroom
rm(blue1,green1,red1)

##what do the empty square brackets do?
#cloud1 = dat.stack[[1]]
#cloud1[] = cloud
#neg.mask1 = dat.stack[[1]]
#neg.mask1[] = neg.mask

##create final mask
#there witll be a warning about extents not overlapping, but this is fine
#landmask
#project depth to same crs and extent as other layers
water = projectRaster(water, depth, method="bilinear")
water = crop(water,cropdepth)

##remove deep water
offshore=depth<=-5

#remove 0 values
offshoremask <- clamp(offshore, lower=1, useValues=FALSE)

#crop depth
depth<-mask(x=depth, mask=offshoremask, inverse=T)
depth = mask(x=depth, mask = water)

#makeroom
#rm(cloud,neg.mask,cloud1,neg.mask1)
rm(neg.mask,neg.mask1)


#maskdat.stack and crop
#preciously, I used the landmask to mask out land and multiplied it by the 
#negtaive value mask created above and then ran inverse =T
#however, in that case, it would also be masking the inverse of the negative value mask 
#which is a mask that has all positive values listed as 1 and negative as NA
#so it would be masking the inverse of that mask, so would be masking positive values
#so now I changed the mask to a mask of water so that when it is multiplied by the neg mask
#anything that is 1 in the neg. mask (which means it was not negative) is 
#is multiplied by the water mask so that all values in the mask = positive values 
#and water which is what we want to keep

#do not worry about paragraph above, new negative mask code from Kristen removes this issue
dat.stack = mask(x=dat.stack, mask = water)#land, cloud, neg mask
#mask dat.stack with offshore
dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)

#to create reverse land mask
#dat.stack = mask(x=dat.stack, mask = rmaskrs)

##
nc_close(nc)
#makeroom
rm(nc,nc.dat,nc_atts,offshoremask)

##createndvilayer
#ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)

##what is gndvi?
#gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)

#create stack with ndvi and gndvi
#dat.stack = stack(dat.stack,ndvi,gndvi)
#rm(ndvi,gndvi)

##name layers in dat stack
names(dat.stack) = c("blue","green","red")


#Lyzenga WCC layers
raster.dat<-dat.stack[[1:3]]
names(raster.dat) = c("blue2", "green2", "red2")

raster.name.dat= c("blue2","green2","red2")
#only use blue green red (sets number of bands to use to 3)
raster.dim.use = 3 #number of bands to use the WCC
#shape.dat is called shape.in because I only have one shape file with all polygons
shape.in = shapefile("C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/sandWateColumnCorrection_Polygon.shp")
shape.dat = "C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/"

shape.in = spTransform(shape.in, raster.dat@crs)
dat = extract(x=raster.dat,y=shape.in,df=T)
dat = na.omit(dat)
rm(shape.in)

#Generate ln #
dat.ln=dat
dat.ln[,2:(1+length(raster.name.dat))] = log (dat[,2:(1+length(raster.name.dat))])#take natural logarithm
#Calculate variance
sub.dat.variance = apply(dat.ln, 2, var) #calculate variance 
#Generate unique combinations
val.in = expand.grid(names(sub.dat.variance[2]),names(sub.dat.variance[3:(1+raster.dim.use)]))
i=2
{while(i<raster.dim.use){
  i = i+1
  test = expand.grid(names(sub.dat.variance[i]),names(sub.dat.variance[(i+1):(1+raster.dim.use)]))
  val.in = rbind(val.in,test)
  rm(test)}}#generate unqiue DDI
val.in = cbind(val.in,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1]))
               ,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])))
names(val.in) = c("B1","B2","Variance.B1","Variance.B2", "Covariance","a","slope","R2")
val.in[,1] = as.character(val.in[,1])
val.in[,2] = as.character(val.in[,2])
rm(i)
#Calculate covariance
for ( i in 1:length(val.in[,1])){
  val.in$Covariance[i] = cov(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]])
}
#Put variance into val.in
i=1
{while(i<raster.dim.use){
  i = i+1
  test = ifelse(names(sub.dat.variance[i])==val.in$B1,sub.dat.variance[i],NA)
  val.in$Variance.B1 = ifelse(is.na(test)==T,val.in$Variance.B1,test)
}}
i=1
{while(i<=raster.dim.use){
  i = i+1
  test = ifelse(names(sub.dat.variance[i])==val.in$B2,sub.dat.variance[i],NA)
  val.in$Variance.B2 = ifelse(is.na(test)==T,val.in$Variance.B2,test)
  rm(test)
}}
#calculate a
for ( i in 1:length(val.in[,1])){
  val.in$a[i] = (val.in$Variance.B1[i]-val.in$Variance.B2[i])/(2*val.in$Covariance[i]) 
}
#calculate the slope
for ( i in 1:length(val.in[,1])){
  val.in$slope[i] = val.in$a[i] + sqrt((val.in$a[i]^2)+1)
}
#Plot each bi-plot
for ( i in 1:length(val.in[,1])){
  png(paste(shape.dat,"biplot-",val.in[i,1],val.in[i,2],".png",sep=""),pointsize=10,family="serif")
  plot(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]], ylab = paste ("ln ", val.in[i,2],sep=""),
       xlab = paste ("ln ", val.in[i,1],sep=""),pch=19, col=dat.ln[,1])
  test = lm(dat.ln[,val.in[i,2]]~ dat.ln[,val.in[i,1]]) 
  abline(a=  test$coefficients[1],b=test$coefficients[2])
  dev.off()
  val.in$R2[i] = summary(test)$r.squared
  rm(test)}
#Write out the coefficients
write.csv(val.in,paste(shape.dat,"WCC-Coef",".csv",sep="") )
##
rm(dat,dat.ln,sub.dat.variance,i)

#Apply the  water column correction to the visible bands
ras.dat = raster.dat
ras.dat = log(ras.dat)
names(ras.dat) = raster.name.dat
for (i in 1:length(val.in[,1])){
  print(i)
  raster.dat[[i]] = ras.dat[[val.in[i,1]]]-(val.in$slope[i]*ras.dat[[val.in[i,2]]])
}

#add WCC layers to raster stack
dat.stack = stack(dat.stack,raster.dat[[3]],raster.dat[[1]]) 

names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl")

#create pca
pca.stack=dat.stack
pca.dat = rasterPCA(pca.stack[[1:3]], spca=F)
pca.dat = pca.dat$map

dat.stack = stack(dat.stack,pca.dat[[1]])

#try with depth
dat.stack = stack(dat.stack, depth)

names(dat.stack)=c("blue","green","red","lyzgr","lyzgrbl","pca1","depth")

##make the final raster!
writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)

plot(dat.stack)

rm(depth, land, offshore, dat.stack, swir11, swir21, cropdepth)


##what does this do?
#for(i in 1:4){dat.stack[[i]] = dat.stack[[i]]*10000}
#writeRaster(dat.stack, write.data2, format="GTiff",NAflag = NaN, overwrite=T)
##



##do without depth data----
rm(list = ls())
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
write.data = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016sansdepth.tif"
write.data2 = "C:/Users/Cormi/Documents/test/Landsat8_Sept2016x10000sansdepth.tif"

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
dat.stack = stack(blue,green,red,nir)

##makeroom
rm(blue,green,red,nir)

##name the layers in data stack
names(dat.stack) = c("blue","green","red","nir")

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

##remove deep water
offshore=depth<=-30
plot(offshore)
#remove 0 values
offshoremask <- clamp(offshore, lower=1, useValues=FALSE)

#makeroom
rm(cloud,landmask1,neg.mask,cloud1,neg.mask1)



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
names(dat.stack) = c("blue","green","red","nir", "ndvi","gndvi")

##make the final raster!
writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)

##

