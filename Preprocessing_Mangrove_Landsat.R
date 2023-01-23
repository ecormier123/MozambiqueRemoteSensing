rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
library("sf")
library("RStoolbox")



#new file created with fixed DSF fitting rather than tiled which produced negative near-infrared values
nc.dat = "C:/Users/cormi/Documents/ImageProcessing/Reference/L8_OLI_2016_05_08_15_43_25_014042_L2R.nc"

#createoutputfileforinverse
write.data = "C:/Users/Cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif"
write.data2 = "C:/Users/Cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif"

#import created landmask
land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")

####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
nc = nc_open(nc.dat)
nc_atts <- ncatt_get(nc, 0)
##
blue1 = t(ncvar_get(nc, nc$var$rhos_483))
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

#added in the swir bands because I think this is what aasg wants
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

#landmask
#project land to same crs and extent as other layers
land = projectRaster(land, blue, method="bilinear")

#same process with cloud is followed for neg mask
#switched nagetive mask to opporite so that it can work with inverse land mask, now all negative pixels will be 1
neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative

#makeroom
rm(blue1,green1,red1,nir1,swir11,swir21)

##what do the empty square brackets do?
#cloud1 = dat.stack[[1]]
#cloud1[] = cloud
neg.mask1 = blue
neg.mask1[] = neg.mask

#create mask
rmaskrs = neg.mask1*land
plot(rmaskrs)

#makeroom
rm(neg.mask,neg.mask1,land)

##createndvilayer
ndvi = (nir-red)/(nir+red)

##mangrove index (cmri(file:///C:/Users/cormi/Downloads/JARS-190642_online.pdf)
cmri = ((nir-red)/(nir+red))-((nir-green)/(nir+green))

#mangrove vegetation index (mvi https://www.sciencedirect.com/science/article/pii/S0924271620301519?casa_token=ObaJW3vJD3cAAAAA:yPTdiC0JGAx7YRCLuC_OIDEhZx39S86ec-dEhvZzkmbtkbxe51DUOH4aHU51p-_MdTmRpEkh)
#mvi=((nir-green)/(swir1-green))
#tried using this but just got a blank raster...

#create pca
pca.stack=stack(blue,green,red,nir,swir1,swir2,ndvi,cmri)
pca.stack = mask(x=pca.stack, mask = rmaskrs)
pca.dat = rasterPCA(pca.stack[[1:5]], spca=F)
pca.dat = pca.dat$map

#stack all layers together
dat.stack = stack(blue, green, swir1, ndvi, cmri, pca.dat[[1]])

##name layers in dat stack
names(dat.stack) = c("blue","green","swir1","ndvi", "cmri", "pca1")

rm(blue,green,red,nir,swir1,ndvi,cmri,pca.dat)

##cloud mask written by kristen
#create map for cloud mask of layer 1373, because now the other data is cropped and this is not

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

#to create reverse land mask
dat.stack = mask(x=dat.stack, mask = rmaskrs)

##
nc_close(nc)
#makeroom
rm(nc,nc.dat,nc_atts,rmaskrs)

##make the final raster!
writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)

##
plot(dat.stack)
#to create k means classification
#rmeove na values that exist due to masking
dat.stack[is.na(dat.stack)] <- -0.999
#create kmeans class https://www.gis-blog.com/unsupervised-kmeans-classification-of-satellite-imagery-using-r/
kMeansclass<-kmeans(dat.stack[],centers = 5)
#create a dummy raster using the first layer of our image 
#and replace the values of the dummy raster with the clusters (classes) of the kMeans classification
result <- raster(dat.stack[[1]])
result <- setValues(result, kMeansclass$cluster)
result = mask(x=result, mask=land)
#for 5 class
plot(result, col=c("lightgreen", "grey","lightblue","darkgreen", "white","green"))
#for 6 class
plot(result, col=c("lightgreen", "beige","green","grey", "darkgreen","white"))
