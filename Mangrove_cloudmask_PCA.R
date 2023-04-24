rm(list=ls())
library(raster)
library(ncdf4)
library(rgdal)
library("RStoolbox")

sat.images = list.files("D:/preprocessmangrovecloudy")

for (ii in 1:length(sat.images)){
  
  in.folder = "D:/preprocessmangrovecloudy"
  sat.images.list = (list.files(in.folder))

  print(paste("Running ", ii, " of ", length(sat.images.list), " in satellite images list", sep = ""))

  images = sat.images.list[ii]
  
  ##
  
  if (images == "L5_TM_2005_06_27_15_31_27_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloud_mask2005_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2004-2006.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2005_06_27_15_31_27_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  } else if (images == "L7_ETM_2001_04_05_15_33_53_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_2001_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-1999-2001.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    #names(dat.stack) = c("blue","green","red", "swir1","ndvi","cmri","pca1")
    write.data = paste("D:/preprocessoutputmangrove/L7_ETM_2001_04_05_15_33_53_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L5_TM_2008_07_21_15_30_30_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_2008_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2008-2009.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2008_07_21_15_30_30_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L8_OLI_2013_03_20_15_44_46_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_2013v2_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L8-2013-2014.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2013_03_20_15_44_46_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L5_TM_2009_02_14_15_29_42_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_200902_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2008-2009.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2009_02_14_15_29_42_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L5_TM_2009_07_24_15_32_50_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_200907_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2008-2009.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2009_07_24_15_32_50_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L5_TM_2011_02_20_15_33_49_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201102_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2010-2011.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2011_02_20_15_33_49_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L5_TM_2011_07_14_15_32_59_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201107_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2010-2011.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2011_07_14_15_32_59_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L8_OLI_2015_02_15_15_43_39_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201502_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L8-2015-201602.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2015_02_15_15_43_39_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L8_OLI_2016_09_29_15_44_04_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201609_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    cloudymask <- mask(x = cloudy, mask = clouds, inverse =T)
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L8-201609.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudymask, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2016_09_29_15_44_04_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L8_OLI_2017_02_04_15_43_51_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201702_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2017_02_04_15_43_51_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L8_OLI_2018_10_21_15_43_43_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201810_v2_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2018_10_21_15_43_43_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images == "L8_OLI_2019_06_02_15_43_35_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201906_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2019_06_02_15_43_35_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images =="L5_TM_1999_03_23_15_22_43_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-1999-2001.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_1999_03_23_15_22_43_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if ( images =="L5_TM_1999_08_14_15_21_27_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-1999-2001.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_1999_08_14_15_21_27_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if ( images =="L5_TM_2000_04_26_15_19_04_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-1999-2001.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2000_04_26_15_19_04_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images =="L7_ETM_2000_02_14_15_36_34_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-1999-2001.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L7_ETM_2000_02_14_15_36_34_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if ( images == "L5_TM_2001_02_08_15_23_52_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-1999-2001.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2001_02_08_15_23_52_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images =="L5_TM_2002_02_27_15_22_08_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2002.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2002_02_27_15_22_08_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)
    
  }  else if (images =="L7_ETM_2003_02_22_15_32_30_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L7-2003.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L7_ETM_2003_02_22_15_32_30_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data) 
    
  }  else if (images =="L7_ETM_2003_04_27_15_32_36_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L7-2003.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L7_ETM_2003_04_27_15_32_36_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data) 
    

  } else if (images =="L5_TM_2004_01_16_15_22_36_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-200401.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2004_01_16_15_22_36_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)   
  
  } else if (images =="L5_TM_2004_09_28_15_27_59_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2004-2006.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2004_09_28_15_27_59_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)  
    
  } else if (images == "L5_TM_2006_02_06_15_34_11_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2004-2006.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2006_02_06_15_34_11_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)  
    
  } else if (images == "L5_TM_2006_09_18_15_37_41_014042_L2R.ncmangrove.tif") {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2004-2006.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2006_09_18_15_37_41_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data) 
    

  } else if (images =="L5_TM_2007_02_09_15_38_50_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2007.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2007_02_09_15_38_50_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data) 
    
  } else if ( images == "L5_TM_2007_04_30_15_38_30_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2007.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2007_04_30_15_38_30_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data)  
    
  } else if (images =="L5_TM_2010_12_18_15_33_43_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L5-2010-2011.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L5_TM_2010_12_18_15_33_43_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data) 
    
  } else if (images =="L8_OLI_2014_01_11_15_45_05_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L8-2013-2014.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2014_01_11_15_45_05_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data) 
    
  } else if (images =="L8_OLI_2016_02_18_15_43_44_014042_L2R.ncmangrove.tif" ) {
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    water = readOGR("C:/Users/cormi/Documents/ImageProcessing/landmask/L8-2015-201602.shp")
    water = spTransform(x=water, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = water, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L8_OLI_2016_02_18_15_43_44_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    rm(cloudy.sat,cloudy,dat.stack,clouds,cloudmask,water,dat.stack,write.data) 
    
    
  } else if (images =="L9_OLI_2022_05_01_15_43_35_014042_L2R.ncmangrove.tif" ){
    
    in.folder = "D:/preprocessmangrovecloudy"
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(cloudy) = c("blue","green","red", "ndvi","cmri")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_202205_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    dat.stack <- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #make without pca or swir as they vary between images and years
    names(dat.stack) = c("blue","green","red","ndvi","cmri")
    write.data = paste("D:/preprocessoutputmangrove/L9_OLI_2022_05_01_15_43_35_014042_L2R.ncmangrove")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
}else{
  
  in.folder = "D:/preprocessmangrovecloudy"
  sat.images.list = (list.files(in.folder))
  
  cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
  dat.stack = stack(cloudy.sat)
  #rm swir 1, as it is not consistent between years
  names(dat.stack) = c("blue","green","red", "ndvi","cmri")
  #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
  
  write.data = paste("D:/preprocessoutputmangrove", "/",sat.images.list[ii], sep="")
  writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)

  }    

    } ##
