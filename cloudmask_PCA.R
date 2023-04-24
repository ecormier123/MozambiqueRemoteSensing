rm(list=ls())
library(raster)
library(ncdf4)
library(rgdal)

sat.images = list.files("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
#sat.images = list.files("D:/preprocessoutputcloudy")

for (ii in 1:length(sat.images)){
  
  in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
  sat.images.list = (list.files(in.folder))
  
  print(paste("Running ", ii, " of ", length(sat.images.list), " in satellite images list", sep = ""))
  
  #import satellite data nc file
  cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
  cloudy = stack(cloudy.sat)
  ##
  
  if (sat.images.list[ii] == "L5_TM_2005_06_27_15_31_27_014042_L2R.nc.tif") {
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloud_mask2005_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L5_TM_2005_06_27_15_31_27_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  } else if (sat.images.list[ii] == "L7_ETM_2001_04_05_15_33_53_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_2001_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L7_ETM_2001_04_05_15_33_53_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)   
    
  }else if (sat.images.list[ii] == "L5_TM_2008_07_21_15_30_30_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_2008_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L5_TM_2008_07_21_15_30_30_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L8_OLI_2013_03_20_15_44_46_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_2013v2_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L8_OLI_2013_03_20_15_44_46_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L5_TM_2009_02_14_15_29_42_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_200902_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L5_TM_2009_02_14_15_29_42_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L5_TM_2009_07_24_15_32_50_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_200907_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L5_TM_2009_07_24_15_32_50_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L5_TM_2011_02_20_15_33_49_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201102_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L5_TM_2011_02_20_15_33_49_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L5_TM_2011_07_14_15_32_59_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201107_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L5_TM_2011_07_14_15_32_59_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L8_OLI_2015_02_15_15_43_39_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201502_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L8_OLI_2015_02_15_15_43_39_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L8_OLI_2016_09_29_15_44_04_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201609_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L8_OLI_2016_09_29_15_44_04_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L8_OLI_2017_02_04_15_43_51_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201702_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L8_OLI_2017_02_04_15_43_51_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L8_OLI_2018_10_21_15_43_43_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201810_v2_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L8_OLI_2018_10_21_15_43_43_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  }else if (sat.images.list[ii] == "L8_OLI_2019_06_02_15_43_35_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_201906_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L8_OLI_2019_06_02_15_43_35_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
  } else if (sat.images.list[ii] == "L9_OLI_2022_05_01_15_43_35_014042_L2R.nc.tif"){
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    cloudy = stack(cloudy.sat)
    clouds = readOGR("C:/Users/cormi/Documents/ImageProcessing/Clouds/cloudmask_202205_Polygon.shp")
    clouds = spTransform(x=clouds, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs' )
    
    
    
    #cloudymask<-mask(x = cloudy, mask = clouds, inverse =T)
    dat.stack<- mask(x = cloudy, mask = clouds, inverse =T)
    
    #create pca
    #pca.stack = cloudymask
    #pca.dat = rasterPCA(pca.stack[[1:4]], spca=F)
    #pca.dat = pca.dat$map
    
    #dat.stack = stack(cloudymask, pca.dat[[1]])
    #dat.stack = subset(dat.stack,order(c(1,2,3,4,5,7,6)))
    #names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth/L9_OLI_2022_05_01_15_43_35_014042_L2R.nc")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
    
    
  } else {
    
    in.folder = ("D:/preprocessoutputcloudy/bgr-lyzgr-lyzgrbl-depth")
    sat.images.list = (list.files(in.folder))
    
    cloudy.sat = paste(in.folder, "/", sat.images.list[ii], sep = "")
    dat.stack = stack(cloudy.sat)
    #rm swir 1, as it is not consistent between years
    names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl","depth")
    #names(cloudy) = c("blue","green","red", "swir1","ndvi","cmri")
    
    write.data = paste("D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth", "/",sat.images.list[ii], sep="")
    writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
    rm(cloudy.sat,cloudy,pca.stack,pca.dat,dat.stack,clouds)
  
  }
  
} 

