rm(list=ls())
library("rgdal")
library("raster")
source("C:/Users/cormi/Downloads/Automatic_Adaptive_Signature_Generalization_in_R.txt")

#Read in raster dataset for reference image
image1 = brick("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016.tif")
#expects 6 band image, need to remove some layers maybe? (depth, ndvi, gndvi)
image1<-dropLayer(image1,9)
image1<-dropLayer(image1,8)
image1<-dropLayer(image1,7)


#name each layer
#names(image1) = c("b","g","r","n","sw1", "sw2", "depth","ndvi","gndvi")
#read in processed reference image
refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.tif")
#read in raster image to be classified, make sure it also only has 6 bands
image2 = brick("D:/SeptMay/Landsat9_May2022.tif")

#create stable sites
StableSites = make.mask(image1, image2, refcl)

#run aasg algorithm
Output_2022<-aasg(image1, refcl, image2, c = NULL, dem = NULL, method = "RF", probs = FALSE, StableSites = StableSites)

writeRaster(Output_2022$Classification, "D:/SeptMay/2022.class.aasg.tif", format = "GTiff", 
            overwrite = TRUE)

class2022=raster("D:/SeptMay/2022.class.aasg.tif")

#examine rf model 
Output_2022$rf
rf=Output_2022$rf
dotchart(sort(rf$importance[,(dim(rf$importance)[2]-1)]))
