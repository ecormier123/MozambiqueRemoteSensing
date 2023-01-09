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

out.folder = "D:/finaloutput"
sat.images.list2 = (list.files(out.folder))
dir.create("D:/finalfinaloutput")
#sat.images.list2  = sat.images.list2[-1]

for (ii in 1:length(sat.images.list2)){

  out.folder = "D:/finaloutput"
  sat.images.list2 = (list.files(out.folder))
  #sat.images.list2  = sat.images.list2[-1]
  
  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  
  image2 = brick (paste(out.folder, "/", sat.images.list2[ii], sep = ""))
  
  write.data = paste("D:/finalfinaloutput", "/", sat.images.list2[ii], sep="")

  #create stable sites
  StableSites = make.mask(image1, image2, refcl)

  #run aasg algorithm
  Output<-aasg(image1, refcl, image2, c = NULL, dem = NULL, method = "RF", probs = FALSE, StableSites = StableSites)

  writeRaster(Output$Classification, write.data, format = "GTiff", 
            overwrite = TRUE)
}

#class2022=raster("D:/SeptMay/2022.class.aasg.tif")

#examine rf model 
Output_2022$rf
rf=Output_2022$rf
dotchart(sort(rf$importance[,(dim(rf$importance)[2]-1)]))

#compare the 2016 image class to the 2022 image class

library(RColorBrewer)

#sets the divisions for colour scheme
cuts=c(1,1.5,2,2.5,3) #set breaks

#names the colours to be used in the scheme, in this order
#to choose colours, type, colors() and  a list will appear, or just google it
pal <- colorRampPalette(c("lightgreen","forestgreen", "cornsilk", "cornsilk"))

#plot the map using above colour scheme and breaks, set NA colour to grey
plot(class2022, breaks=cuts, col = pal(5), colNA="grey")
plot(class2016, breaks=cuts, col = pal(5), colNA="grey")
