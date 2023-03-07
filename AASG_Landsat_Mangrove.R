rm(list=ls())
library("rgdal")
library("raster")
source("C:/Users/cormi/Downloads/Automatic_Adaptive_Signature_Generalization_in_R.txt")

#Read in raster dataset for reference image
image1 = brick("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif")

#name each layer
#names(image1) = c("b","g","r","n","sw1", "sw2", "depth","ndvi","gndvi")
#read in processed reference image
refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.tif")
#read in raster image to be classified, make sure it also only has 6 bands

out.folder = "D:/preprocessoutputmangrove"
sat.images.list2 = (list.files(out.folder))
#sat.images.list2  = sat.images.list2[-1]

for (ii in 35:length(sat.images.list2)){

  out.folder = "D:/preprocessoutputmangrove"
  sat.images.list2 = (list.files(out.folder))
  #sat.images.list2  = sat.images.list2[-1]
  source("C:/Users/cormi/Downloads/Automatic_Adaptive_Signature_Generalization_in_R.txt")
  
  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  
  #Read in raster dataset for reference image
  image1 = brick("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif")
  
  #name each layer
  #names(image1) = c("b","g","r","n","sw1", "sw2", "depth","ndvi","gndvi")
  #read in processed reference image
  refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.tif")
  #read in raster image to be classified, make sure it also only has 6 bands
  
  
  image2 = brick (paste(out.folder, "/", sat.images.list2[ii], sep = ""))
  
  write.map = paste("D:/AASGoutputmangrove", "/", sat.images.list2[ii], sep="")
  write.data = paste("D:/AASGoutputmangrove", "/", sat.images.list2[ii],"manarea.csv", sep="")
  write.rf = paste("D:/AASGoutputmangrove", "/", sat.images.list2[ii],"manrf.csv", sep="")
  
  #create stable sites
  StableSites = make.mask(image1, image2, refcl)

  #run aasg algorithm
  Output<-aasg(image1, refcl, image2, c = NULL, dem = NULL, method = "RF", probs = FALSE, StableSites = StableSites)

  writeRaster(Output$Classification, write.map, format = "GTiff", 
            overwrite = TRUE)
  
  #write out results of random forest classifier
  rf<-capture.output(Output$rf)
  write.csv(rf, write.rf)
  
  #count the frequency of each class type (in number of cells)
  f<-freq(Output$Classification)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  p<-data.frame(f, a=(f[,2]*.09))
  
  Classification<-Output$Classification
  #import NS crop polygon
  NB_Crop<-readOGR("C:/Users/cormi/Documents/ImageProcessing/Seagrass/NorthBimini.shp")
  NB_Crop<-spTransform(x=NB_Crop, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs')
  
  #import SL crop polygon
  EB_Crop<-readOGR("C:/Users/cormi/Documents/ImageProcessing/Seagrass/EastBimini.shp")
  EB_Crop<-spTransform(x=EB_Crop, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs')
  
  #import south bimini crop polygon
  SB_Crop<-readOGR("C:/Users/cormi/Documents/ImageProcessing/Seagrass/SouthBiminiMan.shp")
  SB_Crop<-spTransform(x=SB_Crop, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs')
  
  #crop the classification by NS and SL polygons to be able to calculate area of classes within these specific polygons  
  NB<-crop(Classification,NB_Crop)
  EB<-crop(Classification,EB_Crop)
  SB<-crop(Classification,SB_Crop)
  
  #count the frequency of each class type (in number of cells)
  fNB<-freq(NB)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  NBp<-data.frame(fNB, aNB=(fNB[,2]*.09))
  #add column of area of classes to the original data frame
  p<-cbind(p,NBp[,3])
  
  fEB<-freq(EB)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  EBp<-data.frame(fEB, aEB=(fEB[,2]*.09))
  #add column of area of classess in SL to original data frame
  p<-cbind(p,EBp[,3])
  
  fSB<-freq(SB)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  SBp<-data.frame(fSB, aSB=(fSB[,2]*.09))
  #add column of area of classes to the original data frame
  p<-cbind(p,SBp[,3])
  
  colnames(p)<-c('Class','Count','areakm2','NBareakm2','EBareakm2','SBareakm2')
  
  write.csv(p, write.data)
  
  rm(f,p,Output,write.map, write.data, StableSites,image2,rf,write.rf)
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
