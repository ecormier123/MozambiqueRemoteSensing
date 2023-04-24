rm(list=ls())
gc()
library("rgdal")
library("raster")
source("C:/Users/cormi/Downloads/Automatic_Adaptive_Signature_Generalization_in_R.txt")

#Read in raster dataset for reference image
#forlandsat8/9
image1 = brick("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif")
#to remove the pca and swir layers as they don't stay consistent across years
image1=stack(image1[[1:3]],image1[[5:6]])
#for landsat 5
#image1 = stack("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/L5_TM_2007_04_30_15_38_30_014042_L2R.ncmangrove.tif")
#for landsat 7
#image1 = stack("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/L7_ETM_2000_02_14_15_36_34_014042_L2R.ncmangrove.tif")

#name each layer
#names(image1) = c("b","g","r","n","sw1", "sw2", "depth","ndvi","gndvi")
#read in processed reference image
#refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.tif")
#refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.nolakes_sand-dev.tif")
#refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.nolakes_sand-dev-barren.tif")
#refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.nolakes.nobarren_sand-dev-barren.tif")
#forlandsat 8/9
refcl=raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove-2016-nolakes_sand-dev-veg-nopca-noswir-final.tif")
#for landsat 5
#refcl=raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove-200704L5-nolakes_sand-dev-veg-nopca-noswir-final.tif")
# for landsat 7
#refcl=raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove-200002L7-nolakes_sand-dev-veg-nopca-noswir-final.tif")


#read in reference image with no lakes class and with sand and developed combined

#read in raster image to be classified, make sure it also only has 6 bands

out.folder = "D:/preprocessoutputmangrove"
sat.images.list2 = (list.files(out.folder))
#sat.images.list2  = sat.images.list2[-1]

#for landsat 8
#for(ii in 21:35){

#for landsat 5
#for (ii in 1:17){

#for landsat 7

#going to just use one year to classify to stay consistent with seagrass map methods
for (ii in 1:length(sat.images.list2)){
  

  out.folder = "D:/preprocessoutputmangrove"
  sat.images.list2 = (list.files(out.folder))
  #sat.images.list2  = sat.images.list2[-1]
  source("C:/Users/cormi/Downloads/Automatic_Adaptive_Signature_Generalization_in_R.txt")
  
  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  
  #forlandsat8/9
  image1 = brick("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif")
  #to remove the pca and swir layers as they don't stay consistent across years
  image1=stack(image1[[1:3]],image1[[5:6]])
  #for landsat 5
  #image1 = stack("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/L5_TM_2007_04_30_15_38_30_014042_L2R.ncmangrove.tif")
  #for landsat 7
  #image1 = stack("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/L7_ETM_2000_02_14_15_36_34_014042_L2R.ncmangrove.tif")
  
  #name each layer
  #names(image1) = c("b","g","r","n","sw1", "sw2", "depth","ndvi","gndvi")
  #read in processed reference image
  #refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.tif")
  #refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.nolakes_sand-dev.tif")
  #refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove.nolakes_sand-dev-barren.tif")
  #forlandsat 8/9
  refcl=raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove-2016-nolakes_sand-dev-veg-nopca-noswir-final.tif")
  #for landsat 5
  #refcl=raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove-200704L5-nolakes_sand-dev-veg-nopca-noswir-final.tif")
  # for landsat 7
  #refcl=raster("C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove-200002L7-nolakes_sand-dev-veg-nopca-noswir-final.tif")
  
  #read in raster image to be classified, make sure it also only has 6 bands
  
  
  image2 = brick (paste(out.folder, "/", sat.images.list2[ii], sep = ""))
  #trying without PCA layer as PCA layer may not be that telling as ti only states variation in image... 
  #variation may not be the same across years
  #for withoutpca
  #image2 = image2[[1:6]]
  
  write.map = paste("D:/AASGoutputmangrove/c.25_ndvistablesite/", "/", sat.images.list2[ii], sep="")
  write.data = paste("D:/AASGoutputmangrove/c.25_ndvistablesite/", "/", sat.images.list2[ii],"manarea.csv", sep="")
  write.rf = paste("D:/AASGoutputmangrove/c.25_ndvistablesite/", "/", sat.images.list2[ii],"manrf.csv", sep="")
  
  
  
  #create stable sites
  #StableSites = make.mask(image1, image2, refcl)

  #run aasg algorithm, SVI = false means veg indices are not run
  #set c=.25 as this states how many SD the class can be from the original image to be considered 'stable'
  #.25 is fairly low I believe...
  #using dif.layer =4 to say that stable sites will be determined based on ndvi
  StableSites = make.mask(image1, image2, refcl, c=0.25, dif.layer1 = 4, dif.layer2 = 4)
  Output<-aasg(image1, refcl, image2, StableSites = StableSites, dem = NULL, method = "RF", probs = FALSE, svi=FALSE)

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
  #p<-cbind(p,NBp[,3])
  
  NBp<-NBp[,-2]
  #add column of area of classess in SL to original data frame
  p<-merge(p, NBp, by = "value", all.x =TRUE)
  #p<-cbind(p,SLp[,3])
  
  fEB<-freq(EB)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  EBp<-data.frame(fEB, aEB=(fEB[,2]*.09))
  #add column of area of classess in SL to original data frame
  #p<-cbind(p,EBp[,3])
  EBp<-EBp[,-2]
  #add column of area of classess in SL to original data frame
  p<-merge(p, EBp, by = "value", all.x =TRUE)
  #p<-cbind(p,SLp[,3])
  
  fSB<-freq(SB)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  SBp<-data.frame(fSB, aSB=(fSB[,2]*.09))
  #add column of area of classes to the original data frame
  #p<-cbind(p,SBp[,3])
  SBp<-SBp[,-2]
  #add column of area of classess in SL to original data frame
  p<-merge(p, SBp, by = "value", all.x =TRUE)
  #p<-cbind(p,SLp[,3])
  
  colnames(p)<-c('Class','Count','areakm2','NBareakm2','EBareakm2','SBareakm2')
  
  write.csv(p, write.data)
  
  rm(list=ls())
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
