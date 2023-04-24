rm(list=ls())
library("rgdal")
library("raster")
library("sf")
library("terra")


#out.folder = "D:/preprocessoutput"
out.folder = "D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth"
sat.images.list2 = (list.files(out.folder))
dir.create("D:/AASGoutput")
#sat.images.list2  = sat.images.list2[-1]

for (ii in 1:length(sat.images.list2)){

  source("C:/Users/cormi/Downloads/Automatic_Adaptive_Signature_Generalization_in_R.txt")
  out.folder = "D:/preprocessoutput/bgr-lyzgr-lyzgrbl-depth"
  sat.images.list2 = (list.files(out.folder))
  #sat.images.list2  = sat.images.list2[-1]
  
  #Read in raster dataset for reference image
  image1 = brick("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/L8_OLI_2016_05_08_15_43_25_014042_L2Rbgr-lyzgr-lyzgrbl-depth.nc.tif")
  #had to crop the image by the AOI crop to make sure that extents matched
  cropdepth =readOGR("C:/Users/cormi/Documents/ImageProcessing/bathymetry/AOI_crop2_Polygon.shp")
  cropdepth = spTransform(cropdepth, crs(image1))
  image1=crop(image1, cropdepth)
  #for some reason crop does not work with the new crop... so am going to mask as well
  image1=mask(image1, cropdepth)
  
  #name each layer
  #names(image1) = c("b","g","r","n","sw1", "sw2", "depth","ndvi","gndvi")
  #read in processed reference image
  #refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/outras_7class_lyzgr_depth.tif")
  #refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/outras_7class_lyzgr_depth_nosand_mid-high.tif")
  refcl = raster("C:/Users/cormi/Documents/ImageProcessing/Reference/outras_class_bgr-lyzgr-lyzgrbl-depth_sand-ldsg.tif")
  refcl = crop(refcl, cropdepth)
  refcl = mask(refcl, cropdepth)
  #read in raster image to be classified
  
  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  
  image2 = brick (paste(out.folder, "/", sat.images.list2[ii], sep = ""))
  #image2 = brick ("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_May.tif")
  #had to crop the image by the AOI crop to make sure that extents matched
  image2=crop(image2, cropdepth)
  image2=mask(image2, cropdepth)
  
  
  write.map = paste("D:/AASGoutput", "/", "bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite", "/", sat.images.list2[ii], sep="")
  write.data = paste("D:/AASGoutput", "/", "bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite", "/", sat.images.list2[ii],"area.csv", sep="")
  write.rf = paste("D:/AASGoutput", "/", "bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite", "/", sat.images.list2[ii],"rf.csv", sep="")
  
  #create stable sites, change the dif.layer you want to be used to classify stable sites 
  #eg. if you choose blue, it willl identify which areas have not change blue colour and identify those
  #as stable, red does not work well for water, change the c valeu to the amount of standard
  #deviations away from the originalvalue you want to consider 'stable, 0.25 os fairly low
  #in this case 5 is the lyzenga green-blue DII
  #going to try green insteas as stable sites
  StableSites = make.mask(image1, image2, refcl, c=0.25, dif.layer1 = 2, dif.layer2 = 2)

  #run aasg algorithm
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
  NS_Crop<-readOGR("C:/Users/cormi/Documents/ImageProcessing/Seagrass/NorthSound.shp")
  NS_Crop<-spTransform(x=NS_Crop, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs')
  
  #import SL crop polygon
  SL_Crop<-readOGR("C:/Users/cormi/Documents/ImageProcessing/Seagrass/SharkLand.shp")
  SL_Crop<-spTransform(x=SL_Crop, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs')
  
  #import south bimini crop polygon
  SB_Crop<-readOGR("C:/Users/cormi/Documents/ImageProcessing/Seagrass/SouthBiminiSea.shp")
  SB_Crop<-spTransform(x=SB_Crop, CRSobj = '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs')
  
  #crop the classification by NS and SL polygons to be able to calculate area of classes within these specific polygons  
  NS<-crop(Classification,NS_Crop)
  SL<-crop(Classification,SL_Crop)
  SB<-crop(Classification,SB_Crop)

  #count the frequency of each class type (in number of cells)
  fNS<-freq(NS)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  NSp<-data.frame(fNS, aNS=(fNS[,2]*.09))
  NSp<-NSp[,-2]
  #add column of area of classes to the original data frame
  #this code takes the two data frames and merges them together by a common column ( in this case 'value')
  #all.x makes it so that all of the columns will be included even if they are NA, so if
  # one area of the map does not have all habitat types, you can still include all columns 
  #representing each habitat type, but that habitat 
  # type column will just appear with an NA value
  p<-merge(p, NSp, by = "value", all.x =TRUE)
  #p<-cbind(p,NSp[,3])
  
  fSL<-freq(SL)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  SLp<-data.frame(fSL, aSL=(fSL[,2]*.09))
  SLp<-SLp[,-2]
  #add column of area of classess in SL to original data frame
  p<-merge(p, SLp, by = "value", all.x =TRUE)
  #p<-cbind(p,SLp[,3])
  
  fSB<-freq(SB)
  #add a new column that calculates the total area of each class in km2
  #I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
  SBp<-data.frame(fSB, aSB=(fSB[,2]*.09))
  SBp<-SBp[,-2]
  #add column of area of classes to the original data frame
  p<-merge(p, SBp, by = "value", all.x =TRUE)
  #p<-cbind(p,SBp[,3])
  
  colnames(p)<-c('Class','Count','areakm2','NSareakm2','SLareakm2','SBareakm2')
  
  write.csv(p, write.data)
  
  rm(list = ls())
  
  
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
