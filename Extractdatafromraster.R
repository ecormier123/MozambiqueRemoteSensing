rm(list=ls())
library("raster")


#import csv data points of in situ habitat
mydata=read.csv("C:/Users/cormi/Documents/test/Habitat Data_MarianaFuentes_2016 .csv")
#transform csv into shapefile
data.shp = SpatialPointsDataFrame(coords=mydata[,3:2],data=mydata, proj4string=CRS("+proj=longlat +datum=WGS84 +units=m +no_defs"))
#import raster stack
pca6 = stack("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016.tif")
#name layers of rasterstack
names(pca6) = c("b","g","r","n","sw1", "sw2", "depth","ndvi","gndvi")
#change crs of points to those of raster stack
shp_utm <- spTransform(data.shp, crs(pca6))
#extract values from raster
test = extract(pca6, shp_utm,df=T)
#create matrix of original csv data points and extracted data
mydata = cbind(mydata, test[,2:10])
#rm na data points
mydata2=na.omit(mydata)
head(test)
rm(test)
##

write.csv(mydata2,"C:/Users/cormi/Documents/ImageProcessing/Reference/InSituData/Trainingdata.csv", row.names = F)

