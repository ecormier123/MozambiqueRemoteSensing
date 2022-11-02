
library("raster")

setwd("C:\\Users\\cormi\\Documents\\test")


#import csv data points of habitat
mydata=read.csv("C:\\Users\\cormi\\Documents\\test\\Habitat Data_MarianaFuentes_2016 .csv")
#transform csv into shapefile
data.shp = SpatialPointsDataFrame(coords=mydata[,3:2],data=mydata, proj4string=CRS("+proj=longlat +datum=WGS84 +units=m +no_defs"))
#import raster stack
pca6 = stack("Landsat8_Sept2016_copy.tif")
#name layers of rasterstack
names(pca6) = c("b","g","r","n", "depth","ndvi","gndvi")
#change crs of points to those of raster stack
shp_utm <- spTransform(data.shp, crs(pca6))
#extract values from raster
test = extract(pca6, shp_utm,df=T)
#create matrix of original csv data points and extracted data
mydata = cbind(mydata, test[,2:7])
head(test)
rm(test)
##

write.csv(mydata,"Trainingdata.csv", row.names = F)

