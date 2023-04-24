library("raster")
library("caret")
#import classified map
wv.dat=raster("D:/AASGoutput/bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/L8_OLI_2015_02_15_15_43_39_014042_L2R.nc.tif")
#read in training data ( these points were made in arcgis then converted to csv via online shp to csv converter)
mydata=read.csv("D:/confusion matrix/winter2015.csv", )
#name the columns of my data
colnames(mydata)<-c("w","n","class","nursery")
#for the felicie data, where all coordinates are in lat long but my maps are in utm
#state what data is the coordinates
coordinates(mydata) <- c("w", "n")
#set current projection to long
proj4string(mydata) <- CRS("+proj=longlat +datum=WGS84") 
# change the projection to utm
mydata <- spTransform(mydata, CRS("+proj=utm +zone=17 ellps=WGS84"))
#also might need to remove certain rows for categories no longer predicted (eg. sargasum class in 2003 image)
#select specific rows of data
#mydata=mydata[26:163,]
#mydata=mydata[mydata[,3]<4,]
#or select specific values ( in this case removes any values points of greater than 4
#for 2014 image

#extract predicted data (what the final classification said was found at these locations)
test = extract(wv.dat, mydata[,2:1],df=T)
colnames(test)<-c("ID","class")
#change all 3 values to 2 for felicie data, because I only have classes of seagrass or sand, no high density seagrass

#bind the column that contains the habitat ID from your training data, have to bind the two before removing rows or else rows won't match up in confusion matrix
#matrixtest=cbind(mydata$Id,test$category)
#for felicie data
matrixtest=cbind(mydata$class,test$class)
colnames(matrixtest)<-c("training","predicted")
#remove na values that appear because map was cropped to exclude some training data
matrixtest=na.omit(matrixtest)

#step
#in some cases the habitat training data is not as in depth as the actual classification 
#(for eg. data from 1999 was only classified into sand or seagrass ) so ypou have to reduce #
#of classes from classified image (change all dense or medium density seagrass
#so that it is equivalent to just seagrass)
#matrixtest[,2][test$category==3]<-2
#for matrix with na
matrixtest[,2][matrixtest[,2] == 3] <- 2
#for 2020 map training data, change the high and mid-density seagrass to 
#just equal 'seagrass'
#mydata$Id[mydata$Id==3]<-2

#run confusion matrix (both classes must be factors)
confusionMatrix(reference = as.factor(mydata$Id), data = as.factor(test$category))
#for data with nas
confusionMatrix(reference = as.factor(matrixtest[,1]), data = as.factor(matrixtest[,2]))


rm(list=ls())
