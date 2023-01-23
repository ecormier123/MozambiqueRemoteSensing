rm(list = ls())
library("raster")


setwd("C:/Users/cormi/Documents/ImageProcessing")
raster.dat = brick("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_May2016.tif")
names(raster.dat) = c("blue","green","red","nir","swir1", "swir2","depth", "ndvi","gndvi")

#next chunk of code no longer necessary as I only need to create relationships between red, green and blue band
#this code looks at the relationships between the log transformed red green and blue values to createc oefficients
#that aare then used to create maps of the redvsgreen, redvsblue and bluevsgreen to correct for change in reflectance of water with change in depth
#have to reclassify depth values to remove positive values in depth
# reclassify the values into three groups 
# all values > 0 and <= 10 become 0
#m <- c(0, 10, 0)
#create a matrix of the above to entire into code below
#rclmat <- matrix(m, ncol=3, byrow=TRUE)
#transform the raster data layer for depth (7)
#raster.dat[[7]]<-reclassify(raster.dat[[7]],rclmat, right =FALSE)
#change depth values to absolute values
#raster.dat[[7]]<-abs(raster.dat[[7]])

#remove ndvi and gndvi layers ( do twice to remove the 8th and 9th layer), ight be easier way to do this
#raster.dat<-dropLayer(raster.dat, 8)
#raster.dat<-dropLayer(raster.dat, 8)

names(raster.dat) = c("blue","green","red","nir","swir1", "swir2","depth")
raster.dat<-raster.dat[[1:3]]

raster.name.dat= c("blue","green","red")
#only use blue green red (sets number of bands to use to 3)
raster.dim.use = 3 #number of bands to use the WCC
write.data = "Lyzenga1985.tif"
#shape.dat is called shape.in because I only have one shape file with all polygons
shape.in = shapefile("C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/sandWateColumnCorrection_Polygon.shp")
shape.dat = "C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/"


#Subset polygon data skipped for now because only have one file with all polygons
#files = list.files(shape.dat, pattern=".shp")
#shape.in = shapefile(paste(shape.dat,files[1],sep=""))
#shape.in@data$pol = 1
#for (i in 2:length(files)){
#  i=2
#  tmp = shapefile(paste(shape.dat,files[i],sep=""))
#  tmp@data$pol = i
#  shape.in =bind(shape.in,tmp)
#  rm(tmp)
#}
#rm(files,i)

shape.in = spTransform(shape.in, raster.dat@crs)
dat = extract(x=raster.dat,y=shape.in,df=T)
dat = na.omit(dat)
rm(shape.in)

#Generate ln #
dat.ln=dat
dat.ln[,2:(1+length(raster.name.dat))] = log (dat[,2:(1+length(raster.name.dat))])#take natural logarithm
#Calculate variance
sub.dat.variance = apply(dat.ln, 2, var) #calculate variance 
#Generate unique combinations
val.in = expand.grid(names(sub.dat.variance[2]),names(sub.dat.variance[3:(1+raster.dim.use)]))
i=2
{while(i<raster.dim.use){
    i = i+1
    test = expand.grid(names(sub.dat.variance[i]),names(sub.dat.variance[(i+1):(1+raster.dim.use)]))
    val.in = rbind(val.in,test)
  rm(test)}}#generate unqiue DDI
val.in = cbind(val.in,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1]))
               ,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])))
names(val.in) = c("B1","B2","Variance.B1","Variance.B2", "Covariance","a","slope","R2")
val.in[,1] = as.character(val.in[,1])
val.in[,2] = as.character(val.in[,2])
rm(i)
#Calculate covariance
for ( i in 1:length(val.in[,1])){
  val.in$Covariance[i] = cov(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]])
}
#Put variance into val.in
i=1
{while(i<raster.dim.use){
  i = i+1
  test = ifelse(names(sub.dat.variance[i])==val.in$B1,sub.dat.variance[i],NA)
  val.in$Variance.B1 = ifelse(is.na(test)==T,val.in$Variance.B1,test)
  }}
i=1
{while(i<=raster.dim.use){
  i = i+1
  test = ifelse(names(sub.dat.variance[i])==val.in$B2,sub.dat.variance[i],NA)
  val.in$Variance.B2 = ifelse(is.na(test)==T,val.in$Variance.B2,test)
  rm(test)
}}
#calculate a
for ( i in 1:length(val.in[,1])){
  val.in$a[i] = (val.in$Variance.B1[i]-val.in$Variance.B2[i])/(2*val.in$Covariance[i]) 
}
#calculate the slope
for ( i in 1:length(val.in[,1])){
  val.in$slope[i] = val.in$a[i] + sqrt((val.in$a[i]^2)+1)
}
#Plot each bi-plot
for ( i in 1:length(val.in[,1])){
  png(paste(shape.dat,"biplot-",val.in[i,1],val.in[i,2],".png",sep=""),pointsize=10,family="serif")
  plot(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]], ylab = paste ("ln ", val.in[i,2],sep=""),
       xlab = paste ("ln ", val.in[i,1],sep=""),pch=19, col=dat.ln[,1])
  test = lm(dat.ln[,val.in[i,2]]~ dat.ln[,val.in[i,1]]) 
  abline(a=  test$coefficients[1],b=test$coefficients[2])
  dev.off()
  val.in$R2[i] = summary(test)$r.squared
  rm(test)}
#Write out the coefficients
write.csv(val.in,paste(shape.dat,"WCC-Coef",".csv",sep="") )
##
rm(dat,dat.ln,sub.dat.variance,i)

#Apply the  water column correction to the visible bands
ras.dat = raster.dat
ras.dat = log(ras.dat)
names(ras.dat) = raster.name.dat
for (i in 1:length(val.in[,1])){
print(i)
    raster.dat[[i]] = ras.dat[[val.in[i,1]]]-(val.in$slope[i]*ras.dat[[val.in[i,2]]])
}
#Export data
writeRaster(raster.dat, paste(shape.dat,write.data,sep=""), format="GTiff", NAflag = NaN, overwrite=TRUE)
###

