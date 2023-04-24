library(raster)
library(dplyr)


files<-list.files(path="D:/AASGoutput/bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/winter",pattern=".tif",all.files =TRUE, full.names=FALSE)
#have to set working directory or else 
setwd("D:/AASGoutput/bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/winter")
#stacks all of the rasters you have created
allrasters <- stack(files)
#apply the name of all the files to the raster layers
names(allrasters)=(files)
#change all na values in the raster stack to 0
#this also now creates a column with NA values, so the data frame columns are now
#0, 1, 2, 3, 4
allrasters[is.na(allrasters[])] <- 0 
#transpose data from raster into matrix file
df.ras<-getValues(allrasters)
id.na = which(!is.na(df.ras[,1]))
# create an empty raster to fill later
depth.mask=allrasters[[1]]
#remove na file matrix
df.ras<-na.omit(df.ras)

table(df.ras[2,])
#create table out of each column, apply is a function that tells r to run the process
#on every layer of the raster stack in succession, the table function creates 
#a table of the number of pixels in each class for each raster in the stack
count.ras<-apply(df.ras, 2, table)

#this sums the values form the table above, so that we get a total number of 
# pixels in each raster which should all be the same, set to rows 2:4 
#because column 1 is just 0s which we don't want to include
sum.ras<-apply(count.ras[2:4,], 2, sum)
#this plots the data from row 2 (sand row) for each raster layer, divided by the
#total number of pixels in that image, to give us a percentage of pixels in that image
#that were sand
plot.data<-round((count.ras[2,]/sum.ras)*100,2)
plot(y=plot.data, x=(substr((colnames(count.ras)),2,5)), type="b", ylim=c(0,100))


plot.data2<-round((count.ras[3,]/sum.ras)*100,2)
points(y=plot.data2, x=(substr((colnames(count.ras)),2,5)), type="b", col="red")

plot.data3<-round((count.ras[4,]/sum.ras)*100,2)
points(y=plot.data3, x=(substr((colnames(count.ras)),2,5)), type="b", col="blue")

points(y=plot.data3+plot.data2, x=(substr((colnames(count.ras)),2,5)), type="b", col="green")

count.row<-apply(df.ras, 1, table, simplify = F)

emptydataframe<-matrix(NA, ncol=4, (nrow=dim(df.ras)[1]))

colnames(emptydataframe)<-c("x0", "x1", "x2", "x3")

for (i in 1:dim(df.ras)[1]){
  
a=unlist(count.row[[i]])
a=data.frame(a)
b=data.frame(a$Freq)
b=t(b)
colnames(b)<-paste0("x",a$Var1)
d=which(colnames(emptydataframe)%in%colnames(b))
emptydataframe[i,d]=b

}


emptydataframe[1,]
names(count.row[[1]])

#this does not work, need to change the NAs in columns to equal 0 some how??

emptydataframe <- emptydataframe %>% replace(is.na(.), 0)
df=cbind(emptydataframe[,3],emptydataframe[,4])
new.df2=cbind(emptydataframe[,2],(emptydataframe[,3]+emptydataframe[,4]))

new.df2=round((new.df2/18)*100,2)
df=round((df/18)*100,2)
out.dat.raster = raster(depth.mask)#define a raster
#seagrass to sand
out.dat.raster[id.na] =  new.df2[,2]#save output

#high density seagrass
out.dat.raster[id.na] = df[,2]#save output


# have to figure out masking across layers (it is an issue with some years have clouds masked)
# have to change na.omit above
#either change all rasters with same mask so they are equal

for(i in 1:length(count.row)){
  
  a=unlist(count.row)
  a=unlist(count.row[[i]])
  if(length(a)=1){
    
    new.df[]
  }
  new.df[i,]=a
  
  
  
}

