library(raster)
library(dplyr)
library(pals)

#creating change maps where the outputted map shows seagrass lost or gained in following year
#eg. 1999 image shows 2000-1999 which means that positive values = more seagrass in 2000 -ve values =less seagrass

files<-list.files(path="D:/AASGoutput/bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/winter",pattern=".tif",all.files =TRUE, full.names=FALSE)
#have to set working directory or else 
setwd("D:/AASGoutput/bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/winter")
#stacks all of the rasters you have created
allrasters <- stack(files)
#apply the name of all the files to the raster layers
names(allrasters)= paste("YM",(substr(files,1,4)),(substr(files,6,7)),
                         sep=".")

#before plotting,change the amount of space (margins) around plots
par(mar = c(0.5,0.5,0.5,0.5), mfrow = c(3,6))
# create a plot window by plotting the rasters at first
plot(allrasters)
#this plots with the legend you want
#to get R to plot the stack in the number of dimensions you want, only plot 12 images to begin with 
#and then move the plot window around to force it to plot a certain way
#then print final stacks that are missing


for(i in 1:length(files)){

plot(allrasters[[i]], #raster
     breaks= c(1.0,1.5,2.0,2.5,3.0), 
     legend=F, #don't draw the legend
     xlabel=F,
     main=(names(allrasters[[i]])),
     adj=0,
     line=-1,
     nc=6,
     nr=2,
     xaxt = "n",
     yaxt = "n",
     extent=(allrasters@extent),
     col= c("#FFFF99","#5DE694","#C9DECE","#006633")) 
  }

#your colours, found these at 
#https://www.rapidtables.com/web/color/RGB_Color.html, have to put 4 colours for it to plot
#different colours for each class
#to edit axis/ remove axis marks (xaxt = "n")

#next plot all of the maps for change between years
#you will get an error saying not a valid subset, this is only because when you get to the 18th file
#allrasters[[i+1]] will be 19 which does not exist, but will not ruin rest fo code
for(i in 1:(length(files))){
  
  print(paste("running file", i, "of", length(files), sep=" "))
  
  files<-list.files(path="D:/AASGoutput/bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/winter",pattern=".tif",all.files =TRUE, full.names=FALSE)
  #have to set working directory or else 
  setwd("D:/AASGoutput/bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/winter")
  #stacks all of the rasters you have created
  allrasters <- stack(files)
  
  change<-(allrasters[[(i+1)]]-allrasters[[i]])
  
  writeRaster(change, paste("D:/ChangeMaps","/",files[i],files[(i+1)],".tif", sep=""),format="GTiff",NAflag = NaN, overwrite=T)
  
}

#import the change maps
#read the list of files
changefiles<-list.files(path="D:/ChangeMaps")
#set the working directory to that folder
setwd("D:/ChangeMaps")
#create one raster stack from the list of files
allchange<-stack(changefiles)
#name all of the raster layers
names(allchange)<-paste("Y",substr(changefiles,1,4),substr(changefiles,38,41),sep=".")


#plot all of the files
plot(allchange)
par(mar = c(0.5,0.5,0.5,0.5), mfrow = c(3,6))

for(i in 1:length(changefiles)){
  
  plot(allchange[[i]], #raster
       breaks= c(-2.0,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2.0), 
       legend=F, #don't draw the legend
       xlabel=F,
       main=(names(allchange[[i]])),
       adj=0,
       line=-1,
       nc=6,
       nr=2,
       xaxt = "n",
       yaxt = "n",
       bg="grey",
       extent=(allrasters@extent),
       col=hcl.colors(8,"RdYlBu")) 
}


#also create matrix with information on number of pixels per class in each image
#to use for later
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

#to plot the amount of change between years, subtract one column from another in the change 
#files
change1<-as.data.frame(count.ras[,2]-count.ras[,1])
change2<-as.data.frame(count.ras[,3]-count.ras[,2])
change3<-as.data.frame(count.ras[,4]-count.ras[,3])
change4<-as.data.frame(count.ras[,5]-count.ras[,4])
change5<-as.data.frame(count.ras[,6]-count.ras[,5])
change6<-as.data.frame(count.ras[,7]-count.ras[,6])
change7<-as.data.frame(count.ras[,8]-count.ras[,7])
change8<-as.data.frame(count.ras[,9]-count.ras[,8])
change9<-as.data.frame(count.ras[,10]-count.ras[,9])
change10<-as.data.frame(count.ras[,11]-count.ras[,10])
change11<-as.data.frame(count.ras[,12]-count.ras[,11])
change12<-as.data.frame(count.ras[,13]-count.ras[,12])
change13<-as.data.frame(count.ras[,14]-count.ras[,13])
change14<-as.data.frame(count.ras[,15]-count.ras[,14])
change15<-as.data.frame(count.ras[,16]-count.ras[,15])
change16<-as.data.frame(count.ras[,17]-count.ras[,16])
change17<-as.data.frame(count.ras[,18]-count.ras[,17])
p<-cbind(change1,change2,change3, change4, change5, change6, change7, change8, change9,
         change10, change11, change12, change13, change14, change15, change16, change17)
colnames(p)<-c("1999-2000", "2000-2001", "2001-2002","2002-2003","2003-2004",
                "2004-2006","2006-2007","2007-2009", "2009-2011", "2011-2014",
               "2014-2015","2015-2016","2016-2017","2017-2018", "2018-2020", 
                "2020-2021", "2021-2022")
#multiply entire data frame by .09 to convert to hectares
p<-p*.09
#this calculates percent of total pixels that changed in each year
# the value 2978.01 is the number of pixels in each map multiplied by .09 to convert to
#hectares
par(mar=c(2,2,2,2),mfrow=c(1,1))
plot.data<-round((p[2,]/2978.01)*100,2)
plot(y=plot.data, x=1:18, type="b",ylim=c(-10,10))

#plot(y=plot.data, x=paste(substr(changefiles,1,4),"-",substr(changefiles,38,41),sep=""), type="b", col="blue")
#, type="b")


plot.data2<-round((p[3,]/2978.01)*100,2)
points(y=plot.data2, x=colnames(p), type="b", col="green",ylim=c(-10, 10))

plot.data3<-round((p[4,]/sum.ras)*100,2)
points(y=plot.data3, x=colnames(p), type="b", col="purple",ylim=c(-10, 10))

points(y=plot.data3+plot.data2, x=colnames(p), type="b", col="blue",ylim=c(-10, 10), abline(h=0))


# now to look at specific areas fo the map
#import all of the csv files for calculating number of pixels of each class per year
#add all of the csv files into one matrix
filescsv<-list.files(path="D:/AASGoutput/csvfiles-final-bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesites/winter", 
                     pattern = ".csv", all.files = T, full.names =T)%>%map_df(~read.csv(.))
#write out the csv files so you can add year
write.csv(filescsv,"D:/AASGoutput/csvfiles-final-bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesite/winter/allyearswinter.csv")

# I then went in and manually changed it so that each column is 1 year and each 
#nursery area or AOI has its own page

#plotting change in each nursery----
NS<-read.csv("D:/AASGoutput/csvfiles-final-bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesites/NSwinter.csv")
SL<-read.csv("D:/AASGoutput/csvfiles-final-bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesites/SLwinter.csv")
SB<-read.csv("D:/AASGoutput/csvfiles-final-bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesites/SBwinter.csv")

NS<-NS[1:4,2:19]
SL<-SL[1:4,2:19]
SB<-SB[1:4,2:19]

#calculate total area in pixels of each area of map to use to calculate percent 
#change later
sumNS<-sum(NS[1:3,2])
sumSL<-sum(SL[1:3,2])
sumSB<-sum(SB[1:3,2])

#plot area of habitats from NS
plot.data = round((NS[1,]/sumNS)*100,2)
plot(y=plot.data, x=substr((colnames(NS)),2,5), type="b", col="red", main="NS",
     ylab="hectares",xlab="Year",ylim=c(20,100))

plot.data2 = round((NS[2,]/sumNS)*100,2)
points(y=plot.data2, x=substr((colnames(NS)),2,5), type="b", col="green",main="NS",
     ylab="hectares",xlab="Year",ylim=c(20,100))

plot.data3 = round((NS[3,]/sumNS)*100,2)
points(y=plot.data3, x=substr((colnames(NS)),2,5), type="b", col="darkgreen",main="NS",
     ylab="hectares",xlab="Year",ylim=c(20,100))

points(y=plot.data3+plot.data2, x=substr((colnames(NS)),2,5), type="b", col="blue",main="NS",
     ylab="hectares",xlab="Year",ylim=c(20,100))

legend(1999, 105, legend=c("Sand", "LD_SG","HD_SG","Sum_SG"),
       col=c("red", "green","darkgreen","blue"),lty=1, cex=0.8)

#repeat the code above for the other habitat areas
plot.data = round((SL[1,]/sumSL)*100,2)
plot(y=plot.data, x=substr((colnames(SL)),2,5), type="b", col="red", main="SL",
     ylab="hectares",xlab="Year",ylim=c(15,100))

plot.data2 = round((SL[2,]/sumSL)*100,2)
points(y=plot.data2, x=substr((colnames(SL)),2,5), type="b", col="green",main="SL",
       ylab="hectares",xlab="Year",ylim=c(15,100))

plot.data3 = round((SL[3,]/sumSL)*100,2)
points(y=plot.data3, x=substr((colnames(SL)),2,5), type="b", col="darkgreen",main="SL",
       ylab="hectares",xlab="Year",ylim=c(15,100))

points(y=plot.data3+plot.data2, x=substr((colnames(SL)),2,5), type="b", col="blue",main="SL",
       ylab="hectares",xlab="Year",ylim=c(15,100))

legend(1999, 105, legend=c("Sand", "LD_SG","HD_SG","Sum_SG"),
       col=c("red", "green","darkgreen","blue"),lty=1, cex=0.8)


plot.data = round((SB[1,]/sumSB)*100,2)
plot(y=plot.data, x=substr((colnames(SB)),2,5), type="b", col="red", main="SB",
     ylab="hectares",xlab="Year",ylim=c(10,110))

plot.data2 = round((SB[2,]/sumSB)*100,2)
points(y=plot.data2, x=substr((colnames(SB)),2,5), type="b", col="green",main="SB",
       ylab="hectares",xlab="Year",ylim=c(10,110))

plot.data3 = round((SB[3,]/sumSB)*100,2)
points(y=plot.data3, x=substr((colnames(SB)),2,5), type="b", col="darkgreen",main="SB",
       ylab="hectares",xlab="Year",ylim=c(10,100))

points(y=plot.data3+plot.data2, x=substr((colnames(SB)),2,5), type="b", col="blue",main="SB",
       ylab="hectares",xlab="Year",ylim=c(10,110))

legend(1999, 115, legend=c("Sand", "LD_SG","HD_SG","Sum_SG"),
       col=c("red", "green","darkgreen","blue"),lty=1, cex=0.8)


#another option I tried, which imports all csv files manually, but coulcn't figure out how to bind them all
#together
#for(i in names24){
 # filescsv2<-file.path("D:/AASGoutput/csvfiles-final-bgr-lyzgr-lyzgrbl-depth_sand-ldsg-greenstablesites/winter",
                       #paste(i,".csv",sep=""))
  
  #assign(i, read.csv(filescsv2,
                     #colClasses=c("character","factor",rep("numeric",4)),
                     #sep = ","))
#}
#now to calculate the change
#first for north sound
NS1<-as.data.frame(NS[,2]-NS[,1])
NS2<-as.data.frame(NS[,3]-NS[,2])
NS3<-as.data.frame(NS[,4]-NS[,3])
NS4<-as.data.frame(NS[,5]-NS[,4])
NS5<-as.data.frame(NS[,6]-NS[,5])
NS6<-as.data.frame(NS[,7]-NS[,6])
NS7<-as.data.frame(NS[,8]-NS[,7])
NS8<-as.data.frame(NS[,9]-NS[,8])
NS9<-as.data.frame(NS[,10]-NS[,9])
NS10<-as.data.frame(NS[,11]-NS[,10])
NS11<-as.data.frame(NS[,12]-NS[,11])
NS12<-as.data.frame(NS[,13]-NS[,12])
NS13<-as.data.frame(NS[,14]-NS[,13])
NS14<-as.data.frame(NS[,15]-NS[,14])
NS15<-as.data.frame(NS[,16]-NS[,15])
NS16<-as.data.frame(NS[,17]-NS[,16])
NS17<-as.data.frame(NS[,18]-NS[,17])

pNS<-cbind(NS1,NS2,NS3, NS4, NS5, NS6, NS7, NS8, NS9,
         NS10, NS11, NS12, NS13, NS14, NS15, NS16, NS17)
colnames(pNS)=c("1999-2000", "2000-2001", "2001-2002","2002-2003","2003-2004",
              "2004-2006","2006-2007","2007-2009", "2009-2011", "2011-2014",
              "2014-2015","2015-2016","2016-2017","2017-2018", "2018-2020", 
              "2020-2021", "2021-2022")

SL1<-as.data.frame(SL[,2]-SL[,1])
SL2<-as.data.frame(SL[,3]-SL[,2])
SL3<-as.data.frame(SL[,4]-SL[,3])
SL4<-as.data.frame(SL[,5]-SL[,4])
SL5<-as.data.frame(SL[,6]-SL[,5])
SL6<-as.data.frame(SL[,7]-SL[,6])
SL7<-as.data.frame(SL[,8]-SL[,7])
SL8<-as.data.frame(SL[,9]-SL[,8])
SL9<-as.data.frame(SL[,10]-SL[,9])
SL10<-as.data.frame(SL[,11]-SL[,10])
SL11<-as.data.frame(SL[,12]-SL[,11])
SL12<-as.data.frame(SL[,13]-SL[,12])
SL13<-as.data.frame(SL[,14]-SL[,13])
SL14<-as.data.frame(SL[,15]-SL[,14])
SL15<-as.data.frame(SL[,16]-SL[,15])
SL16<-as.data.frame(SL[,17]-SL[,16])
SL17<-as.data.frame(SL[,18]-SL[,17])
pSL<-cbind(SL1,SL2,SL3, SL4, SL5, SL6, SL7, SL8, SL9,
           SL10, SL11, SL12, SL13, SL14, SL15, SL16, SL17)
colnames(pSL)=c("1999-2000", "2000-2001", "2001-2002","2002-2003","2003-2004",
              "2004-2006","2006-2007","2007-2009", "2009-2011", "2011-2014",
              "2014-2015","2015-2016","2016-2017","2017-2018", "2018-2020", 
              "2020-2021", "2021-2022")

SB1<-as.data.frame(SB[,2]-SB[,1])
SB2<-as.data.frame(SB[,3]-SB[,2])
SB3<-as.data.frame(SB[,4]-SB[,3])
SB4<-as.data.frame(SB[,5]-SB[,4])
SB5<-as.data.frame(SB[,6]-SB[,5])
SB6<-as.data.frame(SB[,7]-SB[,6])
SB7<-as.data.frame(SB[,8]-SB[,7])
SB8<-as.data.frame(SB[,9]-SB[,8])
SB9<-as.data.frame(SB[,10]-SB[,9])
SB10<-as.data.frame(SB[,11]-SB[,10])
SB11<-as.data.frame(SB[,12]-SB[,11])
SB12<-as.data.frame(SB[,13]-SB[,12])
SB13<-as.data.frame(SB[,14]-SB[,13])
SB14<-as.data.frame(SB[,15]-SB[,14])
SB15<-as.data.frame(SB[,16]-SB[,15])
SB16<-as.data.frame(SB[,17]-SB[,16])
SB17<-as.data.frame(SB[,18]-SB[,17])
pSB<-cbind(SB1,SB2,SB3, SB4, SB5, SB6, SB7, SB8, SB9,
           SB10, SB11, SB12, SB13, SB14, SB15, SB16, SB17)
colnames(pSB)=c("1999-2000", "2000-2001", "2001-2002","2002-2003","2003-2004",
              "2004-2006","2006-2007","2007-2009", "2009-2011", "2011-2014",
              "2014-2015","2015-2016","2016-2017","2017-2018", "2018-2020", 
              "2020-2021", "2021-2022")

rm(list=setdiff(ls(),c("pNS","pSB","pSL","sumNS","sumSB","sumSL")))

#plot the data!!
#this calculates percent of total pixels that changed in each year
# the value 2978.01 is the number of pixels in each map multiplied by .09 to convert to
#hectares

#plot(y=plot.data, x=paste(substr(changefiles,1,4),"-",substr(changefiles,38,41),sep=""), type="b", col="blue")
#, type="b")

#NorthSound
plot.data<-round((pNS[2:3,]/sumNS)*100,2)
colnames(plot.data)<-substr((colnames(pNS)),6,10)
plot.data<-as.matrix(plot.data)
barplot(plot.data, main="Percent Change in Habitat Coverage in North Sound",
        xlab="Year",ylab="Percent Change from Previous Year", col=c("green","darkgreen"), ylim=c(-30,30),
        legend = c("MD_Seagrass","HD_Seagrass"), beside=T)

legend(1999, 10, legend=c("Barren","Vegetated"),
       col=c("red", "green"),lty=1, cex=0.8)
#together
plot.data<-round(((pNS[2,]+pNS[3,])/sumNS)*100,2)
colnames(plot.data)<-substr((colnames(pNS)),6,10)
plot.data<-as.matrix(plot.data)
barplot(plot.data, main="Percent Change in Habitat Coverage in North Sound",
        xlab="Year",ylab="Percent Change from Previous Year", col="darkgreen", ylim=c(-30,30),
        beside=T)


#SouthBimini
plot.data<-round((pSB[2:3,]/sumSB)*100,2)
colnames(plot.data)<-substr((colnames(pSB)),6,10)
plot.data<-as.matrix(plot.data)
barplot(plot.data, main="Percent Change in Habitat Coverage in South Bimini",
        xlab="Year",ylab="Percent Change from Previous Year", col=c("green","darkgreen"), ylim=c(-30,30),
        legend = c("MD_Seagrass","HD_Seagrass"), beside=T)

#together
plot.data<-round(((pSB[2,]+pSB[3,])/sumSB)*100,2)
colnames(plot.data)<-substr((colnames(pSB)),6,10)
plot.data<-as.matrix(plot.data)
barplot(plot.data, main="Percent Change in Habitat Coverage in South Bimini",
        xlab="Year",ylab="Percent Change from Previous Year", col="darkgreen", ylim=c(-30,30),
         beside=T)


#SharkLand

plot.data<-round((pSL[2:3,]/sumSL)*100,2)
colnames(plot.data)<-substr((colnames(pEB)),6,10)
plot.data<-as.matrix(plot.data)
barplot(plot.data, main="Percent Change in Habitat Coverage in SharkLand",
        xlab="Year",ylab="Percent Change from Previous Year", col=c("green","darkgreen"), ylim=c(-30,30),
        legend = c("MD_Seagrass","HD_Seagrass"), beside=T)

#together
plot.data<-round(((pSL[2,]+pSL[3,])/sumSL)*100,2)
colnames(plot.data)<-substr((colnames(pSL)),6,10)
plot.data<-as.matrix(plot.data)
barplot(plot.data, main="Percent Change in Habitat Coverage in Shark Land",
        xlab="Year",ylab="Percent Change from Previous Year", col="darkgreen", ylim=c(-30,30),
        beside=T)
