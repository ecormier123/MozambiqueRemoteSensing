library(raster)
library(RStoolbox)

setwd("C://Users//wilsonkri//Documents//Backup-WV//20190817/")

#wv.dat = brick("./Raster/BOA-Level2-10mmask-reflectance-20190817.tif")
wv.dat = brick("./Raster/BOA-Level2-reflectance-20190817.tif")
names(wv.dat) = c("cb","b","g","y","r","re","n1","n2")
#wv.dat.ds = brick("./Raster/Destripe-Level2-10mmask-reflectance-20190817.tif")
wv.dat.ds = brick("./StripeCorrection/PD3-use/BOA-destripe.tif")
names(wv.dat.ds) = c("cb","b","g","y","r","re","n1","n2")

greenlaw = raster("./ExtraFiles/Depth.tif")

#Generate raster pca on 10m depth for the six bands
#pca.dat = rasterPCA(wv.dat[[1:6]], spca=F)
#writeRaster(pca.dat$map, "./Raster/PCA-nostand-BOA-Level2-10mmask-reflectance-20190817.tif", format="GTiff", NAflag = NaN)
#writeRaster(pca.dat$map, "./Raster/PCA-nostand-BOA-Level2-reflectance-20190817.tif", format="GTiff", NAflag = NaN)
pca.boa = brick("./Raster/PCA-nostand-BOA-Level2-reflectance-20190817.tif")
names(pca.boa) = paste0("PCA", 1:6)

#Generate raster pca on 10m depth for the six bands destripes
#pca.dat = rasterPCA(wv.dat.ds[[1:6]], spca=F)
#writeRaster(pca.dat$map, "./Raster/PCA-nostand-Destripe-Level2-10mmask-reflectance-20190817.tif", format="GTiff", NAflag = NaN)
#writeRaster(pca.dat$map, "./Raster/PCA-nostand-Destripe-Level2-reflectance-20190817.tif", format="GTiff", NAflag = NaN)
pca.ds = brick("./Raster/PCA-nostand-Destripe-Level2-reflectance-20190817.tif")
names(pca.ds) = paste0("PCA", 1:6)

#WCC
wcc.boa = brick("./WCC_BOA/Lyzenga1985-boa-alldpth.tif")
names(wcc.boa) = c(paste0("dii-cb", c("b","g","y","r")),paste0("dii-b", c("g","y","r")),paste0("dii-g", c("y","r")), "dii-yr")
wcc.ds = brick("./WCC_Destripe/Lyzenga1985-ds-alldpth.tif")
names(wcc.ds) = c(paste0("dii-cb", c("b","g","y","r")),paste0("dii-b", c("g","y","r")),paste0("dii-g", c("y","r")), "dii-yr")

#SDB
sdb.boa = raster("./Raster/BOA-Level2-10mmask-reflectance-20190817_bgSDB.tif")
sdb.boa.v2 = raster("./Raster/BOA-Level2-10mmask-reflectance-20190817_bgSDBv2.tif")
sdb.ds= raster("./Raster/Destripe-Level2-10mmask-reflectance-20190817_bgSDB.tif")
sdb.ds.v2= raster("./Raster/Destripe-Level2-10mmask-reflectance-20190817_bgSDBv2.tif")

#Point based data
hab.dat = read.csv("./TrainingSites//20190817.csv")
hab.dat  = SpatialPointsDataFrame(coords = hab.dat [,c(5,4)], data = hab.dat , 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))#
hab.dat  = spTransform(hab.dat , crs(wv.dat@crs))
ras.layers.boa = stack(wv.dat,greenlaw,pca.boa,wcc.boa,sdb.boa,sdb.boa.v2)
ras.layers.ds = stack(wv.dat.ds,greenlaw,pca.ds,wcc.ds,sdb.ds,sdb.ds.v2)
hab.dat.boa = extract(y=hab.dat, x=ras.layers.boa, sp=T, cellnumbers=T)
names(hab.dat.boa)[47] = "SDB"
names(hab.dat.boa)[48] = "SDBv2"
shapefile(hab.dat.boa,"./TrainingSites///shapefile//hab_dat_boa.shp",overwrite=T)
hab.dat.ds = extract(y=hab.dat, x=ras.layers.ds, sp=T, cellnumbers=T)
names(hab.dat.ds)[47] = "SDB"
names(hab.dat.ds)[48] = "SDBv2"
shapefile(hab.dat.ds, "./TrainingSites///shapefile//hab_dat_ds.shp",overwrite=T)
rm(hab.dat.boa,hab.dat.ds)

#Poly based data
files = list.files("./TrainingSites//Polys//FinalUse", pattern=".shp")
shape.in = shapefile(paste("./TrainingSites//Polys//FinalUse//",files[1],sep=""))
shape.in@data$pol = files[1]
hab.dat.boa = extract(y=shape.in, x=ras.layers.boa, df=T, cellnumbers=T)
hab.dat.boa$ID = files[1]
for (i in 2:length(files)){
  print(i)
  tmp = shapefile(paste("./TrainingSites//Polys//FinalUse//",files[i],sep=""))
  tmp = extract(y= tmp , x=ras.layers.boa, df=T, cellnumbers=T)
  tmp$ID = files[i]
  hab.dat.boa  = rbind(hab.dat.boa,tmp)
  rm(tmp)
}
rm(files,i)
names(hab.dat.boa)[28] = "SDB"
names(hab.dat.boa)[29] = "SDBv2"

##Reduce each site to 250
in.dat = hab.dat.boa[hab.dat.boa$ID==unique(hab.dat.boa$ID)[1],]
if(length(in.dat[,1])>250){
  set.seed(30)
  out.dat = in.dat[sample(nrow(in.dat), 250), ]}
if(length(in.dat[,1])<=250){out.dat = in.dat}
rdc.ds = out.dat
rm(in.dat)
for (i in 2:length(unique(hab.dat.boa$ID))){
  in.dat = hab.dat.boa[hab.dat.boa$ID==unique(hab.dat.boa$ID)[i],]
  if(length(in.dat[,1])>250){
    set.seed(30)
    out.dat = in.dat[sample(nrow(in.dat), 250), ]}
  if(length(in.dat[,1])<=250){out.dat = in.dat}
  rdc.ds = rbind(rdc.ds,out.dat)
  rm(in.dat,out.dat)
}
##
write.csv(rdc.ds,"./TrainingSites///shapefile//AllPolyBOA.csv", row.names = F)
rm(hab.dat.boa,rdc.ds)

#Poly based data
files = list.files("./TrainingSites//Polys//FinalUse", pattern=".shp")
shape.in = shapefile(paste("./TrainingSites//Polys//FinalUse//",files[1],sep=""))
shape.in@data$pol = files[1]
hab.dat.boa = extract(y=shape.in, x=ras.layers.ds, df=T, cellnumbers=T)
hab.dat.boa$ID = files[1]
for (i in 2:length(files)){
  print(i)
  tmp = shapefile(paste("./TrainingSites//Polys//FinalUse//",files[i],sep=""))
  tmp = extract(y= tmp , x=ras.layers.ds, df=T, cellnumbers=T)
  tmp$ID = files[i]
  hab.dat.boa = rbind(hab.dat.boa,tmp)
  rm(tmp)
}
rm(files,i)
names(hab.dat.boa)[28] = "SDB"
names(hab.dat.boa)[29] = "SDBv2"
##Reduce each site to 250
in.dat = hab.dat.boa[hab.dat.boa$ID==unique(hab.dat.boa$ID)[1],]
if(length(in.dat[,1])>250){
  set.seed(30)
  out.dat = in.dat[sample(nrow(in.dat), 250), ]}
if(length(in.dat[,1])<=250){out.dat = in.dat}
rdc.ds = out.dat
rm(in.dat)
for (i in 2:length(unique(hab.dat.boa$ID))){
  in.dat = hab.dat.boa[hab.dat.boa$ID==unique(hab.dat.boa$ID)[i],]
  if(length(in.dat[,1])>250){
    set.seed(30)
    out.dat = in.dat[sample(nrow(in.dat), 250), ]}
  if(length(in.dat[,1])<=250){out.dat = in.dat}
  rdc.ds = rbind(rdc.ds,out.dat)
  rm(in.dat,out.dat)
}
rm(i)
##

write.csv(rdc.ds,"./TrainingSites//shapefile//AllPolyDS.csv", row.names = F)
rm(hab.dat.boa,hab.dat,rdc.ds)
rm(greenlaw,pca.boa,wcc.boa,pca.ds,wcc.ds,sdb.ds,sdb.ds.v2)
rm(sdb.boa, sdb.boa.v2,ras.layers.boa,ras.layers.ds)
rm(shape.in)
##

#####
##Add in correct labels
hab.dat.boa = shapefile("./TrainingSites//shapefile//hab_dat_boa.shp")
hab.dat.ds = shapefile("./TrainingSites//shapefile//hab_dat_ds.shp")
all.dat.boa = read.csv("./TrainingSites//shapefile//AllPolyBOA.csv")
all.dat.ds = read.csv("./TrainingSites//shapefile//AllPolyDS.csv")

test = unlist(strsplit(all.dat.boa$ID, "_Polygon.shp"))
test = unlist(strsplit(test, "_Polygon_MultiPolygon.shp"))
test = unlist(strsplit(test, "MultiPolygon.shp"))
test = unlist(strsplit(test, "_Polygon"))
#hab.type = ifelse((test=="66"|test=="67A"|test=="68"|test=="73"|test=="74_"|test=="77-77b-veg"|test=="81veg"|test=="NASG-P"|test=="sand_75"|test=="TaylorHeadEcosurvey"|test=="VPseagrass1"),
  #                "FSP","VIS")
hab.type = ifelse(test=="66", hab.dat.boa$InptLbl[hab.dat.boa$station==66][1],
                  ifelse(test=="67A", hab.dat.boa$InptLbl[hab.dat.boa$station==67][1],
                  ifelse(test=="68", hab.dat.boa$InptLbl[hab.dat.boa$station==68][1],
                  ifelse(test=="73", hab.dat.boa$InptLbl[hab.dat.boa$station==73][1],
                  ifelse(test=="77-77b-veg", hab.dat.boa$InptLbl[hab.dat.boa$station==77][1],
                  ifelse(test=="81veg", hab.dat.boa$InptLbl[hab.dat.boa$station==81][1],
                  ifelse(test=="NASG-P", hab.dat.boa$InptLbl[hab.dat.boa$station=="visual check"][1],
                  ifelse(test=="74_", hab.dat.boa$InptLbl[hab.dat.boa$station==74][1],
                  ifelse(test=="sand_75", 0,
                  ifelse(test=="TaylorHeadEcosurvey", 2,
                  ifelse(test=="VPseagrass1", 2,
                  ifelse(test== "veg1"|test=="veg2"|test=="veg3"|test=="veg4"|test=="veg5",4,
                  ifelse(test== "NewVeg1"|test=="NewVeg2"|test=="NewVeg3"|test=="NewVeg4"|test=="NewVeg6",4,
                  ifelse(test== "NewODW1_"|test=="NewODW2_"|test=="NewODW3_"|test=="NewODW4_"|test=="NewODW5_",-1,0)))))))))))))) 

###
coord.ras = xyFromCell(wv.dat[[1]],all.dat.boa$cell)
out.dat = cbind(coord.ras, test,hab.type, all.dat.boa)
names(out.dat)[1:2] = c("x.cord","y.cord")
names(out.dat)[3]="station"
out.dat = SpatialPointsDataFrame(coords =out.dat[,c(1,2)], data = out.dat , 
                                  proj4string = wv.dat@crs)
shapefile(out.dat, "./TrainingSites///shapefile//AllPolyBOA.shp",overwrite=T)

out.dat = cbind(coord.ras, test,hab.type, all.dat.ds)
names(out.dat)[1:2] = c("x.cord","y.cord")
names(out.dat)[3]="station"
out.dat = SpatialPointsDataFrame(coords =out.dat[,c(1,2)], data = out.dat , 
                                 proj4string = wv.dat@crs)
shapefile(out.dat, "./TrainingSites///shapefile//AllPolyDS.shp",overwrite=T)
