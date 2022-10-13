library("raster")

setwd("C://Users//wilsonkri//Documents//Backup-WV//")
wv.dat = stack("Level2A//L2A-WV2-LBH-20200519.tif",
             "./Depth/LBH/DepthNONNA6mmask.tif")
names(wv.dat)= c("cb","b","g","y","r","re","n1","n2","depth")
field.dat = read.csv("GroundTruthing//SouthShore_dropcam_summary.csv")
field.dat = field.dat[grepl("BackHbr",field.dat$site)==T,]


#Look at the spectra
rrs.list = list(NA)
rrs.list.buf = list(NA)
for (i in 1:length(field.dat$site)){
track = strsplit(field.dat$WKT[i], ",")
track = strsplit(track[[1]], ")")
track[[1]] = strsplit(track[[1]], " ")[[1]][2:3]
track[[1]] = paste(strsplit(track[[1]], "[(]")[[1]][2],strsplit(track[[1]], "[(]")[[2]][1])
track = unlist(track)
track = unlist(strsplit(track, " "))
track.out = cbind(track[seq(1,length(track)*2,2)],
                  track[seq(2,length(track)*2,2)])
track.out = na.omit(track.out)
track.out = as.data.frame(track.out)
names(track.out)=c("Easting", "Northing")
track.out$Habitat = rep(field.dat$habitat_label[i],length(track.out[,1]))
track.out[,1] = as.numeric(track.out[,1])
track.out[,2] = as.numeric(track.out[,2])
if (i==20){
  track.out = track.out[-c(0:60,90:170),]
}
track.out = SpatialPointsDataFrame(coords = track.out[,1:2],track.out,
                                   proj4string = CRS("+proj=utm +zone=20 +datum=NAD83 +units=m +no_defs"))

rrs = extract(wv.dat, track.out,df=T, cellnumbers=T)
rrs = cbind(rrs, track.out@data)
rrs = rrs[duplicated(rrs$cells)==F,]
rrs = SpatialPointsDataFrame(coords = rrs[,c("Easting","Northing")],rrs,
                                   proj4string = CRS("+proj=utm +zone=20 +datum=NAD83 +units=m +no_defs"))
rrs.buf = buffer(rrs,width=3, dissolve=T)
rrs.buf.df = extract(wv.dat, rrs.buf,df=T, cellnumbers=T)
rrs.list[[i]] = rrs@data
rrs.list[[i]][,15:46] = field.dat[i,c(2:6,9:35)]
rrs.list.buf[[i]] = rrs.buf.df
rrs.list.buf[[i]][,12:44] = field.dat[i,c(2:6,9:36)]
rm(rrs,rrs.buf,rrs.buf.df,track.out,track)
}
rm(i)




#backup.rrs = rrs.list
sz = rep(NA, length(rrs.list))
sz2 = rep(NA, length(rrs.list))
for(i in 1:length(rrs.list)){
  sz[i] = dim(rrs.list[[i]])[1]
  sz2[i] = dim(rrs.list[[i]])[2]}
rrs.out = as.data.frame(matrix(NA, nrow=sum(sz), ncol=sz2[1]))
rrs.out[1:sz[1],]= rrs.list[[1]]
for (i in 2:length(rrs.list)){
  a = which(is.na(rrs.out$V14)==T)[1]
  a = a:(a+sz[i]-1)
  rrs.out[a,]=rrs.list[[i]]}
names(rrs.out) = names(rrs.list[[1]])
rrs.out$rg = rrs.out$r / rrs.out$g
rrs.out$ndvi = (rrs.out$re - rrs.out$r)/(rrs.out$re + rrs.out$r)

#added
pca6 = brick("./LunenburgBackHarbour/Rasters/PCA-nostand-BOA-6b-6mm.tif")
names(pca6) = c("pca6.1","pca6.2","pca6.3","pca6.4","pca6.5","pca6.6")
test = extract(pca6, rrs.out$cells,df=T)
rrs.out = cbind(rrs.out, test[,2:7])
pca4 = brick("./LunenburgBackHarbour/Rasters/PCA-nostand-BOA-4b-6mm.tif")
names(pca4) = c("pca4.1","pca4.2","pca4.3","pca4.4")
test = extract(pca4, rrs.out$cells,df=T)
rrs.out = cbind(rrs.out, test[,2:5])
wcc = brick("./LunenburgBackHarbour/Rasters/WCC-6mm.tif")
names(wcc) = c("diicbb","diicbg","diicby","diicbr", "diibg", "diiby","diibr", "diigy","diigr","diiyr")
test = extract(wcc, rrs.out$cells,df=T)
test = test[,-1]
rrs.out = cbind(rrs.out, test)
rm(test)
##
bare = unique(rrs.out$site)[c(1,24,25,27,29,2,3,5,7,10,11,13,20)]
mixed = unique(rrs.out$site)[c(16,19,22,23)]
other = unique(rrs.out$site)[c(12,21,28,14)]
eelgrass = unique(rrs.out$site)[c(26,4,6,8,9,15,17,18)]
rrs.out$inptlbl = NA
for (i in 1:length(rrs.out$inptlbl)){
  rrs.out$inptlbl[i] = ifelse(rrs.out$site[i]%in%bare==T,0,
                              ifelse(rrs.out$site[i]%in%eelgrass==T,1,
                                     ifelse(rrs.out$site[i]%in%mixed==T,2,
                                            ifelse(rrs.out$site[i]%in%other==T,3,rrs.out$inptlbl[i] ))))}

rrs.out = rrs.out[is.na(rrs.out$cb)==F,]
write.csv(rrs.out,"LunenburgBackHarbour//FieldDataTransect.csv", row.names = F)

#Buffer
sz = rep(NA, length(rrs.list.buf))
sz2 = rep(NA, length(rrs.list.buf))
for(i in 1:length(rrs.list.buf)){
  sz[i] = dim(rrs.list.buf[[i]])[1]
  sz2[i] = dim(rrs.list.buf[[i]])[2]}
rrs.out = as.data.frame(matrix(NA, nrow=sum(sz), ncol=sz2[1]))
rrs.out[1:sz[1],]= rrs.list.buf[[1]]
for (i in 2:length(rrs.list.buf)){
  a = which(is.na(rrs.out$V14)==T)[1]
  a = a:(a+sz[i]-1)
  rrs.out[a,]=rrs.list.buf[[i]]}
names(rrs.out) = names(rrs.list.buf[[1]])
rrs.out$rg = rrs.out$r / rrs.out$g
rrs.out$ndvi = (rrs.out$re - rrs.out$r)/(rrs.out$re + rrs.out$r)

#added
pca6 = brick("./LunenburgBackHarbour/Rasters/PCA-nostand-BOA-6b-6mm.tif")
names(pca6) = c("pca6.1","pca6.2","pca6.3","pca6.4","pca6.5","pca6.6")
test = extract(pca6, rrs.out$cell,df=T)
rrs.out = cbind(rrs.out, test[,2:7])
pca4 = brick("./LunenburgBackHarbour/Rasters/PCA-nostand-BOA-4b-6mm.tif")
names(pca4) = c("pca4.1","pca4.2","pca4.3","pca4.4")
test = extract(pca4, rrs.out$cell,df=T)
rrs.out = cbind(rrs.out, test[,2:5])
wcc = brick("./LunenburgBackHarbour/Rasters/WCC-6mm.tif")
names(wcc) = c("diicbb","diicbg","diicby","diicbr", "diibg", "diiby","diibr", "diigy","diigr","diiyr")
test = extract(wcc, rrs.out$cell,df=T)
test = test[,-1]
rrs.out = cbind(rrs.out, test)
rm(test)
##

bare = unique(rrs.out$site)[c(1,24,25,27,29,2,3,5,7,10,11,13,20)]
mixed = unique(rrs.out$site)[c(16,19,22,23)]
other = unique(rrs.out$site)[c(12,21,28,14)]
eelgrass = unique(rrs.out$site)[c(26,4,6,8,9,15,17,18)]
rrs.out$inptlbl = NA
for (i in 1:length(rrs.out$inptlbl)){
  rrs.out$inptlbl[i] = ifelse(rrs.out$site[i]%in%bare==T,0,
                                    ifelse(rrs.out$site[i]%in%eelgrass==T,1,
                                           ifelse(rrs.out$site[i]%in%mixed==T,2,
                                                  ifelse(rrs.out$site[i]%in%other==T,3,rrs.out$inptlbl[i] ))))}

bare = unique(rrs.out$site)[c(1,24,25,27,29,2,3,5,7,10,11,13,20)]
mixed = unique(rrs.out$site)[c(16,19,22,23)]
other = unique(rrs.out$site)[c(12,21,28,14)]
eelgrass = unique(rrs.out$site)[c(26,4,6,8,9,15,17,18)]
rrs.out$inptlbl = NA
for (i in 1:length(rrs.out$inptlbl)){
  rrs.out$inptlbl[i] = ifelse(rrs.out$site[i]%in%bare==T,0,
                              ifelse(rrs.out$site[i]%in%eelgrass==T,1,
                                     ifelse(rrs.out$site[i]%in%mixed==T,2,
                                            ifelse(rrs.out$site[i]%in%other==T,3,rrs.out$inptlbl[i] ))))}

rrs.out = rrs.out[is.na(rrs.out$cb)==F,]
write.csv(rrs.out,"LunenburgBackHarbour//FieldDataTransect3mBuffer.csv", row.names = F)
