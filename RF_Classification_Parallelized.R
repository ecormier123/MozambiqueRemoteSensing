rm(list=ls())

library("raster")
library("caret")
library("irr")
library("doParallel")  #Foreach Parallel Adaptor 
library("foreach")     #Provides foreach looping construct
rasterOptions(maxmemory=7e+09,chunksize = 4e+08)

setwd("E:\\Data\\Sentinel")
#Read in raster dataset
wv.dat = stack("20160913Level2A.tif")
names(wv.dat) = c("b","g","r","n", "depth","ndvi","gndvi")
dep.val = 10 
ndvi.thres = 0.4 
b.thres = 0.035 
gr = 0.9
gr1 = 0.3
#Pick one training dataset
train.dat = shapefile("./FinalUseMe/in.dat/s2-points.shp")
#
list.bands.in = list( c("InptLbl",     "b","g",    "r"))
             
list.out.folder = c("./RF/S2-v2/")
UseCores = detectCores() -1-5-5-1


##Generate Training Data 
train.dat2 =train.dat
train.dat = train.dat@data
train.dat$InptLbl = as.numeric(train.dat$InptLbl)
#train.dat$InptLbl = ifelse(train.dat$InptLbl==0,0,
 #                          ifelse(is.na(train.dat$InptLbl)==T,NA,1))
#train.dat = train.dat[!is.na(train.dat$InptLbl),]
##Depth Masking
rcl.matrix = matrix(c(-Inf,dep.val,1,dep.val,Inf,NA), ncol=3, byrow=T)
#depth.mask = clusterR(wv.dat$depth, reclassify, args=list(rcl=rcl.matrix))#works but is slower
depth.mask = reclassify(wv.dat$depth, rcl.matrix)
wv.dat = mask(x=wv.dat, mask=depth.mask)
#wv.dat = wv.dat[[bands.in[2:length(bands.in)]]]
#
wv.dat=getValues(wv.dat)
id.na = which(!is.na(wv.dat))#gives index of true values
wv.dat = na.omit(wv.dat)#remove NA
wv.dat=as.data.frame(wv.dat)
##
#wv.dat$ndvi = (wv.dat$re-wv.dat$r)/(wv.dat$re+wv.dat$r)
wv.dat$gn = wv.dat$r/wv.dat$g
#
rm(rcl.matrix)

for (k in 1:length(list.bands.in)){
#
out.folder = list.out.folder[k]
bands.in = list.bands.in[k]
bands.in = unlist(bands.in)
#

##Create directoriers
dir.create(out.folder)
dir.create(paste(out.folder,"/cvrun",sep=""))

#Define Model tuning
ras.value = train.dat[,bands.in]
ras.value[,1] = as.factor(ras.value[,1])#dependent value as a factor
rfFit = train(form = InptLbl ~ . , data = ras.value,
              method = "rf", tuneLength = (dim(ras.value)[2]-2),
              trControl = trainControl(method = "repeatedcv",number = 5,repeats = 10),
              verbose = TRUE)
saveRDS(rfFit, paste(out.folder,"tunemodel",sep=""))
##
##Define model with all training data and best tune
num.mtry = which.max(rfFit$results$Accuracy)+1
num.mtry = expand.grid(mtry = num.mtry)#only use best mtry evaulated above
rm(rfFit)
#
set.seed(805)
trainIndex = createMultiFolds(ras.value[,1], k = 5, times = 10)#generate mutliplte data splits
###
registerDoSEQ()#removes previous clusters
#Define how many cores you want to use and Register CoreCluster
cl  = makeCluster(UseCores)
registerDoParallel(cl) 
#
foreach(i=1:50) %dopar% {
  library("raster")
  library("caret")
  library("irr")
  #Out data
  full.model = paste(out.folder, "cvrun/", "k",i, ".rds",sep="")
  exp.file = paste(out.folder, "cvrun/", "k",i,".csv",sep="")
  out.ras = paste(out.folder, "cvrun/","k",i,".tif",sep="")
  #Build model
  ras.valueTrain = ras.value[ trainIndex[[i]],]
  ras.valueTest  = ras.value[-trainIndex[[i]],]
  rfFit.k = train(form = InptLbl ~ . , data = ras.valueTrain,
                  method = "rf", 
                  tuneGrid = data.frame(num.mtry),
                  trControl = trainControl(method = "none"),
                  verbose = FALSE)
  #
  saveRDS(rfFit.k$finalModel, full.model)
  ##
  set.seed(3)
  rfFit.test = predict(rfFit.k, ras.valueTest)
  #fix threshold based points
  rfFit.threshold = train.dat[-trainIndex[[i]],]
  rfFit.final = ifelse(rfFit.threshold$ndvi>=ndvi.thres , 2,
                       ifelse(rfFit.threshold$ndvi<ndvi.thres & rfFit.threshold$blue >= b.thres,1,
                              ifelse(rfFit.threshold$ndvi<ndvi.thres & rfFit.threshold$gn <= gr1, 1,
                                     ifelse(rfFit.threshold$ndvi<ndvi.thres & rfFit.threshold$gn>= gr, 1,
                                            rfFit.test))))
  #Evaulate model
  full.dat=as.data.frame(cbind(rfFit.test,ras.valueTest[,1]))
  conmat = table(full.dat)
  conmat = apply(conmat, 2, function(x) as.numeric(as.character(x)))
  conkapp = kappa2(full.dat, weight = c("unweighted"))
  n = sum(conmat)#number of observations
  oa = sum(diag(conmat))#number of correct classifications overall 
  oa.per = oa/n*100#percentage of correct classifications overall
  colsums = apply(conmat, 2, sum)
  PA = diag(conmat) / colsums*100
  rowsums = apply(conmat, 1, sum)
  UA = diag(conmat) / rowsums*100
  export = matrix(c(conmat,PA),nrow=nrow(conmat)+1,byrow=T)
  export = as.matrix(cbind(export,c(UA,oa.per),c(conkapp$value,conkapp$p.value,rep(NA,nrow(export)-2))))
  write.csv(export,exp.file, row.names = F)
  rm(export, full.dat,conmat,conkapp,n,oa,oa.per,colsums,PA,rowsums,UA)
  ##
  set.seed(3)
  out.dat = predict(rfFit.k, wv.dat)
  ##
  out.dat = as.numeric(out.dat)
  out.dat = out.dat-1
  out.dat= ifelse(wv.dat[,"ndvi"]>=ndvi.thres , 1,
                  ifelse(wv.dat[,"ndvi"]<ndvi.thres & wv.dat[,"b"] >= b.thres,0,
                         ifelse(wv.dat[,"ndvi"]<ndvi.thres & wv.dat[,"gn"] <= gr1, 0,
                                ifelse(wv.dat[,"ndvi"]<ndvi.thres & wv.dat[,"gn"] >= gr, 0,
                                       out.dat))))
  ##
  out.dat.raster = raster(depth.mask)#define a raster
  out.dat = as.integer(out.dat)#define as integer to reduce file size
  out.dat.raster[id.na] =  out.dat#save output
  writeRaster(out.dat.raster, out.ras, format="GTiff",NAflag = NaN,overwrite=T)
  rm(ras.valueTest,ras.valueTrain,rfFit.test,exp.file,out.dat, out.ras, out.dat.raster)
    rm(rfFit.k, full.model)
  }
#end cluster
stopCluster(cl)
registerDoSEQ()#removes previous clusters
rm(cl)
gc()
###
rm(num.mtry,trainIndex,ras.value)

#Generate Mean confusion matrix statistics
a = list.files(path=paste(out.folder, "cvrun/", sep=""), pattern=".csv")
ab = paste(out.folder, "cvrun/",a,sep="")
my.list = lapply(ab, read.csv)
my.list  = lapply(my.list , as.matrix)
rm(a,ab)
mat_rows =nrow(my.list[[1]])
len_mat = length(my.list[[1]])
vec = sapply(1:len_mat, function(j) mean(sapply(1:length(my.list), function(i) as.numeric(as.matrix(my.list[[i]])[j])), na.rm=TRUE))
final_mat = matrix(as.numeric(vec), nrow=mat_rows)
write.csv(final_mat,paste(out.folder, "CMsummary.csv", sep=""))
rm(mat_rows,len_mat,vec,final_mat,my.list)
##

##Read in all raster
a = list.files(path=paste(out.folder, "cvrun/", sep=""), pattern=".tif$")
ab = paste(out.folder, "cvrun/",a,sep="")
clas.dat = stack(ab)
rm(a,ab)
#
beginCluster()
#Probability
a = dim(clas.dat)[3]
get.prob = function(x) {round((sum(x)/a)*100,2)}
prob.class.dat = clusterR(clas.dat, calc, args=list(fun=get.prob),export='a')#Generate vegetated probability
#Modal
#getmode = function(v) {uniqv <- unique(v)
#uniqv[which.max(tabulate(match(v, uniqv)))]}
#mode.class.dat = clusterR(clas.dat, calc, args=list(fun=getmode))#Generate modal value 
##
writeRaster(prob.class.dat,paste(out.folder, "probability.tif", sep=""),
            format="GTiff",NAflag = NaN,overwrite=T) 
#writeRaster(mode.class.dat,paste(out.folder, "modal.tif", sep=""),
#            format="GTiff",NAflag = NaN,overwrite=T) 
##
endCluster()
rm(a,clas.dat)

##
class.dat = extract(prob.class.dat,train.dat2,df=T)
oa.all = rep(NA, 100)
kapp.all = rep(NA, 100)
for (i in 1:100){
  rf.thresh.lab = ifelse((class.dat[,2])>=i,1,0)
  full.dat = as.data.frame(cbind(rf.thresh.lab,train.dat$InptLbl ))
  #Evaulate model
  conmat = table(full.dat)
  conkapp = kappa2(full.dat, weight = c("unweighted"))
  n = sum(conmat)#number of observations
  oa = sum(diag(conmat))#number of correct classifications overall 
  oa.per = oa/n*100#percentage of correct classifications overall
  colsums = apply(conmat, 2, sum)
  PA = diag(conmat) / colsums*100
  rowsums = apply(conmat, 1, sum)
  UA = diag(conmat) / rowsums*100
  export = rbind(conmat,PA)
  export = as.data.frame(cbind(export,c(UA,oa.per),c(conkapp$value,conkapp$p.value,rep(NA,nrow(export)-2))))
  names(export) = c("bare","vegetated","UA","kappa-s-p")
  row.names(export) = c("bare","vegetated","PA")
  oa.all[i]=oa.per
  kapp.all[i] = conkapp$value
  rm(full.dat,conmat,conkapp,n,oa,oa.per,colsums,PA,rowsums,UA,rf.thresh.lab)
}
#write.csv(export,exp.file, row.names = T)
png(paste(out.folder, "threshold.png", sep=""))
par(mfrow=c(1,2),mai=c(0.8,0.8,0.1,0.1))
plot(1:100,oa.all,ylim=c(70,100), xlab=c("Veg Threshold"), ylab=c("OVerall Map Accuracy"),pch=20)
plot(1:100,kapp.all,ylim=c(0.4,1), xlab=c("Veg Threshold"), ylab=c("Kappa"),pch=20)
dev.off()
###
rm(list = ls()[!ls() %in% c("wv.dat", "dep.val", "ndvi.thres","b.thres","gr","gr1","train.dat","train.dat2","depth.mask","id.na",
                            "list.bands.in","list.out.folder","UseCores")])
removeTmpFiles(h=0)
gc()

}
