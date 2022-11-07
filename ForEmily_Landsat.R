rm(list=ls())
library("raster")
library("caret")
library("irr")
library("doParallel")  #Foreach Parallel Adaptor 
library("foreach")     #Provides foreach looping construct
rasterOptions(maxmemory=7e+09,chunksize = 4e+08)

setwd("C://Users//cormi//Documents//test//")

#Read in raster datasetwithout depth
wv.dat2 = stack("Landsat8_Sept2016sansdepth_copy.tif")
#wv.dat = stack("Landsat8_Sept2016_copy.tif")
names(wv.dat2) = c("b","g","r","n","ndvi","gndvi")
#names(wv.dat) = c("b","g","r","n", "depth","ndvi","gndvi")
#remove NAs across board
wv.dat <- mask(wv.dat2, calc(wv.dat2, fun = sum))
depth.mask=wv.dat[[1]]#only used for blank raster to fill
rm(wv.dat2)

#Pick one training dataset
train.dat = read.csv("C:\\Users\\cormi\\Documents\\test\\Trainingdata.csv")
#traindat.shp = SpatialPointsDataFrame(coords=train.dat[,3:2],data=train.dat, proj4string=CRS(("+proj=longlat +datum=WGS84 +units=m +no_defs")))
#train.dat2 <- spTransform(traindat.shp, crs(wv.dat))
#train.dat3=extract(wv.dat,train.dat2, df=T)

#
list.bands.in = list( c("Class",     "b","g",  "r"))
list.out.folder = c("./RF/")
UseCores = detectCores()-1

wv.dat = wv.dat[[c( "b", "g" , "r" )]]
wv.dat=getValues(wv.dat)
id.na = which(!is.na(wv.dat[,1]))#gives index of true values
wv.dat = na.omit(wv.dat)#remove NA
wv.dat=as.data.frame(wv.dat)
##
gc()
#

for (k in 1:length(list.bands.in)){
#
bands.in = list.bands.in[k]
bands.in = unlist(bands.in)
out.folder = paste(bands.in, collapse = "-")
out.folder = strsplit(out.folder, "Class-" )[[1]][2]
out.folder = paste0(list.out.folder,out.folder,"/")
#

##Create directoriers
dir.create(out.folder)
dir.create(paste(out.folder,"/cvrun",sep=""))

#Define Model tuning
ras.value = train.dat[,bands.in]
ras.value[,1] = as.factor(ras.value[,1])#dependent value as a factor
ras.value = na.omit(ras.value)
rfFit = train(form = Class ~ . , data = ras.value,
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
#registerDoSEQ()#removes previous clusters
#Define how many cores you want to use and Register CoreCluster
#cl  = makeCluster(UseCores)
#registerDoParallel(cl) 
}
#
#foreach(i=1:50) %dopar% {
for(i in 1:50){
  library("raster")
  library("caret")
  library("irr")
  i=2
  #Out data
  full.model = paste(out.folder, "cvrun/", "k",i, ".rds",sep="")
  exp.file = paste(out.folder, "cvrun/", "k",i,".csv",sep="")
  out.ras = paste(out.folder, "cvrun/","k",i,".tif",sep="")
  #Build model
  ras.valueTrain = ras.value[ trainIndex[[i]],]
  ras.valueTest  = ras.value[-trainIndex[[i]],]
  rfFit.k = train(form = Class ~ . , data = ras.valueTrain,
                  method = "rf",
                  tuneGrid = data.frame(num.mtry),
                  trControl = trainControl(method = "none"),
                  verbose = FALSE)
  #
  saveRDS(rfFit.k$finalModel, full.model)
  ##
  set.seed(3)
  rfFit.final = predict(rfFit.k, ras.valueTest)
  
  #Evaluate model
  full.dat=as.data.frame(cbind(rfFit.final,ras.valueTest[,1]))
  
  if (length(unique(full.dat[,1])) == length(unique(full.dat[,2]))){
    conmat = table(full.dat)
  }else{
    conmat = table(full.dat)
    miss.var = which(unique(full.dat[,2])%in%unique(full.dat[,1])==F)
    conmat2=rbind(conmat, c(0,0,0))
    rownames(conmat2) = c(rownames(conmat), miss.var)
    fixind = order(row.names(conmat2), decreasing=F)
    conmat2=conmat2[fixind,]
    conmat=conmat2
    rownames(conmat)=unique(full.dat[,2])
    rm(conmat2,miss.var,fixind)
  }
}
  
  conmat = apply(conmat, 2, function(x) as.numeric(as.character(x)))
  conkapp = kappa2(full.dat, weight = c("unweighted"))
  n = sum(conmat)#number of observations
  oa = sum(diag(conmat))#number of correct classifications overall 
  oa.per = oa/n*100#percentage of correct classifications overall
  colsums = apply(conmat, 2, sum)
  PA = diag(conmat) / colsums*100
  rowsums = apply(conmat, 1, sum)
  UA = diag(conmat) / rowsums*100
  export = rbind(conmat,PA)
  export = as.matrix(cbind(export,c(UA,oa.per),c(conkapp$value,conkapp$p.value,rep(NA,nrow(export)-2))))
  write.csv(export,exp.file, row.names = F)
  rm(export, full.dat,conmat,conkapp,n,oa,oa.per,colsums,PA,rowsums,UA)
  ##
  set.seed(3)
  out.dat = predict(rfFit.k, wv.dat)
  out.dat.raster = raster(depth.mask)#define a raster
  out.dat = as.integer(out.dat)#define as integer to reduce file size
  out.dat.raster[id.na] =  out.dat#save output
  #out.dat = out.dat-1
  writeRaster(out.dat.raster, out.ras, format="GTiff",NAflag = NaN,overwrite=T)
  ##
  rm(ras.valueTest,ras.valueTrain,exp.file,out.dat, out.ras, out.dat.raster)
    rm(rfFit.k, full.model)
  }
#end cluster
#stopCluster(cl)
#registerDoSEQ()#removes previous clusters
#rm(cl)
gc()
###
rm(num.mtry,trainIndex,ras.value)

#Generate Mean confusion matrix statistics
a = list.files(path=paste(out.folder, "/cvrun", sep=""), pattern=".csv")
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




##Read in all raster2
a = list.files(path=paste(out.folder, "/cvrun/", sep=""), pattern=".tif$")
ab = paste(out.folder, "cvrun/",a,sep="")
clas.dat = stack(ab)
rm(a,ab)
#
beginCluster()
#Modal
getmode = function(v) {uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]}
mode.class.dat = clusterR(clas.dat, calc, args=list(fun=getmode))#Generate modal value 
writeRaster(mode.class.dat,paste(out.folder, "modalmf.tif", sep=""),
            format="GTiff",NAflag = NaN,overwrite=T) 
endCluster()







##Read in all raster
a = list.files(path=paste(out.folder, "cvrun/", sep=""), pattern=".tif$")
ab = paste(out.folder, "cvrun/",a,sep="")
clas.dat = stack(ab)
writeRaster(clas.dat, paste(out.folder, "kstack.tif", sep=""),
            format="GTiff",NAflag = NaN,overwrite=T) 
rm(a,ab)
#
beginCluster()
#Probability
a = dim(clas.dat)[3]
get.prob = function(x) {round((sum(x)/a)*100,2)}
prob.class.dat = clusterR(clas.dat, calc, args=list(fun=get.prob),export='a')#Generate vegetated probability
writeRaster(prob.class.dat,paste(out.folder, "probability.tif", sep=""),
            format="GTiff",NAflag = NaN,overwrite=T) 

rm(clas.dat)

###
rm(list = ls()[!ls() %in% c("wv.dat", "dep.val", "ndvi.thres","b.thres","gr","train.dat","depth.mask","id.na",
                            "list.bands.in","list.out.folder","UseCores")])
removeTmpFiles(h=0)

gc()
}
