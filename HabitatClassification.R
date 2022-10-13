library("raster")
library("caret")
library("irr")
library("doParallel")  #Foreach Parallel Adaptor 
library("foreach")     #Provides foreach looping construct
rasterOptions(maxmemory=7e+09,chunksize = 4e+08)

setwd("C://Users//wilsonkri//Documents//Backup-WV//")

#Read in raster dataset
wv.dat = stack("LunenburgBackHarbour//Rasters//BOA-8mmask.tif",
               "LunenburgBackHarbour//Rasters//WCC-8mm.tif",
               "LunenburgBackHarbour//Rasters//PCA-nostand-BOA-6b-8mm.tif",
               "Depth//LBH//DepthNONNA8mmask.tif")
names(wv.dat) = c( "cb", "b", "g" , "y"  , "r" ,"re" , "n1", "n2" ,        
                   "diicbb","diicbg","diicby","diicbr","diibg","diiby", "diibr" ,"diigy", "diigr"  , "diiyr",           
                    "pca6.1","pca6.2",  "pca6.3", "pca6.4", "pca6.5" , "pca6.6",
                   "depth")
wv.dat = mask(wv.dat, wv.dat$cb)
wv.dat = mask(wv.dat, wv.dat$diicbb)
wv.dat = mask(wv.dat, wv.dat$pca6.1)
wv.dat = mask(wv.dat, wv.dat$depth)
depth.mask=wv.dat[[1]]#only used for blank raster to fill

#Pick one training dataset
train.dat = read.csv("LunenburgBackHarbour//FieldDataTransect3mBuffer.csv")
#train.dat$inptlbl = ifelse(train.dat$inptlbl>1,1,0)
#train.dat = train.dat[train.dat$site!="BackHbr_8",]
#train.dat = train.dat[train.dat$site!="BackHbr_14",]
#train.dat = train.dat[train.dat$site!="BackHbr_17",]
#train.dat = train.dat[train.dat$site!="BackHbr_19",]
#19 maybe problem
#site 8,14,17 might be problematic
#newdata = read.csv( "LunenburgBackHarbour//ExtraFiles//AddDWSand.csv")
#train.dat = merge(train.dat,newdata,all=T)
#train.dat = train.dat[,c(1:30)]
#rm(newdata)
#
list.bands.in = list( c("inptlbl",     "pca6.1","pca6.2",  "pca6.3","depth"),
                      c("inptlbl",     "pca6.1","pca6.2",  "pca6.3"),
                      c("inptlbl",    "diibg","diiby", "diibr" ,"diigy", "diigr"  , "diiyr"),
                      c("inptlbl",    "diibg","diiby", "diibr" ,"diigy", "diigr"  , "diiyr","depth"),
                      c("inptlbl", "diicbb","diicbg","diicby","diicbr","diibg","diiby", "diibr" ,
                        "diigy", "diigr"  , "diiyr" ),
                      c("inptlbl",  "cb","b","g","y","r","re","depth","ndvi","rg"),
                      c("inptlbl",  "b","g","y","r","depth","ndvi","rg"),
                      c("inptlbl",  "cb","b","g","y","r","re","depth"),
                      c("inptlbl",  "b","g","y","r","depth"),
                      c("inptlbl",  "cb","b","g","y","r","re"),
                      c("inptlbl",  "b","g","y","r"),
                      c("inptlbl", "cb", "b", "g" , "y"  , "r" ,"re" , "n1", "n2" ,        
                          "diicbb","diicbg","diicby","diicbr","diibg","diiby", "diibr" ,"diigy", "diigr"  , "diiyr",           
                          "pca6.1","pca6.2",  "pca6.3", 
                          "depth","ndvi","rg"),
                      c("inptlbl", "cb", "b", "g" , "y"  , "r" ,"re" , "n1", "n2" ,        
                        "diicbb","diicbg","diicby","diicbr","diibg","diiby", "diibr" ,"diigy", "diigr"  , "diiyr",           
                        "pca6.1","pca6.2",  "pca6.3", 
                        "depth"))
list.out.folder = c("./LunenburgBackHarbour/RF/")
UseCores = detectCores()-6 #-1-5-5-1


##
wv.dat = wv.dat[[c( "cb", "b", "g" , "y"  , "r" ,"re" ,        
                    "diicbb","diicbg","diicby","diicbr","diibg","diiby", "diibr" ,"diigy", "diigr"  , "diiyr",           
                    "pca6.1","pca6.2",  "pca6.3", "pca6.4", 
                    "depth")]]
wv.dat=getValues(wv.dat)
id.na = which(!is.na(wv.dat[,1]))#gives index of true values
wv.dat = na.omit(wv.dat)#remove NA
wv.dat=as.data.frame(wv.dat)
##
wv.dat$ndvi = (wv.dat$re-wv.dat$r)/(wv.dat$re+wv.dat$r)
wv.dat$rg = wv.dat$r/wv.dat$g
gc()
#

for (k in 1:length(list.bands.in)){
#
bands.in = list.bands.in[k]
bands.in = unlist(bands.in)
out.folder = paste(bands.in, collapse = "-")
out.folder = strsplit(out.folder, "inptlbl-" )[[1]][2]
out.folder = paste0(list.out.folder,out.folder,"/")
#

##Create directoriers
dir.create(out.folder)
dir.create(paste(out.folder,"/cvrun",sep=""))
dir.create(paste(out.folder,"/cvrunmf",sep=""))

#Define Model tuning
ras.value = train.dat[,bands.in]
ras.value[,1] = as.factor(ras.value[,1])#dependent value as a factor
rfFit = train(form = inptlbl ~ . , data = ras.value,
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
  out.ras1 = paste(out.folder, "cvrunmf/","k",i,"-mf.tif",sep="")
  exp.file1 = paste(out.folder, "cvrunmf/", "k",i,".csv",sep="")
  #Build model
  ras.valueTrain = ras.value[ trainIndex[[i]],]
  ras.valueTest  = ras.value[-trainIndex[[i]],]
  rfFit.k = train(form = inptlbl ~ . , data = ras.valueTrain,
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
  out.dat.raster = out.dat.raster-1
  writeRaster(out.dat.raster, out.ras, format="GTiff",NAflag = NaN,overwrite=T)
  ##Add focal filter
  mj.raster = focal(out.dat.raster, w=matrix(1,3,3), modal,na.rm=T)
  mj.raster = mask(x=mj.raster, mask=depth.mask)
  writeRaster(mj.raster, out.ras1, format="GTiff",NAflag = NaN,overwrite=T)
  mj.test = extract(mj.raster,  train.dat[-trainIndex[[i]],"cell"])
  full.dat=as.data.frame(cbind( mj.test,ras.valueTest[,1]))
  full.dat$V2 = full.dat$V2-1
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
  export = rbind(conmat,PA)#
  export = as.matrix(cbind(export,c(UA,oa.per),c(conkapp$value,conkapp$p.value,rep(NA,nrow(export)-2))))
  write.csv(export,exp.file1, row.names = F)
  rm(export, full.dat,conmat,conkapp,n,oa,oa.per,colsums,PA,rowsums,UA)
  ##
  rm(ras.valueTest,ras.valueTrain,exp.file,out.dat, out.ras, out.dat.raster)
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

#Generate Mean confusion matrix statistics2
a = list.files(path=paste(out.folder, "cvrunmf/", sep=""), pattern=".csv")
ab = paste(out.folder, "cvrunmf/",a,sep="")
my.list = lapply(ab, read.csv)
my.list  = lapply(my.list , as.matrix)
rm(a,ab)
mat_rows =nrow(my.list[[1]])
len_mat = length(my.list[[1]])
vec = sapply(1:len_mat, function(j) mean(sapply(1:length(my.list), function(i) as.numeric(as.matrix(my.list[[i]])[j])), na.rm=TRUE))
final_mat = matrix(as.numeric(vec), nrow=mat_rows)
write.csv(final_mat,paste(out.folder, "CMsummary-mf.csv", sep=""))
rm(mat_rows,len_mat,vec,final_mat,my.list)
##

##Read in all raster
a = list.files(path=paste(out.folder, "/cvrun/", sep=""), pattern=".tif$")
ab = paste(list.out.folder,out.folder, "/cvrun/",a,sep="")
clas.dat = stack(ab)
rm(a,ab)
#
beginCluster()
#Probability
#a = dim(clas.dat)[3]
#get.prob = function(x) {round((sum(x)/a)*100,2)}
#prob.class.dat = clusterR(clas.dat, calc, args=list(fun=get.prob),export='a')#Generate vegetated probability
#writeRaster(prob.class.dat,paste(out.folder, "probability.tif", sep=""),
#            format="GTiff",NAflag = NaN,overwrite=T) 
#Modal
getmode = function(v) {uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]}
mode.class.dat = clusterR(clas.dat, calc, args=list(fun=getmode))#Generate modal value 
writeRaster(mode.class.dat,paste(out.folder, "modal.tif", sep=""),
            format="GTiff",NAflag = NaN,overwrite=T) 
endCluster()
rm(clas.dat)

##Read in all raster2
a = list.files(path=paste(ut.folder, "/cvrunmf/", sep=""), pattern=".tif$")
ab = paste(out.folder, "cvrunmf/",a,sep="")
clas.dat = stack(ab)
rm(a,ab)
#
beginCluster()
#Probability
#a = dim(clas.dat)[3]
#get.prob = function(x) {round((sum(x)/a)*100,2)}
#prob.class.dat = clusterR(clas.dat, calc, args=list(fun=get.prob),export='a')#Generate vegetated probability
#writeRaster(prob.class.dat,paste(out.folder, "probabilitymf.tif", sep=""),
           # format="GTiff",NAflag = NaN,overwrite=T) 
#Modal
getmode = function(v) {uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]}
mode.class.dat = clusterR(clas.dat, calc, args=list(fun=getmode))#Generate modal value 
writeRaster(mode.class.dat,paste(out.folder, "modalmf.tif", sep=""),
            format="GTiff",NAflag = NaN,overwrite=T) 
endCluster()
##
rm(clas.dat)


###
rm(list = ls()[!ls() %in% c("wv.dat", "dep.val", "ndvi.thres","b.thres","gr","train.dat","depth.mask","id.na",
                            "list.bands.in","list.out.folder","UseCores")])
removeTmpFiles(h=0)
gc()

}
