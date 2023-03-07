rm(list=ls())
library("MLmetrics")
library("raster")
library("caret")
library("irr")
library("doParallel")  #Foreach Parallel Adaptor 
library("foreach")     #Provides foreach looping construct
library(sf)
library(rgdal)
rasterOptions(maxmemory=7e+09,chunksize = 4e+08)

#Read in raster datasetwithout depth
#wv.dat2 = stack("Landsat8_Sept2016sansdepth_copy.tif")
wv.dat = stack("C:/Users/Cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif")
#names(wv.dat2) = c("b","g","r","n","ndvi","gndvi")
names(wv.dat) = c("blue","green","red","swir1","ndvi", "cmri", "pca1")
#remove NAs across board
wv.dat <- mask(wv.dat, calc(wv.dat, fun = sum))
depth.mask=wv.dat[[1]]#only used for blank raster to fill
#rm(wv.dat2)

#Pick one training dataset as shapefile ( I created this one in ArcGIS as a point file)
mydata=read.csv("C:/Users/cormi/Documents/ImageProcessing/mangrove/Mangrove_trainingData.csv")
#transform csv into shapefile
#data.shp = SpatialPointsDataFrame(coords=mydata[,2:3],data=mydata, proj4string=CRS("+proj=longlat +datum=WGS84 +units=m +no_defs"))
#extract values from raster
data.shp = read_sf("C:/Users/cormi/Documents/ImageProcessing/mangrove/shapefile/Mangrove_trainingData.shp")
test = extract(wv.dat, data.shp,df=T)
#create matrix of original csv data points and extracted data
mydata = cbind(mydata, test[,2:8])
#rm na data points
mydata2=na.omit(mydata)
head(mydata2)
rm(test)
##

write.csv(mydata2,"C:/Users/cormi/Documents/ImageProcessing/Reference/InSituData/Trainingdatamangrove.csv", row.names = F)
train.dat = mydata2
#traindat.shp = SpatialPointsDataFrame(coords=train.dat[,3:2],data=train.dat, proj4string=CRS(("+proj=longlat +datum=WGS84 +units=m +no_defs")))
#train.dat2 <- spTransform(traindat.shp, crs(wv.dat))
#train.dat3=extract(wv.dat,train.dat2, df=T)

#
#names the columns that should be used in rf model (category of habitat, bgr bands)
list.bands.in = list( c("Id",     "blue","green","red","swir1","ndvi", "cmri", "pca1"))
list.out.folder = c("C:/Users/cormi/Documents/ImageProcessing/mangrove")
UseCores = detectCores()-1

#using only b, g, r, layers, this will be used later as the map that the rf model 
#predicts onto
wv.dat = wv.dat[[c( "blue","green","red","swir1","ndvi", "cmri", "pca1" )]]
#gets numerical values from band layers
wv.dat=getValues(wv.dat)
#determines where nas exist in dataset
id.na = which(!is.na(wv.dat[,1]))#gives index of true values
wv.dat = na.omit(wv.dat)#remove NA
#created data frame
wv.dat=as.data.frame(wv.dat)
##
gc()
#

for (k in 1:length(list.bands.in)){
#this runs a loop using the bands in, but only ever works with 1 as k?
bands.in = list.bands.in[k]
bands.in = unlist(bands.in)
#creating b-g-r folder
out.folder = paste(bands.in, collapse = "-")
#what is this line doing? what do the 1 and 2 refer to?
out.folder = strsplit(out.folder, "Id-" )[[1]][2]
out.folder = paste0(list.out.folder,out.folder,"/")
#

##Create directoriers
dir.create(out.folder)
#create folder cvrun
dir.create(paste(out.folder,"/cvrun",sep=""))

#Define Model tuning
#,bands.in selected will change based on k value in beginning of loop
ras.value = train.dat[,bands.in]
ras.value[,1] = as.factor(ras.value[,1])# makes sure dependent value is a factor
#remove nas from file
ras.value = na.omit(ras.value)
#fit model based on training data
#Class~. '~.' means all other columns in dataset are predictor variables (x variables)
#tunelength = the number of columns -2, dim gives dimensions of raster (#rows # columns)
# then ras.value[2] specifies the 2nd value in dim (columns) then -2 
#tune lenght is the number of values used in rf, will choose out of the 2 values in this case
#method = which value we are trying to optimise (ROC in this case, similar to AUC, sensitivity vs specificty)
#not surte if this is right but going to try it out
#train control is a set of intructions on how I want caret to train the model
#: method = repeated cross validation, number = 5 folds
#save predictions just saves final hold out predictions?
#summary function balances between sensitivity and specificity (twoClassSummary)
#and recall and precision, better for very imbalnced datasets (prSummary)
#I am trying prSummary because we have more than two classes? not sure if that's right
#can't use pr summary because I have 3 levels
rfFit = train(form = Id ~ . , data = ras.value,
            method = "rf", tuneLength = (dim(ras.value)[2]-2),
            trControl = trainControl(method = "repeatedcv",number = 5,repeats = 10),
           verbose = TRUE)

saveRDS(rfFit, paste(out.folder,"tunemodel",sep=""))

# a test on a different type of cunfusion matrix based on youtube video
confusionMatrix(reference = ras.valueTest$Class, data = rfFit, mode = "everything", positive = "Good")
##

##Define model with all training data and best tune
#tells you whichy mtry was the most accurate, print rfFit if you want to know more about this
num.mtry = which.max(rfFit$results$Accuracy)+1
#mtry = the number of variables randomly selected for splitting at each node
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
#if you get the ERROR Fold3.Rep07: mtry=7 Error in randomForest.default(x, y, mtry = param$mtry, ...) : 
#Can't have empty classes in y, this means one of your classes does not have enough samples and this means that
#it was not predicted to exist anywhere in your random forest, remove this class, combine it with another class or 
#collect more data points for this class (virtually through online images, or in person)

#foreach(i=1:50) %dopar% {
#runs 50 different iterations of rf
for(i in 1:50){
  library("raster")
  library("caret")
  library("irr")
  #Out data
  full.model = paste(out.folder, "cvrun/", "k",i, ".rds",sep="")
  exp.file = paste(out.folder, "cvrun/", "k",i,".csv",sep="")
  out.ras = paste(out.folder, "cvrun/","k",i,".tif",sep="")
  #Build model
  #split training data into two groups, training and testing data
  ras.valueTrain = ras.value[ trainIndex[[i]],]
  ras.valueTest  = ras.value[-trainIndex[[i]],]
  #this is the random forest model being trained with our training data to predict habitat by
  #reflectence values, see information above
  #this model now uses best mtry tuning parameter value to assess data (which we defined earlier)
  rfFit.k = train(form = Id ~ . , data = ras.valueTrain,
                  method = "rf",
                  tuneGrid = data.frame(num.mtry),
                  trControl = trainControl(method = "none"),
                  verbose = FALSE)
  #method = none means tell caret train the model one time on the whole training set
  saveRDS(rfFit.k$finalModel, full.model)
  ##
  set.seed(3)
  #predict new observations from testing subset
  rfFit.final = predict(rfFit.k, ras.valueTest)
  
  #Evaluate model on your test data, gives a table of what your test data says vs. what the mdoel predicts
  full.dat=as.data.frame(cbind(rfFit.final,ras.valueTest[,1]))
  # a test on a different type of cunfusion matrix based on youtube video
  confusionMatrix(reference = ras.valueTest$Id, data = rfFit.final, mode = "everything", positive = "Good")
}

confusionMatrix(reference = ras.valueTest$Id, data = rfFit.final, mode = "everything", positive = "Good")

#skipe the part below.... could not get it to work and confusion matrix above is just as helpful

  #a confusion matrix evaluates whicho be the same----
  if (length(unique(full.dat[,1])) == length(unique(full.dat[,2]))){
    conmat = table(full.dat)
  }else{
    conmat = table(full.dat)
    miss.var = which(unique(full.dat[,2])%in%unique(full.dat[,1])==F)
    #number of 0s has to equal number of classes inputted
    conmat2=rbind(conmat, c(0,0,0))
    #Error in dimnames(x) <- dn : 
    #length of 'dimnames' [1] not equal to array extent
    #rownames(conmat2) = c(rownames(conmat), miss.var)
    rownames(conmat2) = c(rownames(conmat),miss.var)
    fixind = order(row.names(conmat2), decreasing=F)
    conmat2=conmat2[fixind,]
    conmat=conmat2
    rownames(conmat)=unique(full.dat[,2])
    rm(conmat2,miss.var,fixind)
  }

  conmat = apply(conmat, 2, function(x) as.numeric(as.character(x)))
  #gives metric of cohen's kappa 
  conkapp = kappa2(full.dat, weight = c("unweighted"))
  n = sum(conmat)#number of observations
  oa = sum(diag(conmat))#number of correct classifications overall 
  oa.per = oa/n*100#percentage of correct classifications overall
  colsums = apply(conmat, 2, sum)
  #percent accuracy for each category (seagrass, mangrove etc.)
  PA = diag(conmat) / colsums*100
  rowsums = apply(conmat, 1, sum)
  UA = diag(conmat) / rowsums*100
  export = rbind(conmat,PA)
  export = as.matrix(cbind(export,c(UA,oa.per),c(conkapp$value,conkapp$p.value,rep(NA,nrow(export)-2))))
  write.csv(export,"C:/Users/cormi/Documents/ImageProcessing/Reference/exp.file.csv", row.names = F)
  rm(export, full.dat,conmat,conkapp,n,oa,oa.per,colsums,PA,rowsums,UA)
  ##
  set.seed(3)
  
  
  
  #finally plot results onto full dataset----
  out.dat = predict(rfFit.k, wv.dat)
  #plot the map of the predicted results
  out.dat.raster = raster(depth.mask)#define a raster
  out.dat = as.integer(out.dat)#define as integer to reduce file size
  out.dat.raster[id.na] =  out.dat#save output
  #out.dat = out.dat-1
  writeRaster(out.dat.raster, "C:/Users/cormi/Documents/ImageProcessing/Reference/out.ras.mangrove", format="GTiff",NAflag = NaN,overwrite=T)
  ##
  rm(ras.valueTest,ras.valueTrain,exp.file,out.dat, out.ras, out.dat.raster)
    rm(rfFit.k, full.model)
  
#end cluster
#stopCluster(cl)
#registerDoSEQ()#removes previous clusters
#rm(cl)
gc()
###
rm(num.mtry,trainIndex,ras.value)


#Creating maps with predicted percent chance that a class is what it says it is----
#may not work with 2 classes, see email from Kristen
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



#options given by kristen to fix 2 class issue
##Read in all raster2
#read in the 50 rasters/ name the ones in file
a = list.files(path=paste(out.folder, "/cvrun/", sep=""), pattern=".tif$")
#add file path for r
ab = paste(out.folder, "cvrun/",a,sep="")
# stack these rasters together
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
