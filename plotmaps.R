library(raster)

class199903pca<-raster("D:/AASGoutputmangrove/nolakes_sand-dev/L5_TM_1999_03_23_15_22_43_014042_L2R.ncmangrove.tif")
map199908<-stack("D:/preprocessoutputmangrove/L5_TM_1999_08_14_15_21_27_014042_L2R.ncmangrove.tif")
map2016<-stack("C:/Users/cormi/Documents/ImageProcessing/Reference/PreProcessingOutput/Landsat8_Sept2016_mangrove.tif")
class199903nopca<-raster("D:/AASGoutputmangrove/nolakes_sand-dev-nopca/L5_TM_1999_03_23_15_22_43_014042_L2R.ncmangrove.tif")
class199903og<-raster("D:/AASGoutputmangrove/L5_TM_1999_03_23_15_22_43_014042_L2R.ncmangrove.tif")

in.folder = ("D:/AASGoutputmangrove/rasters")
sat.image.list=list.files(in.folder)

for (ii in 1:length(sat.image.list)){
  
  in.folder=("D:/AASGoutputmangrove/rasters")
  sat.image.list=list.files(in.folder)
  
  plot(raster(paste(in.folder,"/",sat.image.list[ii],sep="")),main=sat.image.list[ii])
  
}
