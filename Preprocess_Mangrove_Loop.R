rm(list=ls())
gc()

##
folder = "D:/20122013"
#the list of satellite images that are in your folder (just their L1 product identifier)
sat.data = read.csv("D:/SatelliteImagesDownloaded2012_2013.csv")
#turns the csv file of images and other details into just a list of satellite names
sat.images.list = sat.data$Landsat.Product.Identifier.L1
#this was another optiojn I tried, but I couldn't figure out how to get it to work
#sat.images.list = (list.dirs(folder))
#sat.images.list  = sat.images.list[-1]

rm(sat.data)

#this starts a loop which runs through each satellite in the folder specified above
#based on the list of satellite images from sat.images.list, everything within the {}
# will be run


###pre-process_Lyzenga

rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
library("sf")
library("RStoolbox")

out.folder = "D:/output"
sat.images.list2 = (list.files(out.folder))
#sat.images.list2  = sat.images.list2[-1]

for (ii in 1:length(sat.images.list2)) {
  
  out.folder = "D:/output"
  sat.images.list2 = (list.files(out.folder))
  #sat.images.list2  = sat.images.list2[-1]

  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  
  #import satellite data nc file
  nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
  nc = nc_open(nc.dat)
  nc_atts <- ncatt_get(nc, 0)
  ##
  
      if (nc_atts$sensor == "L5_ETM"){       
  
      out.folder = "D:/output"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/finaloutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      #import created landmask
      land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      ####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      ##
      blue1 = t(ncvar_get(nc, nc$var$rhos_486))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_571))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_660))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_839))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1678))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2217))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project land to same crs and extent as other layers
      land = projectRaster(land, blue, method="bilinear")
      
      #same process with cloud is followed for neg mask
      #switched nagetive mask to opporite so that it can work with inverse land mask, now all negative pixels will be 1
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      
      #makeroom
      rm(blue1,green1,red1,nir1,swir11,swir21)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      neg.mask1 = blue
      neg.mask1[] = neg.mask
      
      #create mask
      rmaskrs = neg.mask1*land
      plot(rmaskrs)
      
      #makeroom
      rm(neg.mask,neg.mask1,land)
      
      ##createndvilayer
      ndvi = (nir-red)/(nir+red)
      
      ##mangrove index (cmri(file:///C:/Users/cormi/Downloads/JARS-190642_online.pdf)
      cmri = ((nir-red)/(nir+red))-((nir-green)/(nir+green))
      
      #mangrove vegetation index (mvi https://www.sciencedirect.com/science/article/pii/S0924271620301519?casa_token=ObaJW3vJD3cAAAAA:yPTdiC0JGAx7YRCLuC_OIDEhZx39S86ec-dEhvZzkmbtkbxe51DUOH4aHU51p-_MdTmRpEkh)
      #mvi=((nir-green)/(swir1-green))
      #tried using this but just got a blank raster...
      
      #create pca
      pca.stack=stack(blue,green,red,nir,swir1,swir2,ndvi,cmri)
      pca.stack = mask(x=pca.stack, mask = rmaskrs)
      pca.dat = rasterPCA(pca.stack[[1:5]], spca=F)
      pca.dat = pca.dat$map
      
      #stack all layers together
      dat.stack = stack(blue, green, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","swir1","ndvi", "cmri", "pca1")
      
      rm(blue,green,red,nir,swir1,ndvi,cmri,pca.dat)
      
      ##cloud mask written by kristen
      #create map for cloud mask of layer 1373, because now the other data is cropped and this is not
      
      #cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      #cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      #cloud = raster(cloud)
      #proj4string(cloud)  = crs(nc_atts$proj4_string)
      #extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      #cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      #cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #to create reverse land mask
      dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
   
    
} else if (nc_atts$sensor == "L7_ETM"){
      
       out.folder = "D:/output"
       sat.images.list2 = (list.files(out.folder))
       write.data = paste("D:/finaloutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
       nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")  
       #import created landmask
       land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      ####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      ##
      blue1 = t(ncvar_get(nc, nc$var$rhos_479))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_561))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_661))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_835))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1650))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2208))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project land to same crs and extent as other layers
      land = projectRaster(land, blue, method="bilinear")
      
      #same process with cloud is followed for neg mask
      #switched nagetive mask to opporite so that it can work with inverse land mask, now all negative pixels will be 1
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      
      #makeroom
      rm(blue1,green1,red1,nir1,swir11,swir21)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      neg.mask1 = blue
      neg.mask1[] = neg.mask
      
      #create mask
      rmaskrs = neg.mask1*land
      plot(rmaskrs)
      
      #makeroom
      rm(neg.mask,neg.mask1,land)
      
      ##createndvilayer
      ndvi = (nir-red)/(nir+red)
      
      ##mangrove index (cmri(file:///C:/Users/cormi/Downloads/JARS-190642_online.pdf)
      cmri = ((nir-red)/(nir+red))-((nir-green)/(nir+green))
      
      #mangrove vegetation index (mvi https://www.sciencedirect.com/science/article/pii/S0924271620301519?casa_token=ObaJW3vJD3cAAAAA:yPTdiC0JGAx7YRCLuC_OIDEhZx39S86ec-dEhvZzkmbtkbxe51DUOH4aHU51p-_MdTmRpEkh)
      #mvi=((nir-green)/(swir1-green))
      #tried using this but just got a blank raster...
      
      #create pca
      pca.stack=stack(blue,green,red,nir,swir1,swir2,ndvi,cmri)
      pca.stack = mask(x=pca.stack, mask = rmaskrs)
      pca.dat = rasterPCA(pca.stack[[1:5]], spca=F)
      pca.dat = pca.dat$map
      
      #stack all layers together
      dat.stack = stack(blue, green, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","swir1","ndvi", "cmri", "pca1")
      
      rm(blue,green,red,nir,swir1,ndvi,cmri,pca.dat)
      
      ##cloud mask written by kristen
      #create map for cloud mask of layer 1373, because now the other data is cropped and this is not
      
      #cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      #cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      #cloud = raster(cloud)
      #proj4string(cloud)  = crs(nc_atts$proj4_string)
      #extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      #cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      #cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #to create reverse land mask
      dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
   
} else if (nc_atts$sensor == "L8_OLI") {  
    
      out.folder = "D:/output"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/finaloutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      #import created landmask
      land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      ####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      ##
      blue1 = t(ncvar_get(nc, nc$var$rhos_483))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_561))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_655))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_865))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1609))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2201))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project land to same crs and extent as other layers
      land = projectRaster(land, blue, method="bilinear")
      
      #same process with cloud is followed for neg mask
      #switched nagetive mask to opporite so that it can work with inverse land mask, now all negative pixels will be 1
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      
      #makeroom
      rm(blue1,green1,red1,nir1,swir11,swir21)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      neg.mask1 = blue
      neg.mask1[] = neg.mask
      
      #create mask
      rmaskrs = neg.mask1*land
      plot(rmaskrs)
      
      #makeroom
      rm(neg.mask,neg.mask1,land)
      
      ##createndvilayer
      ndvi = (nir-red)/(nir+red)
      
      ##mangrove index (cmri(file:///C:/Users/cormi/Downloads/JARS-190642_online.pdf)
      cmri = ((nir-red)/(nir+red))-((nir-green)/(nir+green))
      
      #mangrove vegetation index (mvi https://www.sciencedirect.com/science/article/pii/S0924271620301519?casa_token=ObaJW3vJD3cAAAAA:yPTdiC0JGAx7YRCLuC_OIDEhZx39S86ec-dEhvZzkmbtkbxe51DUOH4aHU51p-_MdTmRpEkh)
      #mvi=((nir-green)/(swir1-green))
      #tried using this but just got a blank raster...
      
      #create pca
      pca.stack=stack(blue,green,red,nir,swir1,swir2,ndvi,cmri)
      pca.stack = mask(x=pca.stack, mask = rmaskrs)
      pca.dat = rasterPCA(pca.stack[[1:5]], spca=F)
      pca.dat = pca.dat$map
      
      #stack all layers together
      dat.stack = stack(blue, green, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","swir1","ndvi", "cmri", "pca1")
      
      rm(blue,green,red,nir,swir1,ndvi,cmri,pca.dat)
      
      ##cloud mask written by kristen
      #create map for cloud mask of layer 1373, because now the other data is cropped and this is not
      
      #cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      #cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      #cloud = raster(cloud)
      #proj4string(cloud)  = crs(nc_atts$proj4_string)
      #extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      #cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      #cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #to create reverse land mask
      dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
} else {
     
      out.folder = "D:/output"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/finaloutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      #import created landmask
      land = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/landmaskBimini.tif")
      
      ####take out blue layer for extent matching and coordinate refrence systsem (crs) matching
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      ##
      blue1 = t(ncvar_get(nc, nc$var$rhos_482))
      blue = raster(blue1)
      ##changeing projection with project4string
      proj4string(blue)  = crs(nc_atts$proj4_string)
      ##clipping the blue to nc atts extent
      extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##green
      ###get rest of important bands to create final image
      green1 = t(ncvar_get(nc, nc$var$rhos_561))
      green = raster(green1)
      proj4string(green)  = crs(nc_atts$proj4_string)
      extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##red
      red1 = t(ncvar_get(nc, nc$var$rhos_654))
      red = raster(red1)
      proj4string(red)  = crs(nc_atts$proj4_string)
      extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      ##nir
      nir1 = t(ncvar_get(nc, nc$var$rhos_865))
      nir = raster(nir1)
      proj4string(nir)  = crs(nc_atts$proj4_string)
      extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the swir bands because I think this is what aasg wants
      #swir1
      swir11 = t(ncvar_get(nc, nc$var$rhos_1608))
      swir1 = raster(swir11)
      proj4string(swir1)  = crs(nc_atts$proj4_string)
      extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      swir21 = t(ncvar_get(nc, nc$var$rhos_2201))
      swir2 = raster(swir21)
      proj4string(swir2)  = crs(nc_atts$proj4_string)
      extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project land to same crs and extent as other layers
      land = projectRaster(land, blue, method="bilinear")
      
      #same process with cloud is followed for neg mask
      #switched nagetive mask to opporite so that it can work with inverse land mask, now all negative pixels will be 1
      neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0 | swir11<0 | swir21<0 , NA, 1)#set to NA if any are negative
      
      #makeroom
      rm(blue1,green1,red1,nir1,swir11,swir21)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      neg.mask1 = blue
      neg.mask1[] = neg.mask
      
      #create mask
      rmaskrs = neg.mask1*land
      plot(rmaskrs)
      
      #makeroom
      rm(neg.mask,neg.mask1,land)
      
      ##createndvilayer
      ndvi = (nir-red)/(nir+red)
      
      ##mangrove index (cmri(file:///C:/Users/cormi/Downloads/JARS-190642_online.pdf)
      cmri = ((nir-red)/(nir+red))-((nir-green)/(nir+green))
      
      #mangrove vegetation index (mvi https://www.sciencedirect.com/science/article/pii/S0924271620301519?casa_token=ObaJW3vJD3cAAAAA:yPTdiC0JGAx7YRCLuC_OIDEhZx39S86ec-dEhvZzkmbtkbxe51DUOH4aHU51p-_MdTmRpEkh)
      #mvi=((nir-green)/(swir1-green))
      #tried using this but just got a blank raster...
      
      #create pca
      pca.stack=stack(blue,green,red,nir,swir1,swir2,ndvi,cmri)
      pca.stack = mask(x=pca.stack, mask = rmaskrs)
      pca.dat = rasterPCA(pca.stack[[1:5]], spca=F)
      pca.dat = pca.dat$map
      
      #stack all layers together
      dat.stack = stack(blue, green, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","swir1","ndvi", "cmri", "pca1")
      
      rm(blue,green,red,nir,swir1,ndvi,cmri,pca.dat)
      
      ##cloud mask written by kristen
      #create map for cloud mask of layer 1373, because now the other data is cropped and this is not
      
      #cloud = t(ncvar_get(nc, nc$var$rhot_1373))
      #complete threshold calc first with the matrix
      #cloud = ifelse(cloud<="thresh.cloud",1,NA)
      #then change into raster
      #cloud = raster(cloud)
      #proj4string(cloud)  = crs(nc_atts$proj4_string)
      #extent(cloud) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      #cloud = crop(cloud, depth)
      #change cloud back into matrix to be used in calculations
      #cloud = as.matrix(cloud)
      #cloud mask finalized
      
      #to create reverse land mask
      dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
     
      
      }
}
