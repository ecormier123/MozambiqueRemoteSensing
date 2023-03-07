rm(list=ls())
gc()

##
folder = "D:/AllImagesUsed"
#the list of satellite images that are in your folder (just their L1 product identifier)
sat.data = read.csv("D:/AllImagesUsed/SatelliteImagesDownloaded_AllImagesUsed.csv")
#turns the csv file of images and other details into just a list of satellite names
sat.images.list = sat.data$Landsat.Product.Identifier.L1
#this was another optiojn I tried, but I couldn't figure out how to get it to work
#sat.images.list = (list.dirs(folder))
#sat.images.list  = sat.images.list[-1]

rm(sat.data)

#this starts a loop which runs through each satellite in the folder specified above
#based on the list of satellite images from sat.images.list, everything within the {}
# will be run

for (ii in 1:length(sat.images.list))
{
  #this code gives you updates of which image is being processed, the text in quotes can be changed to whatever you like
  print(paste("Running ", ii, " of ", length(sat.images.list), " in satellite images list", sep = ""))
  
  #this changes the in.folder for ewach lop to match to whichever satellite image you are working on
  in.folder = paste(folder,"/", sat.images.list[ii], sep = "")
  out.folder = "D:/acoliteoutput"
  #change the date to match today's date
  date<-paste(format(Sys.time(), "acolite_settings_%Y%m%d_%I%p."),"txt", sep = "")
  #changes the name of the out.settings to be the name of the satellite image being processed and the date
  out.settings = paste (sat.images.list[ii], date, sep="")
  ##
  
  #Read in settings file (this is blank file which you fill in with the info below)
  set.file = read.delim("C:/Users/cormi/Documents/acolite_py_win/config/landsat.txt",header=F)#Acolite default
  #define input file (sets input file to the image [ii])
  set.file[1,1] = paste("inputfile=",in.folder,sep="")
  #define output file (sets where the output image will be put)
  set.file[2,1] = paste("output=", out.folder,sep="")
  #crops the image to the coordinates that you want, in this case for Bimini it had to be cropped a lot
  #this decreases processing time
  set.file[3,1] = "limit=25.663219,-79.324705,25.801764,-79.198116"
  # a very important fix for the BImini image because it is a very small image and acolite overfits dsf 
  #if it is tiled instead of fixed and near-infrared values become negative
  set.file[4,1] = "dsf_aot_estimate=fixed"
  #the last few lines just remove extra files thta we don't need for the loop and allow the next step of processes in
  #the preprocessing file to run smoothly
  set.file[5,1] = "l1r_delete_netcdf=True"
  set.file[6,1] = "l2r_pans_delete_netcdf=True"
  set.file[7,1] = "delete_acolite_run_text_files=True" 
  set.file[8,1] = "rgb_rhos=False" 
  set.file[9,1] = "rgb_rhot=False"
  #set.file[5,1] = l2w_mask_threshold=0.035
  #set.file[6,1] = l2w_mask_high_toa_threshold=0.31
  #do a sunglint correction - seems this is all already in the file I have so no need to run code
  #set.file[33,1] = "glint_mask_rhos_band=1600"#change mask band
  #set.file[34,1] = "glint_mask_rhos_threshold=0.05"#change mask default of 0.05
  #set.file[35,1] = "glint_write_rhog_ref=False"
  #set.file[37,1] = "glint_write_rhog_all=False"
  #write l2w map
  #set.file[82,1] =  "map_l2w=True"
  
  #to fix error "ValueError: The requested sample points xi have dimension 6, but this RegularGridInterpolator has dimension 5" based on website help 
  #from https://odnature.naturalsciences.be/remsem/acolite-forum/viewtopic.php?t=321
  #set.file[98,1]="dsf_interface_reflectance=True"
  
  #write out the settings file
  
  write.table(set.file, paste(folder,"/",out.settings,sep=""),row.names=F,col.names=F,quote=F)
  
  
  #Acolite call, have to provide the full paths for it to work
  cmd = paste("C:/Users/cormi/Documents/acolite_py_win/dist/acolite/acolite.exe --cli --settings=",folder,"/",out.settings,sep="")
  shell(cmd)
  
  
}

rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
library("sf")
library("RStoolbox")

out.folder = "D:/acoliteoutput"
sat.images.list2 = (list.files(out.folder))
#sat.images.list2  = sat.images.list2[-1]

for (ii in 1:length(sat.images.list2)) {
  
  out.folder = "D:/acoliteoutput"
  sat.images.list2 = (list.files(out.folder))
  #sat.images.list2  = sat.images.list2[-1]

  print(paste("Running ", ii, " of ", length(sat.images.list2), " in satellite images list", sep = ""))
  
  #import satellite data nc file
  nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
  nc = nc_open(nc.dat)
  nc_atts <- ncatt_get(nc, 0)
  ##
  
      if (nc_atts$sensor == "L5_TM"){       
  
      out.folder = "D:/acoliteoutput"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/preprocessoutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
      
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
      dat.stack = stack(blue, green, red, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red","swir1","ndvi", "cmri", "pca1")
      
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
            rm(nc,nc.dat,nc_atts,rmaskrs,dat.stack,pca.stack,swir2)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
   
    
} else if (nc_atts$sensor == "L7_ETM"){
      
       out.folder = "D:/acoliteoutput"
       sat.images.list2 = (list.files(out.folder))
       write.data = paste("D:/preprocessoutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
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
      dat.stack = stack(blue, green, red, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green", "red","swir1","ndvi", "cmri", "pca1")
      
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
            rm(nc,nc.dat,nc_atts,rmaskrs,dat.stack,pca.stack,swir2)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
   
} else if (nc_atts$sensor == "L8_OLI") {  
    
      out.folder = "D:/acoliteoutput"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/preprocessoutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
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
      dat.stack = stack(blue, green, red, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red","swir1","ndvi", "cmri", "pca1")
      
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
            rm(nc,nc.dat,nc_atts,rmaskrs,dat.stack,pca.stack,swir2)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
    
} else {
     
      out.folder = "D:/acoliteoutput"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/preprocessoutputmangrove", "/", sat.images.list2[ii], "mangrove.tif", sep="")
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
      dat.stack = stack(blue, green, red, swir1, ndvi, cmri, pca.dat[[1]])
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red", "swir1","ndvi", "cmri", "pca1")
      
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
      rm(nc,nc.dat,nc_atts,rmaskrs,dat.stack,pca.stack,swir2)
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
     
      
      }
}
