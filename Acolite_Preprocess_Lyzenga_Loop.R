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


###pre-process_Lyzenga

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
  
  #to run the april 2017 image only ( or any specific image) skip the for code above and then start here
  #but make sure to load packages, check the number of the image by looking at sat.images.list2
  #ii=28
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
      write.data = paste("D:/preprocessoutput", "/", sat.images.list2[ii], ".tif", sep="")
  
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created watermask
      water = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/watermaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
  
      blue1 = t(ncvar_get(nc, nc$var$rhos_486))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
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
      #nir1 = t(ncvar_get(nc, nc$var$rhos_839))
      #nir = raster(nir1)
      #proj4string(nir)  = crs(nc_atts$proj4_string)
      #extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      #swir11 = t(ncvar_get(nc, nc$var$rhos_1678))
      #swir1 = raster(swir11)
      #proj4string(swir1)  = crs(nc_atts$proj4_string)
      #extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      #swir21 = t(ncvar_get(nc, nc$var$rhos_2217))
      #swir2 = raster(swir21)
      #proj4string(swir2)  = crs(nc_atts$proj4_string)
      #extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project depth to same crs and extent as other layers
      water = projectRaster(water, blue, method="bilinear")
      blue = mask(blue, water)
      green = mask(green, water)
      red = mask(red, water)
      
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
      #cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      
      #have changed the crop depth to crop the AOI instead, because the AOI covers the area that crop depth would anyways
      #this way no need to add extra layer of cropping and no need to change code yay!
      cropdepth =readOGR("C:/Users/cormi/Documents/ImageProcessing/bathymetry/AOI_crop2_Polygon.shp")
      cropdepth = spTransform(cropdepth, crs(depth))
      depth = mask(depth,cropdepth)

      
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = mask(blue, cropdepth)
      green = mask(green, cropdepth)
      red = mask(red, cropdepth)
      #nir = crop(nir, depth)
      #swir1 = crop(swir1, depth)
      #swir2 = crop(swir2, depth)
      
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red)
      
      ##makeroom
      rm(blue,green,red)
      
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red")
      
      
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
      
      #Kristen gave me a new more fool proof way to create negative mask:
      #create one layer which is just the minimum value out of all of the data stack layer
      #for each of the pixels of the dat stack
      neg.mask=min(dat.stack, na.rm=T)
      #use the negative mask to remove anypixels that are overlayed with negative pixels in the neg.mask
      dat.stack[neg.mask<0]=NA
      
      #continue with water mask as expected
      
      #makeroom
      rm(blue1,green1,red1)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      #neg.mask1 = dat.stack[[1]]
      #neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      
      ##remove deep water
      offshore=depth<=-5
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      #rm(cloud,neg.mask,cloud1,neg.mask1)
      rm(neg.mask,neg.mask1)
      
      
      #maskdat.stack and crop
      #preciously, I used the landmask to mask out land and multiplied it by the 
      #negtaive value mask created above and then ran inverse =T
      #however, in that case, it would also be masking the inverse of the negative value mask 
      #which is a mask that has all positive values listed as 1 and negative as NA
      #so it would be masking the inverse of that mask, so would be masking positive values
      #so now I changed the mask to a mask of water so that when it is multiplied by the neg mask
      #anything that is 1 in the neg. mask (which means it was not negative) is 
      #is multiplied by the water mask so that all values in the mask = positive values 
      #and water which is what we want to keep
      
      #do not worry about paragraph above, new negative mask code from Kristen removes this issue
      #land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
      
      
      #to create reverse water mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##createndvilayer
      #ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
      
      ##what is gndvi?
      #gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
      
      #create stack with ndvi and gndvi
      #dat.stack = stack(dat.stack,ndvi,gndvi)
      #rm(ndvi,gndvi)
      
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red")
 
      
  #Lyzenga WCC layers
      raster.dat<-dat.stack[[1:3]]
      names(raster.dat) = c("blue2", "green2", "red2")
      
      raster.name.dat= c("blue2","green2","red2")
      #only use blue green red (sets number of bands to use to 3)
      raster.dim.use = 3 #number of bands to use the WCC
      #shape.dat is called shape.in because I only have one shape file with all polygons
      shape.in = shapefile("C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/sandWaterColumnCorrection_Polygon.shp")
      shape.dat = "C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/"
      
      shape.in = spTransform(shape.in, raster.dat@crs)
      dat = extract(x=raster.dat,y=shape.in,df=T)
      dat = na.omit(dat)
      rm(shape.in)
      
      #Generate ln #
      dat.ln=dat
      dat.ln[,2:(1+length(raster.name.dat))] = log (dat[,2:(1+length(raster.name.dat))])#take natural logarithm
      #Calculate variance
      sub.dat.variance = apply(dat.ln, 2, var) #calculate variance 
      #Generate unique combinations
      val.in = expand.grid(names(sub.dat.variance[2]),names(sub.dat.variance[3:(1+raster.dim.use)]))
      i=2
      {while(i<raster.dim.use){
        i = i+1
        test = expand.grid(names(sub.dat.variance[i]),names(sub.dat.variance[(i+1):(1+raster.dim.use)]))
        val.in = rbind(val.in,test)
        rm(test)}}#generate unqiue DDI
      val.in = cbind(val.in,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1]))
                     ,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])))
      names(val.in) = c("B1","B2","Variance.B1","Variance.B2", "Covariance","a","slope","R2")
      val.in[,1] = as.character(val.in[,1])
      val.in[,2] = as.character(val.in[,2])
      rm(i)
      #Calculate covariance
      for ( i in 1:length(val.in[,1])){
        val.in$Covariance[i] = cov(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]])
      }
      #Put variance into val.in
      i=1
      {while(i<raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B1,sub.dat.variance[i],NA)
        val.in$Variance.B1 = ifelse(is.na(test)==T,val.in$Variance.B1,test)
      }}
      i=1
      {while(i<=raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B2,sub.dat.variance[i],NA)
        val.in$Variance.B2 = ifelse(is.na(test)==T,val.in$Variance.B2,test)
        rm(test)
      }}
      #calculate a
      for ( i in 1:length(val.in[,1])){
        val.in$a[i] = (val.in$Variance.B1[i]-val.in$Variance.B2[i])/(2*val.in$Covariance[i]) 
      }
      #calculate the slope
      for ( i in 1:length(val.in[,1])){
        val.in$slope[i] = val.in$a[i] + sqrt((val.in$a[i]^2)+1)
      }
      #Plot each bi-plot
      for ( i in 1:length(val.in[,1])){
        png(paste(shape.dat,"biplot-",val.in[i,1],val.in[i,2],".png",sep=""),pointsize=10,family="serif")
        plot(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]], ylab = paste ("ln ", val.in[i,2],sep=""),
             xlab = paste ("ln ", val.in[i,1],sep=""),pch=19, col=dat.ln[,1])
        test = lm(dat.ln[,val.in[i,2]]~ dat.ln[,val.in[i,1]]) 
        abline(a=  test$coefficients[1],b=test$coefficients[2])
        dev.off()
        val.in$R2[i] = summary(test)$r.squared
        rm(test)}
      #Write out the coefficients
      write.csv(val.in,paste(shape.dat,"WCC-Coef",".csv",sep="") )
      ##
      rm(dat,dat.ln,sub.dat.variance,i)
      
      #Apply the  water column correction to the visible bands
      ras.dat = raster.dat
      ras.dat = log(ras.dat)
      names(ras.dat) = raster.name.dat
      for (i in 1:length(val.in[,1])){
        print(i)
        raster.dat[[i]] = ras.dat[[val.in[i,1]]]-(val.in$slope[i]*ras.dat[[val.in[i,2]]])
      }
      
      #add WCC layers to raster stack
      dat.stack = stack(dat.stack,raster.dat[[3]],raster.dat[[1]]) 
      
      names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl")
      
      #create pca
      pca.stack=dat.stack
      pca.dat = rasterPCA(pca.stack[[1:3]], spca=F)
      pca.dat = pca.dat$map
      
      dat.stack = stack(dat.stack,pca.dat[[1]]) 
      
      #crop depth
      depth<-mask(x=depth, mask=offshoremask, inverse=T)
      depth = mask(x=depth, mask = water)
      
      #try with depth
      dat.stack = stack(dat.stack, depth)
      
      
      names(dat.stack)=c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
      
       rm(depth, water, offshore, dat.stack, swir11, swir21, cropdepth, offshoremask, pca.dat, pca.stack, ras.dat, raster.dat, val.in          , raster.dim.use, raster.name.dat, shape.dat, write.data)
      
} else if (nc_atts$sensor == "L7_ETM"){
      
       out.folder = "D:/acoliteoutput"
       sat.images.list2 = (list.files(out.folder))
       write.data = paste("D:/preprocessoutput", "/", sat.images.list2[ii], ".tif", sep="")
         
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created watermask
      water = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/watermaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      
      blue1 = t(ncvar_get(nc, nc$var$rhos_479))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
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
      #nir1 = t(ncvar_get(nc, nc$var$rhos_839))
      #nir = raster(nir1)
      #proj4string(nir)  = crs(nc_atts$proj4_string)
      #extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      #swir11 = t(ncvar_get(nc, nc$var$rhos_1678))
      #swir1 = raster(swir11)
      #proj4string(swir1)  = crs(nc_atts$proj4_string)
      #extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      #swir21 = t(ncvar_get(nc, nc$var$rhos_2217))
      #swir2 = raster(swir21)
      #proj4string(swir2)  = crs(nc_atts$proj4_string)
      #extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project depth to same crs and extent as other layers
      water = projectRaster(water, blue, method="bilinear")
      blue = mask(blue, water)
      green = mask(green, water)
      red = mask(red, water)
      
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
      #cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      
      #have changed the crop depth to crop the AOI instead, because the AOI covers the area that crop depth would anyways
      #this way no need to add extra layer of cropping and no need to change code yay!
      cropdepth =readOGR("C:/Users/cormi/Documents/ImageProcessing/bathymetry/AOI_crop2_Polygon.shp")
      cropdepth = spTransform(cropdepth, crs(depth))
      depth = mask(depth,cropdepth)
      
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = mask(blue, cropdepth)
      green = mask(green, cropdepth)
      red = mask(red, cropdepth)
      #nir = crop(nir, depth)
      #swir1 = crop(swir1, depth)
      #swir2 = crop(swir2, depth)
      
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red)
      
      ##makeroom
      rm(blue,green,red,nir,swir1, swir2)
      
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red")
      
      
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
      
      #Kristen gave me a new more fool proof way to create negative mask:
      #create one layer which is just the minimum value out of all of the data stack layer
      #for each of the pixels of the dat stack
      neg.mask=min(dat.stack, na.rm=T)
      #use the negative mask to remove anypixels that are overlayed with negative pixels in the neg.mask
      dat.stack[neg.mask<0]=NA
      
      #continue with water mask as expected
      
      #makeroom
      rm(blue1,green1,red1)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      #neg.mask1 = dat.stack[[1]]
      #neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      #landmask
      #project depth to same crs and extent as other layers
  
      
      ##remove deep water
      offshore=depth<=-5
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      #rm(cloud,neg.mask,cloud1,neg.mask1)
      rm(neg.mask,neg.mask1)
      
      
      #maskdat.stack and crop
      #preciously, I used the landmask to mask out land and multiplied it by the 
      #negtaive value mask created above and then ran inverse =T
      #however, in that case, it would also be masking the inverse of the negative value mask 
      #which is a mask that has all positive values listed as 1 and negative as NA
      #so it would be masking the inverse of that mask, so would be masking positive values
      #so now I changed the mask to a mask of water so that when it is multiplied by the neg mask
      #anything that is 1 in the neg. mask (which means it was not negative) is 
      #is multiplied by the water mask so that all values in the mask = positive values 
      #and water which is what we want to keep
      
      #do not worry about paragraph above, new negative mask code from Kristen removes this issue
     #land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
      
      
      #to create reverse water mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##createndvilayer
      #ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
      
      ##what is gndvi?
      #gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
      
      #create stack with ndvi and gndvi
      #dat.stack = stack(dat.stack,ndvi,gndvi)
      #rm(ndvi,gndvi)
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red")
      
      
      #Lyzenga WCC layers
      raster.dat<-dat.stack[[1:3]]
      names(raster.dat) = c("blue2", "green2", "red2")
      
      raster.name.dat= c("blue2","green2","red2")
      #only use blue green red (sets number of bands to use to 3)
      raster.dim.use = 3 #number of bands to use the WCC
      #shape.dat is called shape.in because I only have one shape file with all polygons
      shape.in = shapefile("C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/sandWaterColumnCorrection_Polygon.shp")
      shape.dat = "C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/"
      
      shape.in = spTransform(shape.in, raster.dat@crs)
      dat = extract(x=raster.dat,y=shape.in,df=T)
      dat = na.omit(dat)
      rm(shape.in)
      
      #Generate ln #
      dat.ln=dat
      dat.ln[,2:(1+length(raster.name.dat))] = log (dat[,2:(1+length(raster.name.dat))])#take natural logarithm
      #Calculate variance
      sub.dat.variance = apply(dat.ln, 2, var) #calculate variance 
      #Generate unique combinations
      val.in = expand.grid(names(sub.dat.variance[2]),names(sub.dat.variance[3:(1+raster.dim.use)]))
      i=2
      {while(i<raster.dim.use){
        i = i+1
        test = expand.grid(names(sub.dat.variance[i]),names(sub.dat.variance[(i+1):(1+raster.dim.use)]))
        val.in = rbind(val.in,test)
        rm(test)}}#generate unqiue DDI
      val.in = cbind(val.in,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1]))
                     ,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])))
      names(val.in) = c("B1","B2","Variance.B1","Variance.B2", "Covariance","a","slope","R2")
      val.in[,1] = as.character(val.in[,1])
      val.in[,2] = as.character(val.in[,2])
      rm(i)
      #Calculate covariance
      for ( i in 1:length(val.in[,1])){
        val.in$Covariance[i] = cov(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]])
      }
      #Put variance into val.in
      i=1
      {while(i<raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B1,sub.dat.variance[i],NA)
        val.in$Variance.B1 = ifelse(is.na(test)==T,val.in$Variance.B1,test)
      }}
      i=1
      {while(i<=raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B2,sub.dat.variance[i],NA)
        val.in$Variance.B2 = ifelse(is.na(test)==T,val.in$Variance.B2,test)
        rm(test)
      }}
      #calculate a
      for ( i in 1:length(val.in[,1])){
        val.in$a[i] = (val.in$Variance.B1[i]-val.in$Variance.B2[i])/(2*val.in$Covariance[i]) 
      }
      #calculate the slope
      for ( i in 1:length(val.in[,1])){
        val.in$slope[i] = val.in$a[i] + sqrt((val.in$a[i]^2)+1)
      }
      #Plot each bi-plot
      for ( i in 1:length(val.in[,1])){
        png(paste(shape.dat,"biplot-",val.in[i,1],val.in[i,2],".png",sep=""),pointsize=10,family="serif")
        plot(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]], ylab = paste ("ln ", val.in[i,2],sep=""),
             xlab = paste ("ln ", val.in[i,1],sep=""),pch=19, col=dat.ln[,1])
        test = lm(dat.ln[,val.in[i,2]]~ dat.ln[,val.in[i,1]]) 
        abline(a=  test$coefficients[1],b=test$coefficients[2])
        dev.off()
        val.in$R2[i] = summary(test)$r.squared
        rm(test)}
      #Write out the coefficients
      write.csv(val.in,paste(shape.dat,"WCC-Coef",".csv",sep="") )
      ##
      rm(dat,dat.ln,sub.dat.variance,i)
      
      #Apply the  water column correction to the visible bands
      ras.dat = raster.dat
      ras.dat = log(ras.dat)
      names(ras.dat) = raster.name.dat
      for (i in 1:length(val.in[,1])){
        print(i)
        raster.dat[[i]] = ras.dat[[val.in[i,1]]]-(val.in$slope[i]*ras.dat[[val.in[i,2]]])
      }
      
      #add WCC layers to raster stack
      dat.stack = stack(dat.stack,raster.dat[[3]],raster.dat[[1]]) 
      
      names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl")
      
      #create pca
      pca.stack=dat.stack
      pca.dat = rasterPCA(pca.stack[[1:3]], spca=F)
      pca.dat = pca.dat$map
      
      dat.stack = stack(dat.stack,pca.dat[[1]])
      
      #crop depth
      depth<-mask(x=depth, mask=offshoremask, inverse=T)
      depth = mask(x=depth, mask = water)
      
      #try with depth
      dat.stack = stack(dat.stack, depth)
      
      names(dat.stack)=c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
      
       rm(depth, water, offshore, dat.stack, swir11, swir21, cropdepth, offshoremask, pca.dat, pca.stack, ras.dat, raster.dat, val.in          , raster.dim.use, raster.name.dat, shape.dat, write.data)
  
} else if (nc_atts$sensor == "L8_OLI") {  
    
      out.folder = "D:/acoliteoutput"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/preprocessoutput", "/", sat.images.list2[ii], ".tif", sep="")
      
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created watermask
      water = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/watermaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      
      blue1 = t(ncvar_get(nc, nc$var$rhos_483))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
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
      #nir1 = t(ncvar_get(nc, nc$var$rhos_839))
      #nir = raster(nir1)
      #proj4string(nir)  = crs(nc_atts$proj4_string)
      #extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      #swir11 = t(ncvar_get(nc, nc$var$rhos_1678))
      #swir1 = raster(swir11)
      #proj4string(swir1)  = crs(nc_atts$proj4_string)
      #extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      #swir21 = t(ncvar_get(nc, nc$var$rhos_2217))
      #swir2 = raster(swir21)
      #proj4string(swir2)  = crs(nc_atts$proj4_string)
      #extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project depth to same crs and extent as other layers
      water = projectRaster(water, blue, method="bilinear")
      blue = mask(blue, water)
      green = mask(green, water)
      red = mask(red, water)
      
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
      #cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      
      #have changed the crop depth to crop the AOI instead, because the AOI covers the area that crop depth would anyways
      #this way no need to add extra layer of cropping and no need to change code yay!
      cropdepth =readOGR("C:/Users/cormi/Documents/ImageProcessing/bathymetry/AOI_crop2_Polygon.shp")
      cropdepth = spTransform(cropdepth, crs(depth))
      depth = mask(depth,cropdepth)
      
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = mask(blue, cropdepth)
      green = mask(green, cropdepth)
      red = mask(red, cropdepth)
      #nir = crop(nir, depth)
      #swir1 = crop(swir1, depth)
      #swir2 = crop(swir2, depth)
      
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red)
      
      ##makeroom
      rm(blue,green,red,nir,swir1, swir2)
      
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red")
      
      
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
      
      #Kristen gave me a new more fool proof way to create negative mask:
      #create one layer which is just the minimum value out of all of the data stack layer
      #for each of the pixels of the dat stack
      neg.mask=min(dat.stack, na.rm=T)
      #use the negative mask to remove anypixels that are overlayed with negative pixels in the neg.mask
      dat.stack[neg.mask<0]=NA
      
      #continue with water mask as expected
      
      #makeroom
      rm(blue1,green1,red1)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      #neg.mask1 = dat.stack[[1]]
      #neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      #landmask
      #project depth to same crs and extent as other layers
 
      ##remove deep water
      offshore=depth<=-5
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      #rm(cloud,neg.mask,cloud1,neg.mask1)
      rm(neg.mask,neg.mask1)
      
      
      #maskdat.stack and crop
      #preciously, I used the landmask to mask out land and multiplied it by the 
      #negtaive value mask created above and then ran inverse =T
      #however, in that case, it would also be masking the inverse of the negative value mask 
      #which is a mask that has all positive values listed as 1 and negative as NA
      #so it would be masking the inverse of that mask, so would be masking positive values
      #so now I changed the mask to a mask of water so that when it is multiplied by the neg mask
      #anything that is 1 in the neg. mask (which means it was not negative) is 
      #is multiplied by the water mask so that all values in the mask = positive values 
      #and water which is what we want to keep
      
      #do not worry about paragraph above, new negative mask code from Kristen removes this issue
      #land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
      
      
      #to create reverse water mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##createndvilayer
      #ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
      
      ##what is gndvi?
      #gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
      
      #create stack with ndvi and gndvi
      #dat.stack = stack(dat.stack,ndvi,gndvi)
      #rm(ndvi,gndvi)
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red")
      
      
      #Lyzenga WCC layers
      raster.dat<-dat.stack[[1:3]]
      names(raster.dat) = c("blue2", "green2", "red2")
      
      raster.name.dat= c("blue2","green2","red2")
      #only use blue green red (sets number of bands to use to 3)
      raster.dim.use = 3 #number of bands to use the WCC
      #shape.dat is called shape.in because I only have one shape file with all polygons
      shape.in = shapefile("C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/sandWaterColumnCorrection_Polygon.shp")
      shape.dat = "C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/"
      
      shape.in = spTransform(shape.in, raster.dat@crs)
      dat = extract(x=raster.dat,y=shape.in,df=T)
      dat = na.omit(dat)
      rm(shape.in)
      
      #Generate ln #
      dat.ln=dat
      dat.ln[,2:(1+length(raster.name.dat))] = log (dat[,2:(1+length(raster.name.dat))])#take natural logarithm
      #Calculate variance
      sub.dat.variance = apply(dat.ln, 2, var) #calculate variance 
      #Generate unique combinations
      val.in = expand.grid(names(sub.dat.variance[2]),names(sub.dat.variance[3:(1+raster.dim.use)]))
      i=2
      {while(i<raster.dim.use){
        i = i+1
        test = expand.grid(names(sub.dat.variance[i]),names(sub.dat.variance[(i+1):(1+raster.dim.use)]))
        val.in = rbind(val.in,test)
        rm(test)}}#generate unqiue DDI
      val.in = cbind(val.in,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1]))
                     ,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])))
      names(val.in) = c("B1","B2","Variance.B1","Variance.B2", "Covariance","a","slope","R2")
      val.in[,1] = as.character(val.in[,1])
      val.in[,2] = as.character(val.in[,2])
      rm(i)
      #Calculate covariance
      for ( i in 1:length(val.in[,1])){
        val.in$Covariance[i] = cov(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]])
      }
      #Put variance into val.in
      i=1
      {while(i<raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B1,sub.dat.variance[i],NA)
        val.in$Variance.B1 = ifelse(is.na(test)==T,val.in$Variance.B1,test)
      }}
      i=1
      {while(i<=raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B2,sub.dat.variance[i],NA)
        val.in$Variance.B2 = ifelse(is.na(test)==T,val.in$Variance.B2,test)
        rm(test)
      }}
      #calculate a
      for ( i in 1:length(val.in[,1])){
        val.in$a[i] = (val.in$Variance.B1[i]-val.in$Variance.B2[i])/(2*val.in$Covariance[i]) 
      }
      #calculate the slope
      for ( i in 1:length(val.in[,1])){
        val.in$slope[i] = val.in$a[i] + sqrt((val.in$a[i]^2)+1)
      }
      #Plot each bi-plot
      for ( i in 1:length(val.in[,1])){
        png(paste(shape.dat,"biplot-",val.in[i,1],val.in[i,2],".png",sep=""),pointsize=10,family="serif")
        plot(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]], ylab = paste ("ln ", val.in[i,2],sep=""),
             xlab = paste ("ln ", val.in[i,1],sep=""),pch=19, col=dat.ln[,1])
        test = lm(dat.ln[,val.in[i,2]]~ dat.ln[,val.in[i,1]]) 
        abline(a=  test$coefficients[1],b=test$coefficients[2])
        dev.off()
        val.in$R2[i] = summary(test)$r.squared
        rm(test)}
      #Write out the coefficients
      write.csv(val.in,paste(shape.dat,"WCC-Coef",".csv",sep="") )
      ##
      rm(dat,dat.ln,sub.dat.variance,i)
      
      #Apply the  water column correction to the visible bands
      ras.dat = raster.dat
      ras.dat = log(ras.dat)
      names(ras.dat) = raster.name.dat
      for (i in 1:length(val.in[,1])){
        print(i)
        raster.dat[[i]] = ras.dat[[val.in[i,1]]]-(val.in$slope[i]*ras.dat[[val.in[i,2]]])
      }
      
      #add WCC layers to raster stack
      dat.stack = stack(dat.stack,raster.dat[[3]],raster.dat[[1]]) 
      
      names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl")
      
      #create pca
      pca.stack=dat.stack
      pca.dat = rasterPCA(pca.stack[[1:3]], spca=F)
      pca.dat = pca.dat$map
      
      dat.stack = stack(dat.stack,pca.dat[[1]]) 
      
      #crop depth
      depth<-mask(x=depth, mask=offshoremask, inverse=T)
      depth = mask(x=depth, mask = water)
      
      #try with depth
      dat.stack = stack(dat.stack, depth)
      
      names(dat.stack)=c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
      
       rm(depth, water, offshore, dat.stack, swir11, swir21, cropdepth, offshoremask, pca.dat, pca.stack, ras.dat, raster.dat, val.in          , raster.dim.use, raster.name.dat, shape.dat, write.data)
      
} else {
      
      out.folder = "D:/acoliteoutput"
      sat.images.list2 = (list.files(out.folder))
      write.data = paste("D:/preprocessoutput", "/", sat.images.list2[ii], ".tif", sep="")
      
      #import depth raster from NOAA
      depth = raster("C:/Users/cormi/Documents/ImageProcessing/bathymetry/Bathymetry_Bimini_NOAA.tif")
      #import created watermask
      water = raster("C:/Users/cormi/Documents/ImageProcessing/landmask/watermaskBimini.tif")
      
      nc.dat = paste(out.folder, "/", sat.images.list2[ii], sep = "")
      nc = nc_open(nc.dat)
      nc_atts <- ncatt_get(nc, 0)
      
      blue1 = t(ncvar_get(nc, nc$var$rhos_482))
      #blue1 = t(ncvar_get(nc, nc$var$sza))
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
      #nir1 = t(ncvar_get(nc, nc$var$rhos_839))
      #nir = raster(nir1)
      #proj4string(nir)  = crs(nc_atts$proj4_string)
      #extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #added in the 2 swir bands because I think this is what aasg wants
      #swir1
      #swir11 = t(ncvar_get(nc, nc$var$rhos_1678))
      #swir1 = raster(swir11)
      #proj4string(swir1)  = crs(nc_atts$proj4_string)
      #extent(swir1) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #swir2
      #swir21 = t(ncvar_get(nc, nc$var$rhos_2217))
      #swir2 = raster(swir21)
      #proj4string(swir2)  = crs(nc_atts$proj4_string)
      #extent(swir2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
      
      #landmask
      #project depth to same crs and extent as other layers
      water = projectRaster(water, blue, method="bilinear")
      blue = mask(blue, water)
      green = mask(green, water)
      red = mask(red, water)
      
      #Depth Data 
      #project depth to same crs as blue
      depth = projectRaster(depth, blue, method="bilinear")
      ##change all NA to -30 to get rid of NAs in depth file
      depth[is.na(depth[])] = -0.1
      #cropdepth = st_read("C:/Users/cormi/Documents/ImageProcessing/landmask/cropdepth_Polygon.shp")
      
      #have changed the crop depth to crop the AOI instead, because the AOI covers the area that crop depth would anyways
      #this way no need to add extra layer of cropping and no need to change code yay!
      cropdepth =readOGR("C:/Users/cormi/Documents/ImageProcessing/bathymetry/AOI_crop2_Polygon.shp")
      cropdepth = spTransform(cropdepth, crs(depth))
      depth = mask(depth,cropdepth)
      
      #had to crop layers in order for the layers to match up now because I had to crop depth to remove NAs
      blue = mask(blue, cropdepth)
      green = mask(green, cropdepth)
      red = mask(red, cropdepth)
      #nir = crop(nir, depth)
      #swir1 = crop(swir1, depth)
      #swir2 = crop(swir2, depth)
      
      ##stackalllayerstogether
      dat.stack = stack(blue,green,red)
      
      ##makeroom
      rm(blue,green,red,nir,swir1, swir2)
      
      ##name the layers in data stack
      names(dat.stack) = c("blue","green","red")
      
      
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
      
      #Kristen gave me a new more fool proof way to create negative mask:
      #create one layer which is just the minimum value out of all of the data stack layer
      #for each of the pixels of the dat stack
      neg.mask=min(dat.stack, na.rm=T)
      #use the negative mask to remove anypixels that are overlayed with negative pixels in the neg.mask
      dat.stack[neg.mask<0]=NA
      
      #continue with water mask as expected
      
      #makeroom
      rm(blue1,green1,red1)
      
      ##what do the empty square brackets do?
      #cloud1 = dat.stack[[1]]
      #cloud1[] = cloud
      #neg.mask1 = dat.stack[[1]]
      #neg.mask1[] = neg.mask
      
      ##create final mask
      #there witll be a warning about extents not overlapping, but this is fine
      #landmask
      #project depth to same crs and extent as other layers
      
      ##remove deep water
      offshore=depth<=-5
      
      #remove 0 values
      offshoremask <- clamp(offshore, lower=1, useValues=FALSE)
      
      #makeroom
      #rm(cloud,neg.mask,cloud1,neg.mask1)
      rm(neg.mask,neg.mask1)
      
      
      #maskdat.stack and crop
      #preciously, I used the landmask to mask out land and multiplied it by the 
      #negtaive value mask created above and then ran inverse =T
      #however, in that case, it would also be masking the inverse of the negative value mask 
      #which is a mask that has all positive values listed as 1 and negative as NA
      #so it would be masking the inverse of that mask, so would be masking positive values
      #so now I changed the mask to a mask of water so that when it is multiplied by the neg mask
      #anything that is 1 in the neg. mask (which means it was not negative) is 
      #is multiplied by the water mask so that all values in the mask = positive values 
      #and water which is what we want to keep
      
      #do not worry about paragraph above, new negative mask code from Kristen removes this issue
      #land, cloud, neg mask
      #mask dat.stack with offshore
      dat.stack = mask(x=dat.stack, mask = offshoremask, inverse=T)
      
      
      #to create reverse water mask
      #dat.stack = mask(x=dat.stack, mask = rmaskrs)
      
      ##
      nc_close(nc)
      #makeroom
      rm(nc,nc.dat,nc_atts,rmaskrs)
      
      ##createndvilayer
      #ndvi = (dat.stack$nir-dat.stack$red)/(dat.stack$nir+dat.stack$red)
      
      ##what is gndvi?
      #gndvi = (dat.stack$nir-dat.stack$green)/(dat.stack$nir+dat.stack$green)
      
      #create stack with ndvi and gndvi
      #dat.stack = stack(dat.stack,ndvi,gndvi)
      #rm(ndvi,gndvi)
      
      ##name layers in dat stack
      names(dat.stack) = c("blue","green","red")
      
      
      #Lyzenga WCC layers
      raster.dat<-dat.stack[[1:3]]
      names(raster.dat) = c("blue2", "green2", "red2")
      
      raster.name.dat= c("blue2","green2","red2")
      #only use blue green red (sets number of bands to use to 3)
      raster.dim.use = 3 #number of bands to use the WCC
      #shape.dat is called shape.in because I only have one shape file with all polygons
      shape.in = shapefile("C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/sandWaterColumnCorrection_Polygon.shp")
      shape.dat = "C:/Users/cormi/Documents/ImageProcessing/Water Column Correction/"
      
      shape.in = spTransform(shape.in, raster.dat@crs)
      dat = extract(x=raster.dat,y=shape.in,df=T)
      dat = na.omit(dat)
      rm(shape.in)
      
      #Generate ln #
      dat.ln=dat
      dat.ln[,2:(1+length(raster.name.dat))] = log (dat[,2:(1+length(raster.name.dat))])#take natural logarithm
      #Calculate variance
      sub.dat.variance = apply(dat.ln, 2, var) #calculate variance 
      #Generate unique combinations
      val.in = expand.grid(names(sub.dat.variance[2]),names(sub.dat.variance[3:(1+raster.dim.use)]))
      i=2
      {while(i<raster.dim.use){
        i = i+1
        test = expand.grid(names(sub.dat.variance[i]),names(sub.dat.variance[(i+1):(1+raster.dim.use)]))
        val.in = rbind(val.in,test)
        rm(test)}}#generate unqiue DDI
      val.in = cbind(val.in,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1]))
                     ,rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])),rep(NA,length(val.in[,1])))
      names(val.in) = c("B1","B2","Variance.B1","Variance.B2", "Covariance","a","slope","R2")
      val.in[,1] = as.character(val.in[,1])
      val.in[,2] = as.character(val.in[,2])
      rm(i)
      #Calculate covariance
      for ( i in 1:length(val.in[,1])){
        val.in$Covariance[i] = cov(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]])
      }
      #Put variance into val.in
      i=1
      {while(i<raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B1,sub.dat.variance[i],NA)
        val.in$Variance.B1 = ifelse(is.na(test)==T,val.in$Variance.B1,test)
      }}
      i=1
      {while(i<=raster.dim.use){
        i = i+1
        test = ifelse(names(sub.dat.variance[i])==val.in$B2,sub.dat.variance[i],NA)
        val.in$Variance.B2 = ifelse(is.na(test)==T,val.in$Variance.B2,test)
        rm(test)
      }}
      #calculate a
      for ( i in 1:length(val.in[,1])){
        val.in$a[i] = (val.in$Variance.B1[i]-val.in$Variance.B2[i])/(2*val.in$Covariance[i]) 
      }
      #calculate the slope
      for ( i in 1:length(val.in[,1])){
        val.in$slope[i] = val.in$a[i] + sqrt((val.in$a[i]^2)+1)
      }
      #Plot each bi-plot
      for ( i in 1:length(val.in[,1])){
        png(paste(shape.dat,"biplot-",val.in[i,1],val.in[i,2],".png",sep=""),pointsize=10,family="serif")
        plot(dat.ln[,val.in[i,1]], dat.ln[,val.in[i,2]], ylab = paste ("ln ", val.in[i,2],sep=""),
             xlab = paste ("ln ", val.in[i,1],sep=""),pch=19, col=dat.ln[,1])
        test = lm(dat.ln[,val.in[i,2]]~ dat.ln[,val.in[i,1]]) 
        abline(a=  test$coefficients[1],b=test$coefficients[2])
        dev.off()
        val.in$R2[i] = summary(test)$r.squared
        rm(test)}
      #Write out the coefficients
      write.csv(val.in,paste(shape.dat,"WCC-Coef",".csv",sep="") )
      ##
      rm(dat,dat.ln,sub.dat.variance,i)
      
      #Apply the  water column correction to the visible bands
      ras.dat = raster.dat
      ras.dat = log(ras.dat)
      names(ras.dat) = raster.name.dat
      for (i in 1:length(val.in[,1])){
        print(i)
        raster.dat[[i]] = ras.dat[[val.in[i,1]]]-(val.in$slope[i]*ras.dat[[val.in[i,2]]])
      }
      
      #add WCC layers to raster stack
      dat.stack = stack(dat.stack,raster.dat[[3]],raster.dat[[1]]) 
      
      names(dat.stack) = c("blue","green","red","lyzgr","lyzgrbl")
      
      #create pca
      pca.stack=dat.stack
      pca.dat = rasterPCA(pca.stack[[1:3]], spca=F)
      pca.dat = pca.dat$map
      
      dat.stack = stack(dat.stack,pca.dat[[1]]) 
      
      #crop depth
      depth<-mask(x=depth, mask=offshoremask, inverse=T)
      depth = mask(x=depth, mask = water)
      
      #try with depth
      dat.stack = stack(dat.stack, depth)
      
      names(dat.stack)=c("blue","green","red","lyzgr","lyzgrbl","pca1", "depth")
      
      ##make the final raster!
      writeRaster(dat.stack, write.data, format="GTiff",NAflag = NaN, overwrite=T)
      
      rm(depth, water, offshore, dat.stack, swir11, swir21, cropdepth, offshoremask, pca.dat, pca.stack, ras.dat, raster.dat, val.in
         , raster.dim.use, raster.name.dat, shape.dat, write.data)
      }
}

   
