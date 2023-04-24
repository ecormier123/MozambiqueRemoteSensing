#install packages (might be slow)
install.packages("ncdf4")
install.packages("raster")
install.packages("rgdal")
install.packages("fields")
install.packages("snow")
install.packages("sf")
install.packages("RStoolbox")
install.packages("terra")


#load in all your packages
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("snow")
library("sf")
library("RStoolbox")
library("terra")


#first we are going to import the image from 1999, which is a landsat 5 file
#processed through acolite to remove atmospheric interference
      
        #import net cdf file (.nc file) by pasting the file path below
        #make sure to have "/" instead of "\" in your file path
        #make sue to have the landmask file downloaded and your two images
        nc.dat = ("D:/MarinePlantsPackage/L5_TM_1999_03_23_15_22_43_014042_L2R.nc")
        
        ##open your net cdf compressed file
        nc = nc_open(nc.dat)
        nc_atts <- ncatt_get(nc, 0)
        
        ##take out blue layer for extent matching and coordinate refrence systsem (crs) matching
        blue1 = t(ncvar_get(nc, nc$var$rhos_486))
        blue = raster(blue1)
        ##changing projection (crs = coordinate reference system) with project4string
        proj4string(blue)  = crs(nc_atts$proj4_string)
        ##clipping the blue to nc atts extent
        extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        #plot your blue layer, this shows the amount of blue light reflected by the image
        plot(blue)
        
        ##green
        ###get rest of bands to create final image
        green1 = t(ncvar_get(nc, nc$var$rhos_571))
        green = raster(green1)
        proj4string(green)  = crs(nc_atts$proj4_string)
        extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        ##red
        red1 = t(ncvar_get(nc, nc$var$rhos_660))
        red = raster(red1)
        proj4string(red)  = crs(nc_atts$proj4_string)
        extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        ##nir (near infrared)
        nir1 = t(ncvar_get(nc, nc$var$rhos_839))
        nir = raster(nir1)
        proj4string(nir)  = crs(nc_atts$proj4_string)
        extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        #stack your layers to make your raster file
        dat.stack = stack(blue, green, red, nir)
        #name your layers
        names(dat.stack) = c("blue", "green", "red", "near-infrared")
        
        plot(dat.stack)
        
        #since we want to look at only land, we will make a land mask
        #load in your land mask tif file
        land = raster("D:/MarinePlantsPackage/landmaskBimini.tif")
        
        #project land to same coordinate reference system and extent as other layers
        land = projectRaster(land, blue, method="bilinear")
        
        #create a mask to cover all negative pixels
        neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0, NA, 1)#set to NA if any are negative
        
        #makeroom
        rm(blue1,green1,red1,nir1)
        
        #translate into a raster matrix
        neg.mask1 = blue
        neg.mask1[] = neg.mask
        
        #create mask, multiply by the land to make one mask
        neg_landmask = neg.mask1*land
        plot(neg_landmask)
        
        #mask the negative values and water pixels
        
        dat.stack = mask(x=dat.stack, mask = neg_landmask)
        
        
        #lets see what out new image looks like
        plot(dat.stack)
        
        ##create ndvi
        #ndvi = normalized difference vegetation index
        #this is a metrix used to measure health of plants by assessing amount of 
        # near infrared light reflected vs red light
        #ndvi = (near-infrared-red)/(near-infrared+red)
        #healthy plants will reflect more near infrared light and absorb more blue light
        #leading to a smaller number
        
        #with rasters you can multiple any layers together as each pixel represents
        # a numerical value, you can then complete any number of calculations to better assess
        #your raster data, there are many vegetation index examples online
        #with the [[]] we are extracting one layer from our data stack
        #our 3rd layer is red and our 4th layer is near-infrared
        ndvi = (dat.stack[[4]]-dat.stack[[3]])/(dat.stack[[3]]+dat.stack[[4]])
        
        plot(ndvi)
        
        #what do you see in this image? which areas have higher or lower ndvi?
        #what does this mean?
        
        #stack your ndvi layer with your other layers
        dat.stack1999 = stack(dat.stack, ndvi)
        
        ##name layers in dat stack
        names(dat.stack1999) = c("blue","green","red","near-infrared","ndvi")
        
        #remove all of your previous files other than the final image you created
        rm(list=setdiff(ls(), "dat.stack1999"))

     
#now for image #2 from 2022

        #import net cdf file (.nc file) by pasting the file path below
        #make sure to have "/" instead of "\" in your file path
        #make sue to have the landmask file downloaded and your two images
        nc.dat = ("D:/MarinePlantsPackage/L8_OLI_2022_02_02_15_43_59_014042_L2R.nc")
        
        ##open your net cdf compressed file
        nc = nc_open(nc.dat)
        nc_atts <- ncatt_get(nc, 0)
        
        ##take out blue layer for extent matching and coordinate refrence systsem (crs) matching
        blue1 = t(ncvar_get(nc, nc$var$rhos_483))
        blue = raster(blue1)
        ##changing projection (crs = coordinate reference system) with project4string
        proj4string(blue)  = crs(nc_atts$proj4_string)
        ##clipping the blue to nc atts extent
        extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        ##green
        ###get rest of bands to create final image
        green1 = t(ncvar_get(nc, nc$var$rhos_561))
        green = raster(green1)
        proj4string(green)  = crs(nc_atts$proj4_string)
        extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        ##red
        red1 = t(ncvar_get(nc, nc$var$rhos_655))
        red = raster(red1)
        proj4string(red)  = crs(nc_atts$proj4_string)
        extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        ##nir (near infrared)
        nir1 = t(ncvar_get(nc, nc$var$rhos_865))
        nir = raster(nir1)
        proj4string(nir)  = crs(nc_atts$proj4_string)
        extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
        
        #stack your layers to make your raster file
        dat.stack = stack(blue, green, red, nir)
        names(dat.stack) = c("blue", "green", "red", "near-infrared")
        
        #landmask
        #load in your land mask tif file
        land = raster("D:/MarinePlantsPackage/landmaskBimini.tif")
        #project land to same crs and extent as other layers
        land = projectRaster(land, blue, method="bilinear")
        
        #same process with cloud is followed for neg mask
        #switched nagetive mask to opporite so that it can work with inverse land mask, now all negative pixels will be 1
        neg.mask = ifelse(blue1<0 | green1<0 | red1<0 | nir1<0, NA, 1)#set to NA if any are negative
        
        #makeroom
        rm(blue1,green1,red1,nir1)
        
        ##what do the empty square brackets do?
        #cloud1 = dat.stack[[1]]
        #cloud1[] = cloud
        neg.mask1 = blue
        neg.mask1[] = neg.mask
        
        #create mask
        neg_landmask = neg.mask1*land
        
        #mask the negative values and water pixels
        
        dat.stack = mask(x=dat.stack, mask = neg_landmask)
        
        plot(dat.stack)
        
        ##createndvilayer
        ndvi = (dat.stack[[4]]-dat.stack[[3]])/(dat.stack[[3]]+dat.stack[[4]])
        
        plot(ndvi)
        
        #what do you see in this map? any changes you notice so far from the 1999 image?
        
        #stack your ndvi layer with your other layers
        dat.stack2022 = stack(dat.stack, ndvi)
        
        ##name layers in dat stack
        names(dat.stack2022) = c("blue","green","red","near-infrared","ndvi")
        
#we can now assess the amount of change in ndvi across the two maps!
      #subtract the 2022 image from 1999  
      changemap = (dat.stack1999[[5]]-dat.stack2022[[5]])
      plot(changemap)
      
      # so not we have a map with positive values = higher ndvi in 1999 and negative values 
      # = higher ndvi in 2022
      
      #lets take a closer look at the north end of the island by reading in a 
      #import shape file and cropping our image (paste the .shp path)
      Northcrop = readOGR("D:/MarinePlantsPackage/NorthBimini.shp")
      #plot the shapefile first to see what we are cropping to
      plot(Northcrop, add=T)
      
      #lets also look at the south end of Bimini
      Southcrop = readOGR("D:/MarinePlantsPackage/SouthBiminiMan.shp")
      #and lets look at what part of the map that is in
      plot(Southcrop, add=T)
      
      #crop the change map by the north crop
      changemapN = crop(changemap, Northcrop)
      #crop the change map by the south crop
      changemapS = crop(changemap, Southcrop)
      
      #crop the change map by the south crop
      plot(changemapN)
      plot(changemapS)
      
      #what do you see? where is the ndvi higher in 1999 (more green) than in 2022
      # in the south map and the north map? what could this indicate?
      
# congrats! we have now taken 2 satellite images, calculated vegetation index for each 
      #and assessed marine vegetation change at our study site between 1999 and 2022!!
      
      
      
        
        
              
