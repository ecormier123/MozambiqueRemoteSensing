#mozambique
#land.shp = shapefile("D:\\HighTide\\landpolygon_edit.shp")
#water = shapefile("D:\\HighTide\\water.shp")
gc()
rm(list=ls())
library("ncdf4")
library("raster")
library("sf")

out.dat = ("C:\\Users\\cormi\\Documents\\ImageProcessing\\landmask\\landmaskBimini.tif")
out.dat2 = ("C:\\Users\\cormi\\Documents\\ImageProcessing\\landmask\\watermaskBimini.tif")
out.dat.shp = ("C:\\Users\\cormi\\Documents\\ImageProcessing\\landmask\\landmask.shp")
##
##
#thresh.land = 0.2 #land threshold
thresh.land2 = 0.045#land threshold
#use nc file of satellite image to be able to get a reference coordinate system
nc.dat ="C:/Users/cormi/Documents/ImageProcessing/Reference/L8_OLI_2016_05_08_15_43_25_014042_L2R.nc"
nc = nc_open(nc.dat)
##

#Build mask

#decided to not include this threshold as it did not work as well as the one below
#land = t(ncvar_get(nc, nc$var$rhot_1609))
#land=raster(land)
#nc_atts <- ncatt_get(nc, 0)
#proj4string(land)  = crs(nc_atts$proj4_string)
#extent(land) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
#land[land>=thresh.land]=2
#land[land<thresh.land]=1
#plot(land)


#take the 865 band layer (near infrared) to build mask, this layer was chosen based on
#testing out different masks in snap
land2 = t(ncvar_get(nc, nc$var$rhos_865))
land2=raster(land2)
nc_atts <- ncatt_get(nc, 0)
#make sure crs of band matches image
proj4string(land2)  = crs(nc_atts$proj4_string)
#match extent of land to the satellite image
extent(land2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
remove(nc, nc_atts)
#use thresholds above to crop the new nir band layer 
#this sets any pixels that are greater or equal to the threshold  to a value of 2
land2[land2>=thresh.land2]=2
#sets any values less than thresh.land to 1
land2[land2<thresh.land2]=1
plot(land2)
#land3 = land2*land
#land3[land3>1]=2
#plot(land3)

##edits to land mask, create in SNAP, land = areas designated as water by mask above, but are actually land   
##and watermask = areas that were initially designated as land but were actually water

#polygon of areas that are marked water but should be land
land.shp = shapefile("C:\\Users\\cormi\\Documents\\test\\landmask\\landpolygon_edit_Polygon.shp")

#match the land polygon coordinate system to the land mask
land.shp = spTransform(land.shp,crs(land2))
#mask the previous land mask with the polygons, inverse=T means the area of the polygons will be added to the mask
#updatevalue to 2 to match the rest of the land
landmask =  mask(x = land2, mask = land.shp, inverse=T, updatevalue=2)
plot(landmask)

##Turn land pixels to water
water = shapefile("C:\\Users\\cormi\\Documents\\test\\landmask\\water_Polygon.shp")
water = spTransform(water,crs(land2))
#changes any values found within the water polygons to a value of 1 to match other
landmask2=  mask(x = landmask, mask = water, inverse=T, updatvalue=1)
plot(landmask2)

#turn all water pixels to NA
landmask2[landmask2==1]=NA
#turn all land pixels to 1
landmask2[landmask2==2]=1
plot(landmask2)

##export new landmask----
writeRaster(landmask2, out.dat, format="GTiff",NAflag = NaN, overwrite=T)
#export as shapefile 
#convert raster to a shapefile
landmask2pol = rasterToPolygons(landmask2)
#export the shapefile using the 'raster' package
raster::shapefile(landmask2pol, out.dat.shp ,overwrite=T)


#create an inverse mask for water
watermask=landmask2
#turn NA into 2
watermask[is.na(watermask[])]=2
#turn land pixels into NA
watermask[watermask==1]=NA
#turn all water pixels to 1
watermask[watermask==2]=1

#export watermask
writeRaster(watermask, out.dat2, format="GTiff",NAflag = NaN, overwrite=T)


#test[test==3]=1
#plot(test)

#plot(test)
#plot(test,xlim=c(725000,740000),ylim=c(5000000,5011000))
#plot(land,xlim=c(725000,740000),ylim=c(5000000,5011000))
#plot(water,add=T)
##
##land=test+landmask
#land[land==0]=NA
#land[land>0]=1
#plot(land)
##
##







##edit landmask-----ignore this----
landmask = "D:/HighTide/landmaskwater.tif"
landmask=raster(landmask)
plot(landmask)
land.shp=shapefile("C:/Users/cormi/Documents/ArcGIS/Projects/Mozambique Habitat Data/landmaskadd.shp")
land.shp = spTransform(land.shp,crs(landmask))
plot(land.shp, add=T)
landmask =  mask(x = landmask, mask = land.shp ,inverse=T, updatevalue=1)
landmask1[landmask1==1]=2
plot(landmask1)
out.dat = "D:/HighTide/landmaskwateredit2.tif"
writeRaster(landmask1,out.dat, format="GTiff",NAflag = NaN, overwrite=T)
land="D:\\HighTide\\landmaskwateredit2.tif"
#------
b= raster(t(ncvar_get(nc, nc$var$rhot_492)))
proj4string(b)  = crs(nc_atts$proj4_string)
extent(b) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
b = b*land
g= raster(t(ncvar_get(nc, nc$var$rhot_560)))
proj4string(g)  = crs(nc_atts$proj4_string)
extent(g) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
g = g*land
r= raster(t(ncvar_get(nc, nc$var$rhot_665)))
proj4string(r)  = crs(nc_atts$proj4_string)
extent(r) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
r = r*land
dat = stack(b,g,r,land)
rm(b,g,r,land)
writeRaster(dat, out.dat, format="GTiff",NAflag = NaN, overwrite=T)