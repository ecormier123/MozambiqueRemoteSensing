##Mozambique
#land.shp = shapefile("D:\\HighTide\\landpolygon_edit.shp")
#water = shapefile("D:\\HighTide\\water.shp")
gc()
rm(list=ls())
library("ncdf4")
library("raster")

##
out.dat = ("C:\\Users\\cormi\\Documents\\test\\landmask\\landmaskBimini.tif")
## 
land.shp = shapefile("C:\\Users\\cormi\\Documents\\test\\landmask\\landpolygon_edit.shp")

water = shapefile("C:\\Users\\cormi\\Documents\\test\\landmask\\water.shp")
##
##
thresh.land = 0.2 #land threshold
thresh.land2 = 0.122#land threshold
nc.dat ="C:\\Users\\cormi\\Documents\\test\\L8_OLI_2016_05_08_15_43_25_014042_L2R.nc"
##

#Build mask
nc = nc_open(nc.dat)
land = t(ncvar_get(nc, nc$var$rhot_1609))
land=raster(land)
nc_atts <- ncatt_get(nc, 0)
proj4string(land)  = crs(nc_atts$proj4_string)
extent(land) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
land[land>=thresh.land]=2
land[land<thresh.land]=1
plot(land)
#build second mask
land2 = t(ncvar_get(nc, nc$var$rhot_865))
land2=raster(land2)
nc_atts <- ncatt_get(nc, 0)
proj4string(land2)  = crs(nc_atts$proj4_string)
extent(land2) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
land2[land2>=thresh.land2]=2
land2[land2<thresh.land2]=1
plot(land2)
land3 = land2*land
land3[land3>1]=2
plot(land3)
##Remove land pixels----
land.shp = spTransform(land.shp,crs(land))
remove(nc, nc_atts, land2)
landmask =  mask(x = land3, mask = land.shp ,inverse=T)
landmask[is.na(landmask)==T]=2
plot(landmask)
##Remove water pixels----
water = spTransform(water,crs(land))
test=  mask(x = landmask, mask = water ,inverse=T,updatevalue=3)
test[test==3]=1
plot(test)
test[test==1]=NA
#plot(test,xlim=c(725000,740000),ylim=c(5000000,5011000))
#plot(land,xlim=c(725000,740000),ylim=c(5000000,5011000))
#plot(water,add=T)
##
##land=test+landmask
#land[land==0]=NA
#land[land>0]=1
#plot(land)
##


writeRaster(land3, out.dat, format="GTiff",NAflag = NaN, overwrite=T)
##
##edit landmask-----
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