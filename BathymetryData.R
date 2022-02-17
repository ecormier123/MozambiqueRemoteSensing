rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library(fields)

bath<-raster("/Users/bodata/Desktop/Bathymetry/gebco_2021_n13.tif")
write.data = "/Users/bodata/Documents/Emily/gebcocropped.tif"
write.data2 = "/Users/bodata/Documents/Emily/sat.tif"
nc.dat<-("/Users/bodata/Documents/Emily/cropped/S2A_MSI_2016_07_30_07_55_54_T36KYU_L2R.nc")
nc = nc_open(nc.dat)
nc_atts <- ncatt_get(nc, 0)
##csv file with depth data form Nakia
moz<-read.csv("/Users/bodata/Downloads/Mozambique Coordinates - Sheet1.csv")
head(moz)
install.packages(c("rgdal", "sp"))

test<-SpatialPointsDataFrame(moz[,3:2], moz, 
                       proj4string = bath@crs)
test2<-spTransform(test,sat@crs)
 plot(sat)
 plot(sat[[1]])
 plot(test2, add=T)
 plot(bath2)
 plot(test2,add=T,pch=5)


##maybe?
proj4string(moz)  = crs(nc_atts$proj4_string)

plot(moz+bath2)
plot(moz)

# Export shapefile
st_write(oahu_lat_long_sf, )
# data.frame
class(MyData)
#nir 10m
if(any(names(nc$var)%in%"rhos_833" ==T)==T){nir833 = raster(t(ncvar_get(nc, nc$var$rhos_833 )))}
#blue 10m
if(any(names(nc$var)%in%"rhos_492" ==T)==T){blue = raster(t(ncvar_get(nc, nc$var$rhos_492)))}
#green 10m
if(any(names(nc$var)%in%"rhos_560" ==T)==T){green = raster(t(ncvar_get(nc, nc$var$rhos_560)))}
if(any(names(nc$var)%in%"rhos_559" ==T)==T){green = raster(t(ncvar_get(nc, nc$var$rhos_559)))}
#red 10m resolution
if(any(names(nc$var)%in%"rhos_665" ==T)==T){red = raster(t(ncvar_get(nc, nc$var$rhos_665)))}

sat=stack(blue, green, red, nir833)
names(sat)=c("blue", "green", "red", "nir")

rm(blue, green, red, nir833)
##changeing projection with project4string, is proj4string a variable in nc_atts?
proj4string(sat)  = crs(nc_atts$proj4_string)
##clipping the blue to nc atts extent
extent(sat) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
#Crop depth data
##create a polygon with xmin, xmax and ymin, ymax
e <- as(extent(32, 38,-28, -20), 'SpatialPolygons')
##give the polygon a projection matching original bathymetry (print bath)
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
##crop bathymetry with polygon
r <- crop(bath, e)
##convert projection of bath to projection of sat
bath2 = projectRaster(r, sat, method="bilinear")
writeRaster(bath2, write.data, format="GTiff",NAflag = NaN, overwrite=T)
##turn all NAs to -130
bath2[is.na(bath2[])] = -130
##turns all greater than -130 to na
thresh.depth=-130

writeRaster(sat, write.data2, format="GTiff", NAflag =NaN, overwrite =T)

plot(bath2)
bathsat=stack(sat,bath2)
names(bathsat)=c("blue", "green", "red", "nir","depth")
