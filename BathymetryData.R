rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("stars")
library("sf")

##atBIO----
bath<-raster("/Users/bodata/Desktop/Bathymetry/gebco_2021_n13.tif")
write.data = "/Users/bodata/Documents/Emily/gebcocropped.tif"
write.data2 = "/Users/bodata/Documents/Emily/sat.tif"
nc.dat<-("/Users/bodata/Documents/Emily/cropped/S2A_MSI_2016_07_30_07_55_54_T36KYU_L2R.nc")
nc = nc_open(nc.dat)
nc_atts <- ncatt_get(nc, 0)
##csv file with depth data form Nakia
moz<-read.csv("/Users/bodata/Downloads/Mozambique Coordinates - Sheet1.csv")

##onlaptop----
bath<-raster("D:/Bathymetry/gebco_2021_n13.tif")
write.data = ("D:/Bathymetry/gebcocropped.tif")
write.data2 = ("D:/Bathymetry/sat.tif")
nc.dat<-("D:/S2A_MSI_2016_07_30_07_55_54_T36KYU_L2R.nc")
nc = nc_open(nc.dat)
nc_atts <- ncatt_get(nc, 0)
##csv file with depth data form Nakia
moz<-read.csv("D:/Mozambique Coordinates - Sheet1(1).csv")
head(moz)
install.packages(c("rgdal", "sp"))

##convert crs of csv file to local crs of sat data----
test<-SpatialPointsDataFrame(moz[,3:2], moz, 
                       proj4string = bath@crs)
test2<-spTransform(test,sat@crs)

# Export shapefile----
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

##import satdata----
blue1 = t(ncvar_get(nc, nc$var$rhos_492))
#blue1 = t(ncvar_get(nc, nc$var$sza))
blue = raster(blue1)
##changeing projection with project4string, is proj4string a variable in nc_atts?
proj4string(blue)  = crs(nc_atts$proj4_string)
##clipping the blue to nc atts extent
extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
##
green1 = t(ncvar_get(nc, nc$var$rhos_560))
green = raster(green1)
proj4string(green)  = crs(nc_atts$proj4_string)
extent(green) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
##
red1 = t(ncvar_get(nc, nc$var$rhos_665))
red = raster(red1)
proj4string(red)  = crs(nc_atts$proj4_string)
extent(red) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])
##
nir1 = t(ncvar_get(nc, nc$var$rhos_833))
nir = raster(nir1)
proj4string(nir)  = crs(nc_atts$proj4_string)
extent(nir) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])

sat=stack(blue, green, red, nir)
names(sat)=c("blue", "green", "red", "nir")
rm(blue, green, red, nir)
rm(blue1, green1, red1, nir1)

#Crop depth data----
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

##make bathymetry into contours----
##to make space
rm(dat.stack)
rm(bath)
rm(e)
rm(r)
rm(nc_atts)
rm(nc.dat)
##convert to polygon and make contours
bathy.st <- st_as_stars(bath2)
bathy.poly <- st_as_sf(bathy.st)
bathy.poly$strata[bathy.poly$layer >= -25]                          <- "< 25 meters" # Make everything great that 25 meter depth a strata
bathy.poly$strata[bathy.poly$layer >= -50 & bathy.poly$layer < -25] <- "25 to 50 meters"
bathy.poly$strata[bathy.poly$layer >= -75 & bathy.poly$layer < -50] <- "50 to 75 meters"
bathy.poly$strata[bathy.poly$layer >=-100 & bathy.poly$layer < -75] <- "75 to 100 meters"
bathy.poly$strata[bathy.poly$layer >=-125 & bathy.poly$layer < -100] <- "100 to 125 meters"
bathy.poly$strata[bathy.poly$layer >=-150 & bathy.poly$layer < -125] <- "125 to 150 meters"
bathy.poly$strata[bathy.poly$layer >=-175 & bathy.poly$layer < -150] <- "150 to 175 meters"
bathy.poly$strata[bathy.poly$layer >=-200 & bathy.poly$layer < -175] <- "175 to 200 meters"
bathy.poly$strata[bathy.poly$layer >=-225 & bathy.poly$layer < -200] <- "200 to 225 meters"
bathy.poly$strata[bathy.poly$layer >=-250 & bathy.poly$layer < -225] <- "225 to 250 meters"
bathy.poly$strata[bathy.poly$layer <=-250] 

# Now make a multi with each 25 meter depth strata a multipolygon object
depth.strata <- aggregate(bathy.poly, list(bathy.poly$strata), function(x) x[1])
# and now clip this to our survey domain (again, last time we used a raster bounding box to trim down, now we really clip it properly)
depth.strata <- st_intersection(depth.strata,survey.domain)
# And calculate the area of each bathy strata
depth.strata$area <- st_area(depth.strata) %>% units::set_units("km^2") %>% as.numeric()
tot.area.bathy <- sum(depth.strata$area) # It is within 3 km^2 of the survey domain, I'll take that!
depth.strata$p.area <- depth.strata$area /tot.area.bathy


##plot all data----
##plots all 4 bands
plot(sat)
##plots only first band (blue?)
plot(sat[[1]])
##adds bathymetric data on top of band 
plot(test2, add=T)
plot(bath2)
plot(test2,add=T,pch=5)
##plot bathymetric data and bathymetric points
plot(bath2)
plot(test2, add=T, pch=7)


writeRaster(sat, write.data2, format="GTiff", NAflag =NaN, overwrite =T)

plot(bath2)
bathsat=stack(sat,bath2)
names(bathsat)=c("blue", "green", "red", "nir","depth")
