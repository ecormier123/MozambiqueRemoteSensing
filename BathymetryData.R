rm(list = ls())
library("ncdf4")
library("raster")
library("rgdal")
library("fields")
library("stars")
library("sf")
library("sp")
library(ddalpha)

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

##import satdata for crs----
blue1 = t(ncvar_get(nc, nc$var$rhos_492))
#blue1 = t(ncvar_get(nc, nc$var$sza))
blue = raster(blue1)
##
rm(blue1)
##changeing projection with project4string, is proj4string a variable in nc_atts?
proj4string(blue)  = crs(nc_atts$proj4_string)
##clipping the blue to nc atts extent
extent(blue) = c(nc_atts$xrange,nc_atts$yrange[2],nc_atts$yrange[1])



##convert crs of csv file to local crs of sat data----
test<-SpatialPointsDataFrame(moz[,3:2], moz, 
                             proj4string = bath@crs)
test2<-spTransform(test,blue@crs)

#Crop depth data ----
##create a polygon with xmin, xmax and ymin, ymax
e <- as(extent(32, 38,-28, -20), 'SpatialPolygons')
##give the polygon a projection matching original bathymetry (print bath)
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
rm(nc_atts)
rm(nc.dat)
rm(nc)
##crop bathymetry with polygon
r <- crop(bath, e)
rm(bath, e)
##convert projection of bath to projection of sat
bath2 = projectRaster(r, blue, method="bilinear")
writeRaster(bath2, write.data, format="GTiff",NAflag = NaN, overwrite=T)
##turn all NAs to -130
bath2[is.na(bath2[])] = -130
contour<-rasterToContour(bath2)

##make bathymetry into contours with fisheries module method----
##to make space
rm(r)
rm(moz)
rm(test)
rm(blue)
##convert to polygon and make contours
bathy.st <- st_as_stars(r)
rm(r)
bathy.poly <- st_as_sf(bathy.st)
bathy.poly$layer[bathy.poly$gebco_2021_n13 > 0] <- 0
bathy.poly$layer[bathy.poly$gebco_2021_n13 < -250] <- -250
bathy.poly$strata <- NA
bathy.poly$strata[bathy.poly$gebco_2021_n13 >= -25]                          <- "< 25 meters" # Make everything great that 25 meter depth a strata
bathy.poly$strata[bathy.poly$gebco_2021_n13 >= -50 & bathy.poly$layer < -25] <- "25 to 50 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >= -75 & bathy.poly$layer < -50] <- "50 to 75 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >=-100 & bathy.poly$layer < -75] <- "75 to 100 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >=-125 & bathy.poly$layer < -100] <- "100 to 125 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >=-150 & bathy.poly$layer < -125] <- "125 to 150 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >=-175 & bathy.poly$layer < -150] <- "150 to 175 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >=-200 & bathy.poly$layer < -175] <- "175 to 200 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >=-225 & bathy.poly$layer < -200] <- "200 to 225 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 >=-250 & bathy.poly$layer < -225] <- "225 to 250 meters"
bathy.poly$strata[bathy.poly$gebco_2021_n13 <=-250] 

# Now make a multi with each 25 meter depth strata a multipolygon object
depth.strata <- aggregate(bathy.poly, list(bathy.poly$strata), function(x) x[1])

# And calculate the area of each bathy strata
depth.strata$area <- st_area(depth.strata) %>% units::set_units("km^2") %>% as.numeric()
tot.area.bathy <- sum(depth.strata$area) # It is within 3 km^2 of the survey domain, I'll take that!
depth.strata$p.area <- depth.strata$area /tot.area.bathy

##plot all data----
##plots all 4 bands
plot(blue)
##plots only first band (blue?)
plot(sat[[1]])
##adds bathymetric data on top of band 
plot(test2, add=T)
plot(bath2)
plot(test2,add=T,pch=5)
##plot bathymetric data and bathymetric points
plot(bath2)
plot(test2, add=T)
##to plot labels 
##text(test2, labels = test2$Average.Depth..m., pos = 4, offset = 0.7)
plot(contour, add=T)


