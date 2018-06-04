# https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/

library(adimpro)
img <- read.raw('/media/user/Edison/datascience@BGU/Analyzing Raster image Data/Data/rain_1/cat.png', cspace="sRGB", compress=F) # img read as 'adimpro' object
show.image(img)

# Coordinate system = WGS 84 (EPSG:4326)
setwd("/media/user/Edison/datascience@BGU/Analyzing Raster image Data/Data/radiation/geotiff")
library(raster)
# sudo apt-get install libgdal1-dev libproj-dev
library(rgdal)

sg = stack(list.files(pattern='*.tif'))
gimms = brick(sg)
rm(sg)

fun <- function(x) { 
  gimms.ts = ts(x, start=c(1982,1), end=c(1983,1), frequency=1)
  x <- aggregate(gimms.ts) 
}
gimms.sum <- calc(gimms, fun)
gimms.sum=gimms.sum/1
plot(gimms.sum)


time <- 1:nlayers(gimms.sum) 
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }}
gimms.slope=calc(gimms.sum, fun)
gimms.slope=gimms.slope*25
plot(gimms.slope)

##
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }}
p <- calc(gimms.sum, fun=fun)
plot(p, main="p-Value")

m = c(0, 0.05, 1, 0.05, 1, 0)
rclmat = matrix(m, ncol=3, byrow=TRUE)
p.mask = reclassify(p, rclmat)
fun=function(x) { x[x<1] <- NA; return(x)}
p.mask.NA = calc(p.mask, fun)

trend.sig = mask(gimms.slope, p.mask.NA)
plot(trend.sig, main="significant NDVI change")




### Delete later to convert tiff to ascii
just try with 3 lines of R code:
#load the package
library(raster)
#read your file
r <- raster(system.file("external/test.grd", package="raster"))
#export it to asc (ESRI ASCII)
writeRaster(r, filename="allint.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)
#Done
