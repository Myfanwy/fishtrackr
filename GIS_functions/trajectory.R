require(dplyr)
library(dplyr)
require(maptools)
require(rgdal)
library(plotKML)
library(fossil)
#example

ams.ny <- geopath(lon1=4.892222, lon2=-74.005973, lat1=52.373056, lat2=40.714353,
                  print.geo=TRUE)
# write to a file:
kml(ams.ny)

d <- tbl_df(read.csv("stations_regions.csv", header=T, stringsAsFactors=F))
d
dy <- filter(d, Region=="YoloBypass")
dy
dy <- filter(distinct(dy, Station))
dy
