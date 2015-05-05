## Comments are preceded by ##

## This tutorial shows how to export spatial data as KML so that it can be plotted on Google Earth/ Google Maps. It provides an extension to "intro to spatial data worksheet" (available at rspatialtips.org) so very little explanation is provided for the first few steps.

##Data Requirements:

##London Cycle Hire locations.

##Install the following packages (if you haven't already done so):

##maptools, rgdal

## Load required packages
library(maptools)
library(rgdal)

##Set your working directory- this should be the folder where the above files are saved.

setwd("/XX/XX")

## Load the cycle hire  locations.

cycle<- read.csv("London_cycle_hire_locs.csv", header=T)

## Inspect column headings

head(cycle)

## Plot the XY coordinates (do not close the plot window).

plot(cycle$X, cycle$Y)

## create a SpatialPointsDataframe object and add the appropriate CRS

coordinates(cycle)<- c("X", "Y")

BNG<- CRS("+init=epsg:27700")

proj4string(cycle)<-BNG

## In order for the points to be displayed in the correct place they need to be re-projected to WGS84 geographical coordinates.

p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

cycle_wgs84<- spTransform(cycle, CRS= p4s)

## Using the OGR KML driver we can then export the data to KML. dsn should equal the name of the exported file and the dataset_options argument allows us to specify the labels displayed by each of the points.

writeOGR(cycle_wgs84, dsn="london_cycle_docks.kml", layer= "cycle_wgs84", driver="KML", dataset_options=c("NameField=name"))

## if you have Google Earth installed double click on the kml file you just created to open it. The points should be loaded as labelled pins on the map.If you click on the pin you will be able to see its full name and capacity. 


##Further Reading:

##Applied spatial data analysis with R. Bivand et al.


##Disclaimer: The methods provided here may not be the best solutions, 
##just the ones I happen to know about! No support is provided with these worksheets. 
##I have tried to make them as self-explanatory as possible and will not be able to 
##respond to specific requests for help. I do however welcome feedback on the tutorials.
##License: cc-by-nc-sa. Contact: james@spatialanalysis.co.uk