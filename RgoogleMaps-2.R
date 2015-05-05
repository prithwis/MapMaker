# the correct version was not available on CRAN
install.packages("/home/hduser/Downloads/rjson_0.2.13.tar.gz",repos=NULL, type="source")

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

#
# R program to show specific addresses on a Google Map
#

setwd("/home/hduser/yau/maps")
library(rjson)
library(ggmap)
library(RgoogleMaps)
library(png)

#
# input data scraped off the web by running the python program
# https://github.com/prithwis/WebScraper/blob/master/SchoolDataScraper0.py
# the actual tsv file used in this exercise can be downloaded at
# https://github.com/prithwis/WebScraper/blob/master/CalcuttaSchools.tsv
#

Schools = read.csv(file="CalcuttaSchools.tsv",head=FALSE, sep="\t")

# since there is a limit on the number of geocoding requests that can be made, we work with only 5 schools
Schools = Schools[sample(1:nrow(Schools), 5, replace=FALSE),]
colnames(Schools) = c("Name", "Address")

# Lat, Lon is extracted along with address as understood by Google
GeoLocations = geocode(as.character(Schools$Address),output ='latlona')
MapData = cbind(Schools,GeoLocations)
names(MapData)[5] = "GooglePlace"
MapData = MapData[c("Name","lon","lat","Address","GooglePlace")]

# sanity check whether Address is similar to GooglePlace. If different, possible geolocation error
print(MapData[c("Address","GooglePlace","Name")])

# --------------------------------------------------------------------------

# Map is defined in terms of centre and zoom level
cent2 = c(mean(MapData$lat), mean(MapData$lon))
zoom2 = min(MaxZoom(range(MapData$lat), range(MapData$lon)))

# first get the map from Google as a png file
SchoolMap = GetMap(center = cent2, zoom = zoom2, destfile = "MapSchools.png", maptype = "map")
imgSchoolMap = readPNG("MapSchools.png")
grid::grid.raster(imgSchoolMap)

# Define set of long, lat to be plotted on map
LatSet = MapData$lat
LonSet = MapData$lon

# Plot points on the map
# to change plot symbols look at http://www.statmethods.net/advgraphs/parameters.html
PlotOnStaticMap(SchoolMap,lat = LatSet, lon = LonSet, cex = 0.7, pch = 6, col = "red", FUN = points, NEWMAP = TRUE)

# Name of the school, truncated to first 4 char, will be used as identify the points
NameSet = substr(as.character(MapData$Name),1,4)

# Location where name is printed, slightly different from the point plotted
LonOffSet2 = 0.005+LonSet

# Write names
PlotOnStaticMap(SchoolMap,lat = LatSet, lon = LonOffSet2, cex = 0.7, labels= NameSet, col = "black", FUN = text, add = T)

