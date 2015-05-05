require(maptools)
require(rgdal)

unique(encounters$Species)
chn <- filter(encounters, Species=="Chinook")
wst <- filter(encounters, Species=="White_Sturgeon")

######################### Creating a KML of CHN Tracks ####################################

## experimenting with "distinct" to generate single tracks from which to export lat/long:

test <- chn

test1 <- 
  test %>%
  group_by(TagID) %>%
  arrange(Departure) %>%
  distinct(Station) %>%
  select(TagID, Station, Lat, Lon, Rkm, tagging_date, Sex, Departure)
test1


test2 <- as.data.frame(test1)
coordinates(test2) <- c("Lon", "Lat")
BNH <- CRS("+init=epsg:4326")
proj4string(test2) <- BNH
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
test2_wgs84 <- spTransform(test2, CRS=p4s)
writeOGR(test2_wgs84, dsn="chn_tracks.kml", 
         layer= "test2_wgs84", driver="KML", dataset_options=c("NameField=station"))

######################### Creating a KML of WST Tracks ####################################

test <- wst

test1 <- 
  test %>%
  group_by(TagID) %>%
  arrange(Departure) %>%
  distinct(Station) %>%
  select(TagID, Station, Lat, Lon, Rkm, tagging_date, Sex, Departure)
test1

test2 <- as.data.frame(test1)
coordinates(test2) <- c("Lon", "Lat")
BNH <- CRS("+init=epsg:4326")
proj4string(test2) <- BNH
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
test2_wgs84 <- spTransform(test2, CRS=p4s)
writeOGR(test2_wgs84, dsn="wst_tracks.kml", 
         layer= "test2_wgs84", driver="KML", dataset_options=c("NameField=station"))

######################### Creating a KML of Common Points ####################################

chn_st <- select(chn, Station, Lat, Lon, Rkm)
wst_st <- select(wst, Station, Lat, Lon, Rkm)
common <- semi_join(chn_st, wst_st)
common

test1 <- 
  common %>%
  distinct(Station)

test2 <- as.data.frame(test1)
coordinates(test2) <- c("Lon", "Lat")
BNH <- CRS("+init=epsg:4326")
proj4string(test2) <- BNH
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
common_wgs84 <- spTransform(test2, CRS=p4s)
writeOGR(common_wgs84, dsn="common_tracks.kml", 
         layer= "common_wgs84", driver="KML", dataset_options=c("NameField=Rkm"))


######################### Creating a KML of CHN-only Points ####################################

chn_st <- select(chn, Station, Lat, Lon)
wst_st <- select(wst, Station, Lat, Lon)
chn_only <- anti_join(chn_st, wst_st)
chn_only <- distinct(chn_only, Station)
#chn_only <- filter(chn_only, Station = !duplicated(Station)) #remove duplicates
chn_only <- arrange(chn_only, Station)

con <- as.data.frame(chn_only)
coordinates(con) <- c("Lon", "Lat")
BNH <- CRS("+init=epsg:4326")
proj4string(con) <- BNH
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
con_wgs84 <- spTransform(con, CRS=p4s)
writeOGR(con_wgs84, dsn="chn_tracks.kml", 
         layer= "con_wgs84", driver="KML", dataset_options=c("NameField=Rkm"))

wst_only <- anti_join(wst_st, chn_st)
wst_only <- distinct(wst_only, Station)
wst_only <- filter(wst_only, Station = !duplicated(Station))
wst_only <- arrange(wst_only, Station)
filter(wst_only, Station=="Wallace_weir")

## Combine to check
cs <- as.character(chn_only$Station)
ws <- as.character(wst_only$Station)
length(cs)
length(ws)

n <- max(length(cs), length(ws))
length(cs) <- n
length(ws) <- n
combined <- as.data.frame(cbind(cs, ws))
combined <- tbl_df(combined)
combined  ## df showing stations where only chn or only wst were detected.  The rest of the stations must have both.

### Making a vector of the common stations
cs <- chn_only$Station
ws <- wst_only$Station
ccc <-
  encounters %>%
  filter(Station %in% (c(cs, ws)))

ccc
## FAIL ##

################## Make a KML of WST only points ###############################

won <- as.data.frame(wst_only)
coordinates(won) <- c("Lon", "Lat")
BNH <- CRS("+init=epsg:4326")
proj4string(con) <- BNH
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
won_wgs84 <- spTransform(con, CRS=p4s)
writeOGR(won_wgs84, dsn="wst_tracks.kml", 
         layer= "won_wgs84", driver="KML", dataset_options=c("NameField=Rkm"))

