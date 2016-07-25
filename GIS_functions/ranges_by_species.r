library(sp)
library(dismo)

chn <- filter(encounters, Species=="Chinook")
wst <- filter(encounters, Species=="White_Sturgeon")

chn_st <- select(chn, Station, Lat, Lon)
wst_st <- select(wst, Station, Lat, Lon)

chn_only <- anti_join(chn_st, wst_st)
chn_only <- distinct(chn_only, Station)
chn_only <- arrange(chn_only, Station)

wst_only <- anti_join(wst_st, chn_st)
wst_only <- distinct(wst_only, Station)
wst_only <- arrange(wst_only, Station)

## Isolating Chn-only points ##
cs <- as.character(chn_only$Station)
length(cs)

latlon <- select(encounters, Station, Lat, Lon)
latlon <- filter(distinct(latlon, Station))
latlon$Station <- as.character(latlon$Station)

cs <- data.frame(as.character(cs))
colnames(cs) <- "Station"
cs <- arrange(cs, Station)
head(cs)
cs1 <- semi_join(latlon, cs)  ##have to pull the rows of the latlon df that match the lat/lons of cs.
head(cs)
head(cs1)


## Isolating wst-only points ##
ws <- as.character(wst_only$Station)
ws <- data.frame(as.character(ws))
colnames(ws) <- "Station"
head(ws)
ws <- arrange(ws, Station)
ws1 <- semi_join(latlon, ws)
head(ws1)

## Begin Plotting Points

f <- extent(-122.652, -120.3113, 37.73557, 40.32957) #wider map for drawing
g = gmap(f, lonlat=T)
plot(g, interpolate=TRUE)

won <- as.data.frame(ws1)
coordinates(won) <- c("Lon", "Lat")
plot(won, cex=2, pch=16, col='gray15', add=T)

con <- as.data.frame(cs1)
coordinates(con) <- c('Lon', 'Lat')
plot(con, cex=2, pch=16, col='red', add=T)


com <- as.data.frame(common)
coordinates(com) <- c('Lon', 'Lat')
plot(com, cex=1.5, pch=16, col='gray50', add=T)
