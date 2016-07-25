library(dplyr)
install.packages("beepr")
library(beepr)
beep()

baydelta <- tbl_df(read.csv("Baydelta.csv", stringsAsFactors = FALSE)) #read data in
baydelta <- select(baydelta, -X)
baydelta <- tbl_df(BayDelta) # or just convert BayDelta if working from a saved ws
baydelta
baydelta$DetectDate <- as.POSIXct(strptime(baydelta$DetectDate,'%m/%d/%Y %H:%M:%S'))  # convert to R date/time format

test <- 
b %>%
  arrange(TagID, DetectDate) %>% ##order by TagID, then by DetectDate
  group_by(TagID, id = cumsum(!duplicated(TagID) | c(F,round(diff(DetectDate)/60) > 60))) %>%
  slice(c(1,length(DetectDate))) %>%
  mutate(Departure = DetectDate[2]) %>%
  slice(1) %>%
  ungroup 

write.csv(test, "Arr_dep_v1.csv")
rm(test, b) # clean up workspace

##after checking .csv, try the whole dataset:

encounters <- 
  baydelta %>%
  arrange(TagID, DetectDate) %>% ##order by TagID, then by DetectDate
  group_by(TagID, id = cumsum(!duplicated(TagID) | c(F,round(diff(DetectDate)/60) > 60))) %>%
  slice(c(1,length(DetectDate))) %>%
  mutate(Departure = DetectDate[2]) %>%
  slice(1) %>%
  ungroup 

beep(6)

getwd()
write.csv(encounters, "Arr_dep_v2.csv")


## separate into CHN and WST datasets
colnames(encounters)
unique(encounters$Species)
chn <- filter(encounters, Species=="Chinook")
wst <- filter(encounters, Species=="White_Sturgeon")

## Begin working with CHN
chn
write.csv(chn, "chn.csv")

## experimenting with "distinct" to generate single tracks from which to export lat/long:

test <- chn

test1 <- 
  test %>%
  group_by(TagID) %>%
  arrange(Departure) %>%
  distinct(Station) %>%
  select(TagID, Station, Lat, Lon, Rkm, tagging_date, Sex, Departure)
test1

## Calculate CHN monitor residence in YB
chnYB <- filter(chn, Region=="YoloBypass")
View(chnYB)
class(chnYB$Departure)
chnYB <- mutate(chnYB, enc_time_days=(difftime(Departure, DetectDate, units="days")))
chnYB <- mutate(chnYB, enc_time_hrs=(difftime(Departure, DetectDate, units="hours")))

## Summarize YB monitor residence in hours, by TagID
by_tagID <- group_by(chnYB, TagID)
chn_YBhrs <- 
  summarise(by_tagID, total = sum(enc_time_hrs))
chn_YBdays <- 
  summarise(by_tagID, total = sum(enc_time_days))

##Calculate monitor residence in hours
chn_YBhrs
chn_YBhrs$total <- as.numeric(chn_YBhrs$total) #re-class from difftime to numeric
summary(chn_YBhrs$total)
chn_YBhrs <- mutate(chn_YBhrs, units="hours", species="CHN")

# Calculate monitor residence in days
chn_YBdays
chn_YBdays$total <- as.numeric(chn_YBdays$total) #re-class from difftime to numeric
summary(chn_YBdays$total)
chn_YBdays <- mutate(chn_YBdays, units="days", species="CHN")

### Calculate CHN residence in YB (total)

encounters <- 
  baydelta %>%
  arrange(TagID, DetectDate) %>% ##order by TagID, then by DetectDate
  group_by(TagID, id = cumsum(!duplicated(TagID) | c(F,round(diff(DetectDate)/60) > 60))) %>%
  slice(c(1,length(DetectDate))) %>%
  mutate(Departure = DetectDate[2]) %>%
  slice(1) %>%
  ungroup 



## Calculate WST monitor residence in YB
wstYB <- filter(wst, Region=="YoloBypass")
View(wstYB)
class(wstYB$Departure)
wstYB <- mutate(wstYB, enc_time_days=(difftime(Departure, DetectDate, units="days")))
wstYB <- mutate(wstYB, enc_time_hrs=(difftime(Departure, DetectDate, units="hours")))

## Summarize YB monitor residence in hours, by TagID
by_tagID <- group_by(wstYB, TagID)
wst_YBhrs <- 
  summarise(by_tagID, total = sum(enc_time_hrs))
wst_YBdays <- 
  summarise(by_tagID, total = sum(enc_time_days))

wst_YBhrs
wst_YBhrs$total <- as.numeric(wst_YBhrs$total) #re-class from difftime to numeric
summary(wst_YBhrs$total)
wst_YBhrs <- mutate(wst_YBhrs, units="hours", species="WST")

wst_YBdays
wst_YBdays$total <- as.numeric(wst_YBdays$total) #re-class from difftime to numeric
summary(wst_YBdays$total)
wst_YBdays <- mutate(wst_YBdays, units="days", species="WST")

## make and export monitor residence table

YBmonitorRes <- rbind(chn_YBhrs, chn_YBdays, wst_YBhrs, wst_YBdays)
YBmonitorRes
write.csv(YBmonitorRes, "YBmonitorRes.csv")


### Identify First and Last Detection Locations

## testing with one Tag ##
bay2841 <- filter(baydelta, TagID==2841)
class(bay2841$DetectDate)
bay2841 <- select(bay2841, TagID, DetectDate,Station, Region, Rkm, Species, tagging_date)
bay2841 <- arrange(bay2841, DetectDate, TagID)
tail(bay2841)


  bay2841 %>%
  fin.loc <- summarize(bay2841, Station[DetectDate==max(DetectDate)])
  first.loc <- summarize(bay2841, Station[DetectDate==min(DetectDate)])

locations.cn <- c("TagID", "tagging_date", "tagging_loc", "first_loc", "fin_loc")
t2841 <- data.frame(TagID=2841, tagging_date= "3/20/2012", tagging.loc = "Fyke", first.loc, fin.loc)
t2841
colnames(t2841) <- locations.cn


## Modify for entire dataset ##
bay <- baydelta
library(dplyr)
fin <- 
  bay %>%
  select(TagID, DetectDate, Station, Region) %>%
  arrange(DetectDate, TagID) %>%
  group_by(TagID) %>%
  summarize(fin.loc = Station[DetectDate==max(DetectDate)][1], fin.region = Region[DetectDate==max(DetectDate)][1] )

first <- 
  bay %>%
  select(TagID, DetectDate, Station, Region) %>%
  arrange(DetectDate, TagID) %>%
  group_by(TagID) %>%
  summarize(first.loc = Station[DetectDate==min(DetectDate)][1], first.region = Region[DetectDate==min(DetectDate)][1] )

# returning some NAs - not sure why


locs <- inner_join(fin, first)
locs <- filter(locs, TagID !=23054)
locs <- filter(locs, !is.na(first.loc))

tag.loc <- read.csv("tagging_locations.csv", stringsAsFactors=F)
head(tag.loc)

sofar <- inner_join(locs, tag.loc)

streg <- tbl_df(read.csv("stations_regions.csv", stringsAsFactors=F))
streg <- select(streg, fin.loc =Station, Lat, Lon, Rkm)
streg$fin.loc <- as.character(streg$fin.loc)
sofar2 <- inner_join(sofar, streg)
sofar2 <- arrange(sofar2, TagID)

species <- select(bay, TagID, Species, tagging_date)
species <- species[!duplicated(species),]

locations <- inner_join(sofar2, species )
locations
locations <- select(locations, TagID, Tagging_Location, tagging_date, first.loc, fin.loc, first.region, fin.region, fin.Rkm = Rkm, Lat, Lon, Species)


### working with just the CHN data

chn_loc <-
  locations %>%
  filter(Species=="Chinook")

## extracting the Year from the tag date
install.packages("chron")
library(chron)
class(chn_loc$tagging_date)
chn_loc$tagging_date <- as.Date(chn_loc$tagging_date, format="%m/%d/%Y")  # convert to R date/time format
chn_loc$year <- as.chron(chn_loc$tagging_date)
chn_loc$year <- years(chn_loc$year) ## extract just the year
chn_loc
write.csv(chn_loc, "chn_loc.csv")

by_finreg <- group_by(chn_loc, fin.region, year)
endreg <- summarise(by_finreg, n())
endreg 

wst_loc <- 
  locations %>%
  filter(Species=="White_Sturgeon")


by_year <- group_by(chn_loc, year)
by_finloc <- group_by(chn_loc, fin.loc, Lat, Lon, year)
ends <- summarise(by_finloc, n())
ends
filter(ends, year==2013)

by_finreg <- group_by(chn_loc, fin.region, tagging_year = as.POSIXct(tagging_date, format="%Y"))
by_finreg
?POSIXct
endreg <- summarise(by_finreg, n())
endreg
