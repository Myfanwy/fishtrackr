# YB Workplan Analysis

# requires: fishpaths, grouped by joint stations
library(dplyr)
#	1. Up to what point in the Toe Drain do flow and water quality conditions allow fish to abort upstream migration and head back downstream (i.e., where is the point of no return)?
fishpaths <- filter(fishpaths, Sp == "chn")
fishpaths$year <- lubridate::year(fishpaths$DateTagged)

# Wallace Weir:
# - 2012: 9/12 made it up in 2012 (5 started there)
# - 2013: 0/52, which is odd; when did the trap go in? No fish rescues in 2013?
# - 2014: 3/30 fish 
# - 2015: 1/25 fish, and that was the first fish tagged in the season - when did the trap go in?

# Putah_creek:
# - 2012: 0/12 fish
# - 2013: 2/52 fish, both last detected at Putah creek, 12/02 & 12/17 respectively
# - 2014: ?/30 fish (didn't have a monitor)
# - 2015: 3/25 fish, all 3 last detected at Putah Creek, between 11/16-11/19 

# Order Stations correctly for plots #
fpstation <- unique(fishpaths$Station)
fpstation
stationabb <- c("BC_joint2", "BC_joint", "Base_TD",
  "Rstr_joint", "Lisbon", "Abv_lisbon", 
  "Putah_creek", "Levee_marker", "I80_1",
  "Swanston", "Abv_swanston", "Cache_creek", "Knaggs", "Wallace_weir")
identical(fpstation, sort(stationabb)) #have to match before releveling the factors
fishpaths$Station <- factor(fishpaths$Station, levels = stationabb) # correctly orders the levels for plots

# Plotting
library(ggplot2)
library(reshape2)

# start with a single one
p <- filter(fishpaths, TagID == 37845)
p <- arrange(p, arrival, departure)
unique(p$Station)

# line plot
ggplot(p, aes(x = arrival, y = Station, group = TagID)) + geom_line()

# Step plot from melted df
pm <- melt(p, measure.vars = c("arrival", "departure"), value.name = "step")
head(pm)
ggplot(pm, aes(x = step, group = TagID)) + geom_step(aes(y = Station))

# Plot Full (melted) df
pmf <- melt(fishpaths, measure.vars = c("arrival", "departure"), value.name = "step")

# filter out weird tags
pmf <- dplyr::filter(pmf, TagID != 31550, TagID != 13729, TagID != 20168, TagID != 20164, TagID != 20156, TagID != 13728, TagID != 13720) # filter out weird tags (that were detected before being tagged)
ggplot(pmf, aes(x = step, group = TagID)) + geom_step(aes(y = Station, color = factor(TagID), alpha = 0.4)) +
  theme(legend.position = "none") + facet_grid(~year, scales = "free") + ggtitle("All Fish Paths by Tagging Year") +
  xlab("Date")

ggsave("AllFishPaths_by_Year.png")

## Plotting full dataset (all fish), separated by year
fp12 <- filter(fishpaths, year == 2012)
ggplot(fp12, aes(x = arrival, y = Station, group = TagID)) + geom_line(aes(color = factor(TagID))) + 
  theme(legend.position = "none") + ggtitle("FishPaths 2012")

fp13 <- filter(fishpaths, year == 2013)
fp13 <- filter(fp13, arrival > "2012-10-01 00:00:00") #filter out weird detections from 2012
range(fp13$arrival)
tail(arrange(fp13, departure))
ggplot(fp13, aes(x = arrival, y = Station, group = TagID)) + geom_line(aes(color = factor(TagID))) + 
  theme(legend.position = "none") + ggtitle("FishPaths 2013")

fp14 <- filter(fishpaths, year == 2014)
head(arrange(fp14, arrival)) # weird lone tag out front, ID 20156
tail(arrange(fp14, departure)) # shed tag at the end, 13728
fp14 <- filter(fp14, arrival > "2014-07-01 00:00:00") #filter out weird detections from 2012
ggplot(fp14, aes(x = arrival, y = Station, group = TagID)) + geom_line(aes(color = factor(TagID))) + 
  theme(legend.position = "none") + ggtitle("FishPaths 2014")

fp15 <- filter(fishpaths, year == 2015)
fp15 <- filter(fp15, arrival > "2014-07-01 00:00:00") #filter out weird detections from 2012
ggplot(fp15, aes(x = arrival, y = Station, group = TagID)) + geom_line(aes(color = factor(TagID))) + 
  theme(legend.position = "none") + ggtitle("FishPaths 2015")

# plotting shed tags:
fp14[940:951, c(1:3)]
shed <- c(20168, 13729, 20164, 13728)
shed2 <- filter(fp14, fp14$TagID %in% shed)
shedm <- melt(shed2, measure.vars = c("arrival", "departure"), value.name = "step")
ggplot(shedm, aes(x = step, group = TagID)) + geom_step(aes(y = Station, color = factor(TagID)))

# Does highest river kilometer reached correlate with tag date?

maxrkm15 <- fp15 %>% 
  group_by(TagID) %>% 
  mutate(maxrkm = max(Rkm))

ggplot(maxrkm15, aes(x = DateTagged, y = maxrkm)) + geom_point(alpha = 0.5) + 
  theme(legend.position = "none") +
  geom_smooth() + ggtitle("Tag Date vs. Maximum River Kilometer Reached")

# 2013 (best sample size)
maxrkm13 <- fp13 %>% 
  group_by(TagID) %>% 
  mutate(maxrkm = max(Rkm))

ggplot(maxrkm13, aes(x = DateTagged, y = maxrkm)) + geom_point(alpha = 0.5) + 
  theme(legend.position = "none") +
  geom_smooth(method = "lm") + ggtitle("Tag Date vs. Maximum River Kilometer Reached, 2013")

# 2014 (second best sample size)
maxrkm14 <- fp14 %>% 
  group_by(TagID) %>% 
  mutate(maxrkm = max(Rkm))

ggplot(maxrkm14, aes(x = DateTagged, y = maxrkm)) + geom_point(alpha = 0.5) + 
  theme(legend.position = "none") +
  geom_smooth(method = "lm") + ggtitle("Tag Date vs. Maximum River Kilometer Reached, 2014")

# Pool all the data
tagdate <- fishpaths
tagdate$DateTaggedAbb <- as.POSIXlt(tagdate$DateTagged, format = "%Y-%m-%d")
tagdate$m <- lubridate::month(tagdate$DateTaggedAbb)
tagdate$d <- lubridate::day(tagdate$DateTaggedAbb)
tagdate$dm <- paste(tagdate$m, tagdate$d, sep = "-")
class(tagdate$dm)
tagdate$dm <- as.POSIXct(tagdate$dm, format = "%m-%d")
tail(tagdate)
tagdate <- select(tagdate, -DateTaggedAbb, -m, -d)

#add maxrkm column
maxrkm <- tagdate %>% 
  group_by(TagID) %>% 
  mutate(maxrkm = max(Rkm))

ggplot(maxrkm, aes(x = dm, y = maxrkm)) + geom_point(alpha = 0.5) + 
  theme(legend.position = "none") +
  geom_smooth(method = "lm") + ggtitle("Tag Date vs. Maximum River Kilometer Reached, all years") +
  xlab("Date Tagged") + ylab("Maximum River Kilometer Reached")

ggsave("TagDate_vs_MaxRkm.png")

# See if that one tagged fish is an outlier
maxrkmoutlier <- maxrkm %>% filter(dm > "2016-09-15")
ggplot(maxrkmoutlier, aes(x = dm, y = maxrkm)) + geom_point(alpha = 0.5) + 
  theme(legend.position = "none") +
  geom_smooth(method = "lm") + ggtitle("Tag Date vs. Maximum River Kilometer Reached \nOutlier Removed \nall years") +
  xlab("Date Tagged") + ylab("Maximum River Kilometer Reached")

ggsave("TagDate_vs_MaxRkm_outliersgone.png")

## Use Pooled Data to plot Tag Location vs. Max Rkm reached

# fix tag locaiton name inconsistencies
library(stringr)
allt <- as.data.frame(alltags)
allt$Tagging_Location <- str_replace(allt$Tagging_Location, 'Fyke', 'FYKE')
allt <- select(allt, TagID, Tagging_Location, DateTagged, Sp)

maxrkm2 <- inner_join(maxrkm, allt)
maxrkm2 <- filter(maxrkm2, Tagging_Location != "WW") # filter out fish tagged at Wallace
maxrkm2$Tagging_Location <- factor(maxrkm2$Tagging_Location, levels = c("BRSTR", "FYKE", "LIS", "Abv_LIS"))

ggplot(maxrkm2, aes(x = Tagging_Location, y = maxrkm, group = TagID)) + geom_point(alpha = 0.5) + 
  ggtitle("Tagging Location vs. Maximum River Kilometer Reached, all years")

# Do by RiverKm
maxrkm2$Tagging_Location <- as.character(maxrkm2$Tagging_Location)
# Group FYKE and LIS
taglockey <- as.data.frame(cbind(Tagging_Location = c("BRSTR", "FYKE", "LIS", "Abv_LIS"), TagLocRkm = c(117, 134, 134, 136)))
taglockey
maxrkm2 <- inner_join(maxrkm2, taglockey, by = "Tagging_Location")
class(maxrkm2$TagLocRkm)
maxrkm2$TagLocRkm <- as.numeric(as.character((maxrkm2$TagLocRkm))) # change back to numeric


# Filter out duplicate maxrkms for each fish
maxrkm2 <- filter(maxrkm2, !duplicated(TagID))

p <- ggplot(maxrkm2, aes(x = TagLocRkm, y = maxrkm, group = TagID)) + geom_point(aes(alpha = 0.5), position = "jitter") + 
  theme(legend.position = "none") +  geom_smooth(method = "lm") + 
  ggtitle("Tagging Location vs. Maximum River Kilometer Reached, all years")
p + geom_smooth()

## Begin Modeling
model <- lm(maxrkm ~ Tagging_Location, data = maxrkm2)
summary(model) # for every one step increase in maxrkm, tagging at bRSTR decreases by 1.02 rkm?
m2 <- lm(maxrkm ~ TagLocRkm, data = maxrkm2)
summary(m2)
#	2. Do adult fish typically move at night or during daylight hours?

# 3. What is the fate of rescued fish?
## 3: fate of single rescued fish from 2015:

res <- filter(fishpaths, TagID == 37845) #tagid of rescued fish from 12/27 at WW
res2 <- filter(fishpaths, TagID == 37835) #tagid of rescued fish that shed its tag

head(res)
head(res2)

str(res)

r = as.POSIXct(range(f$DateTimeUTC))


