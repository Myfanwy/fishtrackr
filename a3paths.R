library(ybp)
library(dplyr)
library(ggplot2)
library(stringr)

## @knitr a3paths

source("functions_built/fishpaths.R")

dtags <- f %>% 
  group_by(year, TagID) %>% 
  filter(Station == "Swanston" | Station == "Abv_swanston" |
           Station == "Cache_creek" | Station == "Knaggs" | Station == "Wallace_weir") %>% 
  filter(Sp == "chn")
dtags <- unique(dtags$TagID)

a3 <- filter(f, TagID %in% dtags)

a3 <- a3 %>% 
  filter(Sp == "chn") %>% 
  group_by(TagID) %>% 
  arrange(DateTimeUTC)
a3 <- filter(a3, year == 2015 | year == 2014)

f1 = split(a3, list(a3$TagID, a3$Station)) # splits the dataframe into a list, where each element of the list is every combination of TagID and station, and the detections within that.
# glimpse <- f1[sapply(f1, nrow) == 34] # see how it looks: these are all the fish/station combos with 34 rows

f1 = f1[ sapply(f1, nrow) > 0 ] # filter out the combos that don't actually occur

tmp = lapply(f1, splitFishStationVisits) # apply this function, which adds the grouping variable, then calls on the other function, which acts on the grouping variable

a3paths = do.call(rbind, tmp) # get the whole thing into a df
a3paths$year <- lubridate::year(a3paths$DateTagged)

# Order Stations correctly for plots #
astation <- unique(a3paths$Station)
astation
stationabb <- c("BC_joint2", "BC_joint", "Base_TD",
                "Rstr_joint", "Lisbon", "Abv_lisbon", 
                "Levee_marker", "I80_1",
                "Swanston", "Abv_swanston", "Cache_creek", "Knaggs", "Wallace_weir")
identical(sort(astation), sort(stationabb)) #have to match before releveling the factors
a3paths$Station <- factor(a3paths$Station, levels = stationabb) # correctly orders the levels for plots



## @knitr plotpaths
ggplot(a3paths, aes(x = arrival, y = Station, group = TagID)) + geom_line(aes(color = factor(TagID))) + 
  facet_wrap(~year, scales = "free_x") +
  theme(legend.position = "none") + ggtitle("Paths of Ag Crossing #4 Fish")
