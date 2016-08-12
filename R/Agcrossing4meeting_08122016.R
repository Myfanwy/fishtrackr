# From Edmund's email, 8/8/16:

# Workplan:

# Plot density distributions of tagged fish throughout the Bypass, by year
# if time, plot density distributions of tagged fish by Julian Day, either within years or not

# Plot or make table of proportions of fish that encounter Ag Crossing #4 that successfully exit the Bypass vs. those that don't.

# Calculate migratory delay for fish that encounter Ag crossing 4

# See if we can bring in any environmental data at all.


#################### Plotting distribution of fish throughout the Bypass: ##########################

# Tally frequency of unique fish at each location across entire season (by year)
library(ybp)
library(ybp)
library(beepr)
devtools::install_github("hadley/ggplot2")
library(ggplot2)
library(dplyr)
f <- all69khz

# --- Run this chunk first if you want to group stations for plots --- #
library(stringr)
# Abv_rstr & rstr (121.04 & 121.01) -> rstr_joint (121.0)
# BCE/W <- BC_joint
# BCE/W2 <- BC2_joint
f$Station <- str_replace(f$Station, 'RSTR', 'Rstr_joint')
f$Station <- str_replace(f$Station, 'Abv_rstr', 'Rstr_joint')
f$Station <- str_replace(f$Station, 'BCE' , 'BC_joint')
f$Station <- str_replace(f$Station, 'BCE2', 'BC2_joint') # stringr replaces it with "BC_joint2", but whatevs
f$Station <- str_replace(f$Station, 'BCW' , 'BC_joint')
f$Station <- str_replace(f$Station, 'BCW2', 'BC2_joint') # stringr replaced it with "BC_joint2", but whatevs
f$Rkm[f$Rkm == 121.04] <- 121 # combine rstr and abv_rstr
f$Rkm[f$Rkm == 121.01] <- 121 # combine rstr and abv_rstr
f$year <- lubridate::year(f$DateTagged)
f <- filter(f, Sp == "chn")
length(unique(f$TagID))
unique(f$Station)
sort(unique(f$Rkm))
str(f)
beep(0)


# Kind of it - need to separate by nfish, though, not ndets
ggplot(f, aes(Rkm, ..count.., fill = Station)) + geom_density(position = "fill") + facet_wrap(~year)

# Group by Station, TagID, and extract every unique value (count of nfish at each station)

d <- f %>% 
  filter(Sp == "chn") %>% 
  group_by(year, Rkm) %>% 
  summarise(nfish = length(unique(TagID)))
dput(d)
head(d)

# line graph to show us the true distribution of number of fish per riverkm
ggplot(d, aes(Rkm, nfish)) + geom_point() + geom_path() + facet_wrap(~year) + labs(x = "River Kilometer \nBeginning at Confluence, ending at Wallace Weir", y = "Number of Unique Fish Detected", title = "Number of Unique Fish Detected by River Kilometer in the Yolo Bypass \n 2012-2016")

# Density graph
library(rethinking)
# within fish, remove duplicated Rkms
d <- f %>% 
  group_by(year, TagID) %>% 
  filter(!duplicated(Rkm)) %>% 
  ungroup()
head(d)
dens(d$Rkm) # all years

# with ggplot2, faceting by year
ggplot(d, aes(Rkm)) + geom_density() + facet_wrap(~year)

## Optional Next step: transform d into a df that represents true density of nfish per year per Rkm/Station: repeat BC_joint 49 times as a row in 2013, etc.


#################### Visualizing proportions of fish that encounter AgCrossing #4 ##########################

fstat <- f %>% 
  group_by(year, Station) %>% 
  summarise(nstat = n())

filter(fstat, Station == "Swanston" | Station == "Abv_swanston") # have data for this station only in 2014 & 2015

d <- f %>% 
  filter(year == 2014 | year == 2015)

# figure out how many fish were detected at or above Swanston Weir

ag4 <- d %>% 
  group_by(year, Station) %>% 
  summarise(nfish = length(unique(TagID))) %>% 
  filter(Station == "Swanston" | Station == "Abv_swanston") # in 2014, 11 fish reached the weir; 9 went over it; in 2015, 4 fish reached the weir and all went over it

ag4plot <- as.data.frame(cbind(year = c(2014, 2015), nfish_encounter = c(11, 4), nfish_continue = c(9, 4),
                               samplesize = c(35, 30)))
ag4plot <- ag4plot %>% 
  mutate(propenc = nfish_encounter/samplesize, propcontinue = nfish_continue/nfish_encounter)
ag4plot

# Plotting proportion of fish encountering Ag4
ggplot(ag4plot, aes(factor(year), propenc, group = 1)) + geom_point(aes(color = factor(year), size= 1), show.legend = FALSE) + geom_path(lty = 2) + scale_y_continuous(limits = c(0, 1)) + labs(x = "Year", y = "Proportion of Tagged Fish", title = "Proportion of tagged fish that encountered Ag Crossing #4 by Year")

d# Proportions 2014:
11/35 # 31% of tagged fish encountered swanston; 
9/35 # 26% of tagged fish continued above Swanston
9/11 # Of those fish that encountered swanston, 82% of them continued

# Proportions 2015:
4/30 # 13% of tagged fish encountered swanston
4/30 # 13% of tagged fish continued above swanston
4/4 # of those fish that encountered swanston, 100% of them continued

# Plotting proportion of fish that continued
ggplot(ag4plot, aes(factor(year), propcontinue, group = 1)) + geom_point(aes(color = factor(year), size= 1), show.legend = FALSE) + geom_path(lty = 2) + scale_y_continuous(limits = c(0, 1)) + labs(x = "Year", y = "Proportion of Fish", title = "Proportion of Fish that Encountered & Passed Above Ag Crossing #4 by Year")


# Of the fish that encountered Ag4, how many successfully exited the Bypass?

dtags <- f %>% 
  group_by(year, TagID) %>% 
  filter(Station == "Swanston" | Station == "Abv_swanston" |
           Station == "Cache_creek" | Station == "Knaggs" | Station == "Wallace_weir") %>% 
  filter(Sp == "chn")
dtags <- unique(dtags$TagID)

d <- f %>% 
  filter(TagID %in% dtags)

length(unique(d$TagID))
identical(sort(dtags), sort(unique(d$TagID))) # k we've got the right fish

head(d)
unique(d$year)

# do one year at a time:
d14 <- filter(d, year == 2014)
# Call splitfishvisits on d14
# source fishpaths.R

##  Building fishpaths dataframe ##
f1 = split(d14, list(d14$TagID, d14$Station)) # splits the dataframe into a list, where each element of the list is every combination of TagID and station, and the detections within that.
# glimpse <- f1[sapply(f1, nrow) == 34] # see how it looks: these are all the fish/station combos with 34 rows

f1 = f1[ sapply(f1, nrow) > 0 ] # filter out the combos that don't actually occur

tmp = lapply(f1, splitFishStationVisits) # apply this function, which adds the grouping variable, then calls on the other function, which acts on the grouping variable

ag14 = do.call(rbind, tmp) # get the whole thing into a df
head(ag14) # we now have the fish paths of those fish that encountered Ag4 in 2014.  Now we can sort on their final detection location

length(unique(ag14$TagID)) # should be 11 fish
ag14 <- arrange(ag14, TagID, arrival)

ag14 <- ag14 %>% 
  group_by(TagID) %>% 
  filter(departure == max(departure)) # final locations of the fish

# crosscheck w/ full detection set
a2 <- filter(f, TagID %in% dtags)
str(a2)

a2 <- a2 %>% 
  filter(Sp == "chn") %>% 
  group_by(TagID) %>% 
  arrange(DateTimeUTC) %>% 
  filter(DateTimeUTC == max(DateTimeUTC))

a2 <- filter(a2, year == 2014)
identical(sort(ag14$Station), sort(a2$Station)) # match; that's good.

ag14 %>% 
  group_by(Station) %>% 
  summarise(nfish = n()) # of the 11 fish that encountered Ag Crossing #4 in 2014, only 1 may have ultimately turned around.

#  note: build function for finding out final detection location for fish that were detected at X station

# Do same for four fish in 2015:
a2 <- filter(f, TagID %in% dtags)
a2 <- a2 %>% 
  filter(Sp == "chn") %>% 
  group_by(TagID) %>% 
  arrange(DateTimeUTC) %>% 
  filter(DateTimeUTC == max(DateTimeUTC))
a2 <- filter(a2, year == 2015)

a2 %>% 
  group_by(Station) %>% 
  summarise(nfish = n()) # of the 4 fish that encountered Ag Crossing #4 in 2015, none made it back down.

# Visualize those fish tracks:

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
head(a3paths)
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

# 2014 Ag crossing fishpaths:
a3paths %>% 
  group_by(year) %>% 
  summarise(mind = min(arrival), maxd = max(departure))

# find the weird point:
a3paths <- a3paths %>% filter(arrival != "2014-05-15 17:34:29")
a3paths[a3paths$arrival == "2014-05-15 17:34:29", ] # don't know how to do this, but it's not that important

save(a3paths, file = "Ag4fishpaths.Rda")
ggplot(a3paths, aes(x = arrival, y = Station, group = TagID)) + geom_line(aes(color = factor(TagID))) + 
  facet_wrap(~year, scales = "free_x") +
  theme(legend.position = "none") + ggtitle("Paths of Ag Crossing #4 Fish")


################ Calculating Migratory Delay for Ag4 Fish: ################################

head(a3paths)
# calculate residence in Bypass for each fish:

test <- filter(a3paths, TagID == 13640)
head(test)
str(test$residence)
test$res2 <- difftime(test$departure, test$arrival, units = "hours")
head(test[1:6, c("arrival", "departure", "residence", "res2")]) # worked; use this one for residence calcs

ggplot(test, aes(x = arrival, y = Station, group = 1)) + geom_line(aes(color = factor(TagID)))

# summarise residence at each station

res <- test %>% 
  arrange(arrival) %>% 
  group_by(Station) %>% 
  summarise(totalres = sum(res2))
str(res)
res
filter(test, Station == "Swanston")
ggplot(res, aes(x = Station, y = as.numeric(totalres))) + geom_boxplot()

# try with whole Ag4 dataset:
a3paths$res2 <- difftime(a3paths$departure, a3paths$arrival, units = "hours")
# filtering out dead tags
a3paths_short <- filter(a3paths, res2 <= 169)
range(a3paths_short$res2)
a3paths_short %>% 
  group_by(TagID, Station) %>% 
  arrange(arrival) %>% 
  summarise(totalres = sum(res2)) %>% 
  ggplot(., aes(Station, as.numeric(totalres))) + geom_boxplot() + scale_y_continuous(limits = c(0, 200))+ geom_point(alpha = 0.2, position = "jitter") + labs(y = "Residence in Hours", title = "Residence Time of Ag4 Fish at Each Monitor in the Yolo Bypass Array")

# now visualize entire tagged population, minus the ag4 crossing fish:
# setup data:
others <- filter(f, !(TagID %in% dtags))
others <- others %>% 
  filter(Sp == "chn") %>% 
  group_by(TagID) %>% 
  arrange(DateTimeUTC)
others <- filter(others, year == 2015 | year == 2014)

f1 = split(others, list(others$TagID, others$Station)) # splits the dataframe into a list, where each element of the list is every combination of TagID and station, and the detections within that.
# glimpse <- f1[sapply(f1, nrow) == 34] # see how it looks: these are all the fish/station combos with 34 rows

f1 = f1[ sapply(f1, nrow) > 0 ] # filter out the combos that don't actually occur

tmp = lapply(f1, splitFishStationVisits) # apply this function, which adds the grouping variable, then calls on the other function, which acts on the grouping variable

op = do.call(rbind, tmp) # get the whole thing into a df
head(op)
op$year <- lubridate::year(op$DateTagged)

# Order Stations correctly for plots #
astation <- unique(op$Station)
astation
stationabb <- c("BC_joint2" ,  "BC_joint"  , "Base_TD" ,
                "Rstr_joint", "Putah_creek", "Lisbon"  ,
                "Abv_lisbon" ,  "Levee_marker",  "I80_1" )
identical(sort(astation), sort(stationabb)) #have to match before releveling the factors
op$Station <- factor(op$Station, levels = stationabb) # correctly orders the levels for plots

# try with whole Ag4 dataset:
op$res2 <- difftime(op$departure, op$arrival, units = "hours")
# filtering out dead tags
op_short <- filter(op, res2 <= 169)
range(op_short$res2)

op_short %>% 
  group_by(TagID, Station) %>% 
  arrange(arrival) %>% 
  summarise(totalres = sum(res2)) %>% 
  ggplot(., aes(Station, as.numeric(totalres))) + geom_boxplot() + scale_y_continuous(limits = c(0, 150)) + geom_point(alpha = 0.2, position = "jitter") + labs(y = "Total Residence in Hours", title = "Residence Time of Non-Ag4 Fish at Each Station in the Yolo Bypass Array")

# K - this is a good first look; need to figure out how to visualize residence/travel time directly.

# comparing total residence time (filtered for shed tags)

# all non-ag4 fish: 
d <- op_short %>% 
  group_by(year, TagID, Station) %>% 
  arrange(arrival) %>% 
  summarise(totalres = sum(res2))
d <- d %>% 
  group_by(year, TagID) %>% 
  summarise(total = sum(totalres))
d$fishcat <- "NonAg4"

# Ag4 fish:
d2 <- a3paths_short %>% 
  group_by(year, TagID, Station) %>% 
  arrange(arrival) %>% 
  summarise(totalres = sum(res2))
d2 <- d2 %>% 
  group_by(year, TagID) %>% 
  summarise(total = sum(totalres))
d2$fishcat <- "Ag4"

d3 <- full_join(d, d2)
head(d3) # d3 is now a table of residence times for Ag4 fish and nonAg4 fish in each year; can compare residences

d3$total <- as.numeric(d3$total)
smeans <- d3 %>% 
  group_by(fishcat, year) %>% 
  summarise(mean = mean(total), sd = sd(total))
d3
ggplot(d3, aes(x = fishcat, y = total)) + geom_boxplot(alpha = 0.3) + geom_point(alpha = 0.7, aes(color = fishcat), position = "jitter") + facet_grid(~year) +
  labs(x = "Ag4 Encounter Status", y = "Residence Time in Yolo Bypass (hrs)", title = "Yolo Bypass Residence Time Between Ag4 Crossing Fish and Non-Ag4 Crossing Fish") 

m14 <- smeans$mean[1] - smeans$mean[3] # difference in sample pop means 2014
m15 <- smeans$mean[2] - smeans$mean[4] # difference in sample pop means 2015

# piping in a t-test:
library(broom)
d3 %>% 
  group_by(year) %>% 
  do(tidy(t.test(total~fishcat, data = .))) # differences are slightly significant in 2014, but that was a weird year, as we saw. # also this test is not valid because nON-NORMAL sampling distributions

# Bootstrapped CI intervals:
library(bootES)

b14 <- filter(d3, year == 2014)
bootES(data = b14, data.col = "total", group.col = "fishcat", contrast = c("Ag4", "NonAg4")) # there does seem to be a difference between Ag4 fish residence and nonAg4 fish residence.

b15 <- filter(d3, year == 2015)
bootES(data = b15, data.col = "total", group.col = "fishcat", contrast = c("Ag4", "NonAg4")) # again, a difference, but the data are so incredibly variable that the CI interval is HUGE.  We're 95% confident that the difference between the true population means is between 242 and 9?  Gee, thanks.

# Plot densities
ggplot(d3, aes(total)) + geom_density() + facet_grid(~fishcat + year) + labs(x = "Residence Time in Yolo Bypass (hrs)", title = "Density Plots of Yolo Bypass Residence between Ag4 Crossing Fish and Non-Ag4 Crossing Fish")
# 2014 was a weird year; apparently got a lot of fish with abnormally short residences.  Even so, looks like there's a much longer tail on fish that encounter Ag Crossing 4, which makes sense since they continue upstream and spend more time overall.

## Out of curiosity: average residence time at barriers for total fish population:

# rebuild fishpaths
head(fishpaths)
str(fishpaths)
fishpaths$res2 <- difftime(fishpaths$departure, fishpaths$arrival, units = "hours")
fishpaths$year <- lubridate::year(fishpaths$DateTagged)

d <- fishpaths %>% 
  filter(res2 <= 169, Station != "I80_2", Station != "I80_5", Station != "I80_6", Station != "I80_7") %>% 
  group_by(year, TagID, Station) %>% 
  summarise(meanres = mean(as.numeric(res2)))
head(d)

d <- d %>% 
  group_by(year, Station) %>% 
  summarise(meanres = mean(meanres))

ggplot(d, aes(x = Station, y = meanres)) + geom_boxplot() + facet_wrap(~year) 
# gets complicated because I haven't get differentiated between the fish that shed their tags and the fish that genuinely sat at a monitor before moving elsewhere.  Dont' know how to do taht yet.

# Environmental Data: not going to happen.  Best I could do was separate all the results for you by year.
