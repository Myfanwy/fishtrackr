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

head(d)

# line graph to show us the true distribution of number of fish per riverkm
ggplot(d, aes(Rkm, nfish)) + geom_point() + geom_path() + facet_wrap(~year) 

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
ggplot(ag4plot, aes(factor(year), propcontinue, group = 1)) + geom_point(aes(color = factor(year), size= 1), show.legend = FALSE) + geom_path(lty = 2) + scale_y_continuous(limits = c(0, 1)) + labs(x = "Year", y = "Proportion of Fish", title = "Proportion of Fish that Encountered & Passed above Ag Crossing #4 by Year")


# Of the fish that encountered Ag4, how many successfully exited the Bypass?

dtags <- f %>% 
  group_by(year, TagID) %>% 
  filter(Station == "Swanston")
dtags <- unique(dtags$TagID)

d <- f %>% 
  filter(TagID %in% dtags)

length(unique(d$TagID))
identical(sort(dtags), sort(unique(d$TagID))) # k we've got the right fish

head(d)
d <- filter(d, Sp == "chn")
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

ag14 %>% 
  group_by(TagID) %>% 
  filter(departure == max(departure))
