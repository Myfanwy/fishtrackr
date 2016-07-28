library(dplyr)
library(ggplot2)
fishpaths <- filter(fishpaths, Sp == "chn")
fishpaths$year <- lubridate::year(fishpaths$DateTagged)

allt <- as.data.frame(alltags)
names(allt)
allt$year <- lubridate::year(allt$DateTagged)
allt$Tagging_Location <- str_replace(allt$Tagging_Location, 'Fyke', 'FYKE')
allt <- select(allt, TagID, Tagging_Location, DateTagged, year, Sp, `Tagging Group`)

# visualizing tagging locations across years
allt %>% 
  group_by(year, Tagging_Location) %>% 
  filter(`Tagging Group` == "fca_2014" | `Tagging Group` == "fca_2013" | `Tagging Group` == "fca_2012" | `Tagging Group` == "fca_2015") %>% 
  summarise(count = n()) %>% 
  ggplot(., aes(Tagging_Location, count)) + geom_bar(aes(fill = Tagging_Location), stat = "identity") + facet_wrap(~year)

tagsum <- allt %>% 
  filter(`Tagging Group` == "fca_2014" | `Tagging Group` == "fca_2013" | `Tagging Group` == "fca_2012" | `Tagging Group` == "fca_2015") %>% 
  group_by(year) %>% 
  mutate(total = n()) %>% 
  ungroup() %>% 
  group_by(year, Tagging_Location) %>% 
  summarise(CountTagged = n(), TotalTaggedYear = unique(total), PropTaggedYearLoc = (CountTagged/TotalTaggedYear)*100) # TotalTaggedYear only works b/c sample sizes are different every year
tagsum

  # determine whether fish are detected upstream of tagging location

# add riverkm to tagging location
taglockey <- as.data.frame(cbind(Tagging_Location = c("BRSTR", "FYKE", "LIS", "Abv_LIS", "WW"), TagLocRkm = c(117, 134, 134, 136, 165)))
taglockey

allt <- inner_join(allt, taglockey, by = "Tagging_Location")
head(allt)
allt <- filter(allt, Sp == 'chn')
unique(allt$`Tagging Group`)

# add tagsum info:
tagsum
head(allt)
tagsum2 <- inner_join(allt, tagsum, by = c("year", "Tagging_Location"))
head(tagsum2)
length(unique(tagsum2$TagID))

fp <- inner_join(fishpaths, tagsum2) # get count, tagLocRkm, and TotalTaggedYear in there

length(unique(fp$TagID))
(unique(fp$`Tagging Group`))

head(fp)
fp$TagLocRkm <- as.numeric(as.character(fp$TagLocRkm)) # convert the tagging location riverkm from factor to numeric
length(unique(fp$TagID)) #all fca, including WW

# add max rkm reached for each fish
fp <- fp %>% 
  group_by(TagID) %>% 
  mutate(maxrkm = max(Rkm))

# summary of fish that go above lisbon by tagging location
fpsumm <- fp %>% 
  filter(!duplicated(TagID)) %>% 
  group_by(Tagging_Location) %>% 
  summarise(upstream = sum(maxrkm > TagLocRkm), downstream = sum(maxrkm <= TagLocRkm), 
            abvlis = sum(maxrkm >= 136)) %>%  # gives binomial count by TagID
  summarise(us = sum(upstream), ds = sum(downstream), abvlis = sum(abvlis), total = sum(us, ds)) #gives final count
fpsumm

118/129 # over 90% of tagged fish go upstream of their tagging location
11/129 # only ~9% do not ever go upstream of their tagging location
48/129 # about 37% of tagged fish are detected upstream of lisbon weir; that's including the ones that were tagged AbvLis, though

# breakdown by taglocation and year; plot proportions (barplot?), facet by year
options(digits = 4)
pp <- fp %>% 
  filter(!duplicated(TagID), year != 2012, Tagging_Location != "Abv_LIS" & Tagging_Location != "WW") %>% 
  group_by(year, Tagging_Location) %>% 
  summarise(totaltagged = n(), abvlis = sum(maxrkm >= 136), prop = round((abvlis/totaltagged), 2)) 
pp

# grouped Barplot (terrible - for exercise purposes only)
p2 <- ggplot(pp, aes(x = Tagging_Location, y = abvlis)) + 
  geom_bar(stat = "identity", aes(fill = Tagging_Location, label = prop)) + 
  facet_wrap(~year) + labs(x = "Tagging Location", y = "Number of Fish Detected Above Lisbon Weir", title = "Number of fish that continued above Lisbon Weir by Tag Location and Year; \nlabels represent proportion of total fish tagged at that location") + scale_y_continuous(breaks = c(1:10), minor_breaks = 1)

p2 + geom_label(aes(label = pp$prop))

# find out proportions of fish detected abv lisbon by tag date
# redo with Julian Day
names(fp)
tagdate <- select(fp, TagID, Rkm, DateTagged, year, Tagging_Location, TagLocRkm, CountTagged, TotalTaggedYear, PropTaggedYearLoc, maxrkm)
tagdate <- filter(tagdate, !duplicated(TagID))
unique(tagdate$maxrkm)
tagdate$DateTaggedAbb <- as.POSIXct(tagdate$DateTagged, format = "%Y-%m-%d")
tagdate$m <- lubridate::month(tagdate$DateTaggedAbb)
tagdate$d <- lubridate::day(tagdate$DateTaggedAbb)
tagdate$dm <- paste(tagdate$m, tagdate$d, sep = "-")
class(tagdate$dm)
tagdate$dm <- as.POSIXlt(tagdate$dm, format = "%m-%d")
tail(tagdate)
tagdate <- select(tagdate, -DateTaggedAbb, -m, -d)

tagdate$dm[1:6]

# facet line plot by early/late tagged fish

pp <- fp %>% 
  filter(!duplicated(TagID), year != 2012, Tagging_Location != "Abv_LIS" & Tagging_Location != "WW") %>% 
  group_by(year, Tagging_Location) %>% 
  summarise(totaltagged = n(), abvlis = sum(maxrkm >= 136), prop = round((abvlis/totaltagged), 2)) 
pp

# linechart (much better (trends over time))

ll <- ggplot(pp, aes(x = factor(year), y = prop, group = Tagging_Location)) + geom_line(aes(color = Tagging_Location), size = 3)
ll

head(tagdate)
unique(tagdate$Tagging_Location) # have to get abv_lis & ww out

timingplot <- tagdate %>% 
  filter(year != 2012, Tagging_Location != "Abv_LIS" & Tagging_Location != "WW") %>% 
  group_by(TagID) %>% 
  mutate(timing = ifelse(dm < "2016-10-20", "early", "late")) %>% 
  ungroup() %>% 
  group_by(year, Tagging_Location, timing) %>% 
  summarise(subtotal = n(), abvlis = sum(maxrkm >=136), prop = round((abvlis/subtotal), 2))
timingplot

ll2 <- ggplot(timingplot, aes(x = factor(year), y = prop, group = Tagging_Location)) + geom_line(aes(color = Tagging_Location), size = 1.5) + facet_wrap(~timing, labeller = label_both) + geom_label(aes(label = prop))

ll2 + labs(x = "Tagging Season", y = "Proportion of Fish Tagged that were \n Detected Above Lisbon Weir", title = "Proportion of Fish that continued above Lisbon Weir \nby Tag Location, Year, and Tag Date")

dput(timingplot)
