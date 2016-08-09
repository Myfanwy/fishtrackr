library(dplyr)
library(ggplot2)
library(lubridate)
fishpaths <- filter(fishpaths, Sp == "chn")
fishpaths$year <- lubridate::year(fishpaths$DateTagged)

allt <- as.data.frame(alltags)
names(allt)
allt$year <- lubridate::year(allt$DateTagged)
allt$Tagging_Location <- str_replace(allt$Tagging_Location, 'Fyke', 'FYKE')
allt <- select(allt, TagID, Tagging_Location, DateTagged, year, Sp, `Tagging Group`)
allt

# visualizing tagging locations across years
allt %>% 
  group_by(year, Tagging_Location) %>% 
  filter(`Tagging Group` == "fca_2014" | `Tagging Group` == "fca_2013" | `Tagging Group` == "fca_2012" | `Tagging Group` == "fca_2015") %>% 
  summarise(count = n()) %>% 
  ggplot(., aes(Tagging_Location, count)) + geom_bar(aes(fill = Tagging_Location), stat = "identity") + facet_wrap(~year)
allt

tagsum <- allt %>% 
  filter(`Tagging Group` == "fca_2014" | `Tagging Group` == "fca_2013" | `Tagging Group` == "fca_2012" | `Tagging Group` == "fca_2015") %>% 
  group_by(year) %>% 
  mutate(total = n()) %>% 
  ungroup() %>% 
  group_by(year, Tagging_Location) %>% 
  summarise(CountTagged = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(TotalTaggedYear = sum(CountTagged))
tagsum

  # determine whether fish are detected upstream of tagging location

# add riverkm to tagging location
taglockey <- as.data.frame(cbind(Tagging_Location = c("BRSTR", "FYKE", "LIS", "Abv_LIS", "WW"), TagLocRkm = c(117, 133, 134, 136, 165)))
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

# summary of fish that go above lisbon by tagging location: all years pooled
fpsumm <- fp %>% 
  group_by(Tagging_Location, TagID) %>% 
  filter(!duplicated(TagID)) %>% 
  summarise(upstream = sum(maxrkm > TagLocRkm), downstream = sum(maxrkm <= TagLocRkm), 
            abvlis = sum(maxrkm >= 136)) %>%  # gives binomial count by TagID
  summarise(us = sum(upstream), ds = sum(downstream), abvlis = sum(abvlis), total = sum(us, ds)) #gives final count
fpsumm

# find out proportions of fish detected abv lisbon by tag date
# redo with Julian Day ##
names(fp)
tagdate <- select(fp, TagID, Rkm, DateTagged, year, Tagging_Location, TagLocRkm, CountTagged, TotalTaggedYear, maxrkm)
tagdate <- tagdate %>% 
  group_by(year, Tagging_Location) %>% 
  filter(!duplicated(TagID))
head(tagdate)
length(unique(tagdate$TagID))

unique(tagdate$maxrkm)

# Convert DateTagged to usable julian days with a for-loop
tagdate$jd <- rep("NA", length.out = length(tagdate$DateTagged))

  for( i in 1:length(tagdate$jd)) {
  if(tagdate$year[i] == 2012) {
  tagdate$jd[i] <- julian(tagdate$DateTagged[i], origin = as.Date("2012-01-01")) 
} else if(tagdate$year[i] == 2013) {
 tagdate$jd[i] <- julian(tagdate$DateTagged[i], origin = as.Date("2013-01-01")) 
} else if(tagdate$year[i] == 2014) {
  tagdate$jd[i] <- julian(tagdate$DateTagged[i], origin = as.Date("2014-01-01"))
} else { tagdate$jd[i] <- julian(tagdate$DateTagged[i], origin = as.Date("2015-01-01"))}
                        }
tagdate$jd <- as.numeric(tagdate$jd)
julian(as.Date("2012-10-20"), origin = as.Date("2012-01-01")) # 2012 was a leap year
julian(as.Date("2013-10-20"), origin = as.Date("2013-01-01"))
julian(as.Date("2014-10-20"), origin = as.Date("2014-01-01"))
julian(as.Date("2015-10-20"), origin = as.Date("2015-01-01"))

tail(tagdate)

# facet line plot by early/late tagged fish

tagdate$abv_lis <- ifelse(tagdate$maxrkm >= 136, 1, 0)
tagdate$timing <- ifelse(tagdate$jd <= 293, "early", "late")
head(tagdate)

options(digits = 2)
pp <- tagdate %>% 
  filter(year != 2012, Tagging_Location != "Abv_LIS" & Tagging_Location != "WW") %>% 
    group_by(timing, Tagging_Location, year) %>% 
    summarise(abvlis = sum(abv_lis), CountTimingTagged = n(), propAbvlis = abvlis/CountTimingTagged)
pp
str(pp)

# append rows for missing values
options(digits = 2)
pp[nrow(pp) + 1, ] <- c("early", "LIS", 2014, 0, 0, 0.00)
pp[nrow(pp) + 1, ] <- c("late", "FYKE", 2014, 0, 0, 0.00)
pp <- arrange(pp, timing, Tagging_Location, year)
pp$propAbvlis <- round(as.numeric(pp$propAbvlis), digits = 2)

pp <- as.data.frame(pp)
# pp <- filter(pp, Tagging_Location != "FYKE") # for a cleaner graph
pp$propdivide <- as.character(paste(pp$abvlis, pp$CountTimingTagged, sep = "/"))
pp

ll3 <- ggplot(pp, aes(x = factor(year), y = propAbvlis, group = Tagging_Location)) + geom_point(shape=1, size=3) +
  geom_path(aes(color = Tagging_Location), size = 1.5, linejoin = "bevel") + facet_wrap(~timing, labeller = label_both)
ll3 + geom_text(aes(label = propdivide, fontface = "bold", vjust = -1), color = "gray20")

# put into write-up
dput(pp)

# spotcheck
filter(tagdate, year == 2013 & timing == "early" & Tagging_Location == "FYKE")

# linechart (much better (trends over time))

ll <- ggplot(pp, aes(x = factor(year), y = prop, group = Tagging_Location)) + geom_line(aes(color = Tagging_Location), size = 3)
ll

ll2 <- ggplot(pp, aes(x = factor(year), y = prop, group = Tagging_Location)) + geom_line(aes(color = Tagging_Location), size = 1.5) + facet_wrap(~timing, labeller = label_both) + geom_label(aes(label = prop))

ll2 + labs(x = "Tagging Season", y = "Proportion of Fish Tagged that were \n Detected Above Lisbon Weir", title = "Proportion of Fish that continued above Lisbon Weir \nby Tag Location, Year, and Tag Date")

## Make the same plot, but for numbers sampled there

