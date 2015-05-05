library(dplyr)
tags <- tbl_df(read.csv("All_Tags.csv", header=T, stringsAsFactors=F))
tags <- select(tags, -Species, -Researcher, -Code_Space, -Collection_Method)
tags
library(chron)
dates <- as.chron(tags$DateTagged) ## convert @##$$%-ing Dates to a #@$#@%#-ing Date
head(dates)
dy <- years(dates)
head(dy)
tags$TagDate <- dates
class(tags$TagDate)
tags$TagDate <- as.Date(tags$TagDate)
tags <- select(tags, -DateTagged)
tags
tags$year <- dy
tags

wst <- filter(tags, Sp=="wst")
chn <- filter(tags, Sp=="chn")
chn$FL
chn$FL <- chn$FL/10

wst2012 <- filter(wst, year=="2012")
wst2014 <- filter(wst, year=="2014")
chn2012 <- filter(chn, year=="2012")
chn2013 <- filter(chn, year=="2013")

mean(wst$FL)
mean(chn$FL)

head(wst2014)

t1 <- t.test(wst2012$FL, var.equal=F)
t2 <- t.test(wst2012$FL, wst2014$FL)
t2
t3 <- t.test(chn2012$FL, chn2013$FL)
t3
t4 <- t.test(chn$FL)
t4

wst$FL

t5 <- t.test(chn$FL, wst$FL)
t5

chn2012$FL
chn2013$FL
