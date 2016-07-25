# visualizing a fishtrack

library(dplyr)
library(ggplot2)
test <- filter(fishpaths, TagID == 13642)
test$residencenum <- as.numeric(test$residence)

ggplot(test, aes(x = Station, y = residencenum)) + geom_point()
range(test$residencenum)
summary(test$residencenum)

range(test$arrival)
range(test$departure)
rng = as.Date(c("2014-10-01", "2014-11-30"))

ggplot(test, aes(y = Station)) + scale_x_date(limits = as.Date(c("2014-10-01", "2014-11-30"))) + geom_path(aes(x = residence))


library(ggplot2)
library(reshape2)    
tasks <- c("Review literature", "Mung data")

mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))
mdfr
mdfr$value2<-as.POSIXct(strptime(mdfr$value, "%d/%m/%Y %H:%M:%S"))
mdfr$date<-as.factor(as.Date(strptime(mdfr$value, "%d/%m/%Y %H:%M:%S")))
ggplot(mdfr, aes(value2, name)) + 
  geom_line(size = 6) +
  xlab("") + ylab("") +
  theme_bw() + facet_grid(~date,scales="free_x",space="free_x")

md <- melt(test, measure.vars = c("arrival", "departure"))
head(md)
str(md$value)
ggplot(md, aes(value, Station)) + geom_line() + facet_grid(~Station)
