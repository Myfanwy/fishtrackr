
# source("fishpaths.R")
library(ybp)
f <- all69khz
f1 = split(f, list(f$TagID, f$Station)) # splits the dataframe into a list, where each element of the list is every combination of TagID and station, and the detections within that.
summary(sapply(f1, nrow)) # most fish/station combos have around 194 detections before filtering the ones with none
glimpse <- f1[sapply(f1, nrow) == 34] # see how it looks: these are all the fish/station combos with 34 rows

# what happens if we just call redRowFun on this, and nothing else?
cur <- redRowFun(glimpse) # doesn't work because we haven't added a grouping variable to the bouts yet

f1 = f1[ sapply(f1, nrow) > 0 ] # filter out the combos that don't actually occur
summary(sapply(f1, nrow)) # most fish/station combos have around 608 detections


tmp = lapply(f1, splitFishStationVisits) # apply this function, which adds the grouping variable, then calls on the other function, which acts on the grouping variable

tmp[3]

fishpaths = do.call(rbind, tmp) # get the whole thing into a df


head(fishpaths)

tmp2 = lapply(f1, splitFishStationVisits, rowFunc = myRowFun, TimeThreshold = 60*20)
tmp3 = do.call(rbind, tmp2)

