redRowFun =
  function(d, dtc1)
  {
    r = as.POSIXct(range(d[[dtc1]]))
    data.frame(d[1,],
               arrival = r[1],
               departure = r[2],
               stringsAsFactors = FALSE)
  }
#-------------------------------------------------------#

splitFishStationVisits =
  function(d, TimeThreshold = 60*60, rowFunc = redRowFun, dtc2 = Datetime_col)
  {
    d = d[order(d[[dtc2]]), ] #order dataframe by DateTimeUTC
    g = cumsum( c(0, diff(d[[dtc2]])) > TimeThreshold )
    ans = by(d, g, rowFunc, dtc1 = dtc2) # apply redRowFun by the grouping variable g to the dataframe
    do.call(rbind, ans) # bind that into a dataframe
  }
#-------------------------------------------------------#

fishpaths <- function(x, TagID_col, Station_col, Datetime_col="DateTimeUTC") {

  f1 <- split(x, list(TagID_col, Station_col))
  f1 <- f1[ sapply(f1, nrow) > 0 ]
  tmp = lapply(f1, splitFishStationVisits, dtc2 = Datetime_col)
  fishpaths = do.call(rbind, tmp)

}
#-------------------------------------------------------#

#test
d = readRDS("data/onetag.rds")
d2 = readRDS("data/alltags.rds")

f1 <- fishpaths(d, d$TagID, d$Station, "DateTimePST") # error in (function (..., deparse.level = 1)  : cannot coerce type 'closure' to vector of type 'list' ... debug shows it's when the output of splitFishStationVisits gets handed to fishpaths()... thinks the output of splitFishStationVisits is a function or list of functions...?
head(f1)
f2 <- fishpaths(d2, d2$TagID, d2$Station, "DateTimePST")
head(f2)
