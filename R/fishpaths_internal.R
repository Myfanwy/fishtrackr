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
  function(d, TimeThreshold = Threshold, rowFunc = redRowFun, dtc2 = Datetime_col)
  {
    d = d[order(d[[dtc2]]), ] #order dataframe by DateTimeUTC
    g = cumsum( c(0, diff(d[[dtc2]])) > TimeThreshold )
    ans = by(d, g, rowFunc, dtc1 = dtc2) # apply redRowFun by the grouping variable g to the dataframe
    do.call(rbind, ans) # bind that into a dataframe
  }
#-------------------------------------------------------#
