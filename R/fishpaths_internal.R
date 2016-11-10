splitFishStationVisits =
  function(d, TimeThreshold = 60*60, rowFunc = redRowFun) # where d is
  {
    #   if(nrow(d) == 0)
    #      return(data.frame(Station = character(), ....))
    d = d[order(d$DateTimeUTC), ] #order dataframe by DateTimeUTC
    g = cumsum( c(0, diff(d$DateTimeUTC)) > TimeThreshold )
    ans = by(d, g, rowFunc) # apply redRowFun by the grouping variable g to the dataframe
    do.call(rbind, ans) # bind that into a dataframe
  }



redRowFun = # takes a list that has been separated by fish and station and outputs a new dataframe with the following columns, via an index.  In our case, each element of the list is a TagID/Station combo.
  function(d)
  {
    r = as.POSIXct(range(d$DateTimeUTC))
    data.frame(d[1,],
               arrival = r[1],
               departure = r[2],
               stringsAsFactors = FALSE)
  }

