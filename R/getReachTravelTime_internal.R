# Isolating Travel Time By Reach


# Start with the reach you want, then calculate travel time within that reach for each TagID

########################################################
# Filter Reach fxn

filterReach <- function(df, station1, station2) {

  # df = y12
  # station1 = "BC_joint"
  # station2 = "Chipps"

  a <- df[df$Station == station1, ] # create a dataframe with only the first station's detections
  b <- df[df$Station == station2, ] # create a dataframe with only the second station's detections

  int_ab <- intersect(unique(a$TagID), unique(b$TagID)) # find the fish that are detected at both stations

  dets = df[df$TagID %in% int_ab & df$Station == station1 |
              df$TagID %in% int_ab & df$Station == station2, ] # subset the master dataframe for those fish and detections

  stopifnot(identical(length(unique(dets$TagID)), length(int_ab))) # make sure you get those and only those fish detected at both stations
  return(dets) # return the filtered dataframe

}

#fxn testing
#y12 <- filter(d, TagGroup == "Yolo Bypass 2012")
#a <- filterReach(y12, "Lisbon", "Rstr")
#y12Release_I80 <- filterReach(y12, "Release", "I80_1")

########################################################
# Calc transit time for a single Tag; later gets called by getReachTravelTime

# a <- filter(d, TagID == 2051) fxn testing

reachTransit <-  function(a){  # where a is a dataframe of a single reach, multiple tagids; the return df of filterReach()

  a = a[order(a$arrival),] # Sort each sub-group by 'arrival'
  a = a[!duplicated(a$Station),] # Retain only the first appearances of 'arrival'
  a = a[order(a$arrival),] # Sort each sub-group by 'arrival'

  b = as.data.frame(
    cbind(TagID = as.numeric(a$TagID[1]), # obtain TagID, station, and ttime of the sub-group,
          Station1 = a$Station[1],
          Station2 = a$Station[NROW(a)],
          ttime = (as.numeric(as.POSIXct(a$arrival[NROW(a)])) -
                     as.numeric(a$releasetime))/(60*60*24)) # get travel time in days
  )

  b = b[!duplicated(b$TagID), ]
  return(b)

}


