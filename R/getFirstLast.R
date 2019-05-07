#' getFirstLast
#'
#' @param df the output of the fishpaths() function: a dataframe that includes of TagIDs, arrival, departure, TagID, and Rkm.
#' @return df with TagID, station of first detection, station of last detection, travel time in days between those two stations, and the distance between the two stations.
#' @details Function selects the first station as that with the earliest time, breaking ties with the lowest river km, and the last station as that with the last time, with the highest river km.
#' @author Myfanwy Johnston
#' @export
getFirstLast <- function(df, tidc2 = "TagID", dtc2 = "DateTimeUTC", stnc2= "Station") {

  do.call(rbind, lapply(split(df, df$TagID), firstlastOneFish, tagidcol = tidc2, datetimecol = dtc2, stationcol = stnc2))

  }
