#' getFirstLast
#'
#' @param df the output of the fishpaths() function: a dataframe that includes of TagIDs, arrival, departure, TagID, and Rkm.
#' @return df with TagID, station of first detection, station of last detection, travel time in days between those two stations, and the distance between the two stations.
#' @details function author: Myfanwy Johnston
#' @export


getFirstLast <- function(df) {

  do.call(rbind, lapply(split(df, df$TagID), firstlastOneFish))

  }
