#' getReachTravelTime
#'
#' A function to calculate the time (in days) between detections at the top and bottom of a reach.
#' @param df dataframe of detections
#' @param s1 station at the top of the reach, in quotes (must match the name of the station in df)
#' @param s2 station at the bottom of the reach, in quotes (must match the name of the station in df)
#' @return a dataframe with the TagID, travel time in days (column "tt"), Station1, Station2, and a generated "reachname" column.
#' @export
#'
getReachTravelTime <- function(df, s1, s2) {

  # df = y12
  # s1 = "Release"
  # s2 = "I80_1"

  x = filterReach(df, station1 = s1, station2 = s2)

  aa = do.call(rbind, lapply(split(x, x$TagID), reachTransit))

  aa$reachname <- paste0(s1, s2)
  return(aa)
}
