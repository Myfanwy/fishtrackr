#' fishpaths
#'
#' @param x a dataframe of detections
#' @param TagID_col column containing unique fish identification codes
#' @param Station_col column containing unique station codes or names
#' @param Datetime_col column containing date and time of detection, in POSIXct format ("%Y-%M-%D %H:%M:%S")
#' @param Threshold desired time threshold between station visits, in seconds.  See details
#'
#' \details{
#'The time threshold allows you to delineate the period of time that detections can be separated from each other at a receiver and still be considered part of the same "stay" at that receiver.  The default is 1 hour (`60*60`).  If you set Threhold = `60*60*2`, that means that after a fish arrives at a receiver, all detections that occur at that receiver within 2 hours of the first arrival are considered part of the same "stay" at that receiver.}
#'
#'
#' @return dataframe with fishpaths for each tagID
#' @export
#'
#' @examples
#' # Test
#' library(ybp)
#' f <- all69khz_grouped
#' fishpaths(f, f$TagID, f$Station, "DateTimePST", 60*60*2) # time threshold of 2 hours

fishpaths <- function(x, TagID_col, Station_col, Datetime_col="DateTimeUTC", Threshold = 60*60) {

  f1 <- split(x, list(TagID_col, Station_col))
  f1 <- f1[ sapply(f1, nrow) > 0 ]
  tmp = lapply(f1, splitFishStationVisits, dtc2 = Datetime_col, TimeThreshold = Threshold)
  fishpaths = do.call(rbind, tmp)
}
