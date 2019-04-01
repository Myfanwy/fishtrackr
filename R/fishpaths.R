#' fishpaths
#'
#' @param x a dataframe of detections
#' @param TagID_col must be called "TagID"
#' @param Station_col must be called "Station"
#'
#' @return dataframe with fishpaths for each tagID
#' @export
#'
#' @examples
#' # Test
#' library(ybp)
#' f <- all69khz_grouped
#' fishpaths(f, f$TagID, f$Station)

fishpaths <- function(x, TagID_col, Station_col, Datetime_col="DateTimeUTC") {

  f1 <- split(x, list(TagID_col, Station_col))
  f1 <- f1[ sapply(f1, nrow) > 0 ]
  tmp = lapply(f1, splitFishStationVisits, dtc2 = Datetime_col)
  fishpaths = do.call(rbind, tmp)
}
