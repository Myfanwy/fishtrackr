#' fishpaths
#'
#' @param x a dataframe of detections
#' @param TagID_col must be called "TagID"
#' @param Station_col must be called "Station"
#' @details For current version of function, the detection column of dates and times must be called DateTimeUTC, and must be in POSIXct "%Y-%m-%d %H:%M:%S" format.  Any other columns are optional, but will be carried over to the final dataframe.
#'
#' @return dataframe with fishpaths for each tagID
#' @export
#'
#' @examples
#' # Test
#' library(ybp)
#' f <- all69khz_grouped
#' fishpaths(f, f$TagID, f$Station)

fishpaths <- function(x, TagID_col, Station_col) {

  f1 <- split(x, list(TagID_col, Station_col)) #splits the dataframe into a list, where each element of the list is every combination of TagID and station, and the detections within that.

  f1 <- f1[ sapply(f1, nrow) > 0 ] # filter out the combos that don't actually occur

  tmp = lapply(f1, splitFishStationVisits) # apply this function, which adds the grouping variable, then calls on the other function, which acts on the grouping variable

  fishpaths = do.call(rbind, tmp) # get the whole thing into a df

}
