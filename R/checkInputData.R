#' checkInputData
#'
#' @param dTagID TagID column of your starting dataframe of TagIDs, detections, and river kilometers.
#' @return nothing, if it's all good. If not, throws an error for the first column that's not good.
#' @details function author: Myfanwy Johnston
#' @export

checkInputData = function(dTagID, dDets, dStation, dRkm)
{
  expect_is(dTagID, "numeric")
  expect_is(dDets, "POSIXct")
  expect_is(dStation, "character")
  expect_is(dRkm, "numeric")
}
