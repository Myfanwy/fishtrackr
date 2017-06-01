#' getReachMovements
#'
#' @param x a dataframe that is the output of the fishpaths() function
#' @return a dataframe with the TagID, departure station, arrival station, and travel time in days
#' @details function author: Matt Espe
#' @export

getReachMovements = function(df)
{
    do.call(rbind, lapply(split(df, df$TagID), getReachOneFish))
}
