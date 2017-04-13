#' getReachMovements
#'
#' @param x a dataframe that is the output of the fishpaths() function
#' @return a dataframe with the TagID, departure station, arrival station, and travel time in days
#' @details function author: Matt Espe
#' @export

getReachOneFish = function(x)
{
    x = x[order(x$arrival),]
    # difference between departure 1...n-1 and arrival 2...n
    ttime = as.numeric(x$arrival[-1]) - as.numeric(x$departure[-nrow(x)])
    between_stns = x$Station[-nrow(x)] != x$Station[-1]
    data.frame(depStation = x$Station[-nrow(x)][between_stns],
               arrStation = x$Station[-1][between_stns],
               ttime = ttime[between_stns]/ (60*60*24),
               TagID = x$TagID[1],
               stringsAsFactors = FALSE)
}

getReachMovements = function(df)
{
    do.call(rbind, lapply(split(df, df$TagID), getReachOneFish))
}
