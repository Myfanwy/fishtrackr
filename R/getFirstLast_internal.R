#' getFirstLast_internal; called by getFirstLast
#'

firstlastOneFish <- function(x) {

  x = x[order(x$arrival), ]

  data.frame(
    TagID = x$TagID[1],

    FirstStation = x$Station[x$departure == min(x$departure)],

    LastStation = x$Station[x$arrival == max(x$arrival)],

    ttime = (as.numeric(x$arrival[x$arrival == max(x$arrival)]) - as.numeric(x$departure[x$departure == min(x$departure)]))/(60*60*24),

    reachdistance = x$Rkm[x$departure == min(x$departure)] -
      x$Rkm[x$arrival == max(x$arrival)],

    stringsAsFactors = FALSE
  )

}
