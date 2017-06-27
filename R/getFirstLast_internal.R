#' getFirstLast_internal; called by getFirstLast

firstlastOneFish <- function(x) {

  x = x[order(x$arrival), ]

  FirstRow = x[x$arrival == min(x$departure), ]
  FirstRow = FirstRow[FirstRow$Rkm == min(FirstRow$Rkm), ]
  LastRow = x[x$arrival == max(x$arrival), ]
  LastRow = LastRow[LastRow$Rkm == max(LastRow$Rkm), ]

    data.frame(
    TagID = x$TagID[1],

    FirstStation = FirstRow$Station,
    LastStation = LastRow$Station,

    ttime = (as.numeric(LastRow$arrival) - as.numeric(FirstRow$departure))/(60*60*24),

    reachdistance = FirstRow$Rkm - LastRow$Rkm,

    stringsAsFactors = FALSE
  )

}
