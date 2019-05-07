#' getFirstLast_internal; called by getFirstLast

# goal: function for extracting the first detection of a fish, and the first detection at its last location, so that the travel time can then be easily calculated

# quality checks:
# - what happens if there's only one detection (i.e fish is never detected again after release?)?
# - No detections?
# - there's only one detection at the final location?
# - final location is gated (what's the tie-breaker mechanism?)

# method: arrange each fish by DateTime
# assign an rleid to each encounter
# extract the minimum detection of the minimum rleid, and the minimum detection of the maximum rleid
# put together in a tidy dataframe

firstlastOneFish <- function(x, tagidcol, datetimecol = dtc2, stationcol = stnc2) {

  x = x[order(x[[datetimecol]]), ]
  x$rleidcol = data.table::rleid(x[[stationcol]])

  first_det = x[x[[datetimecol]] == min(x[[datetimecol]]), ]
  intermed = x[x$rleidcol == max(x$rleidcol), ]

  first_det_last_stn = intermed[intermed[[datetimecol]] == min(intermed[[datetimecol]]), ]

    data.frame(
    TagID = as.numeric(unique(x[[tagidcol]])),

 #{{   insert check that str(first_det) and str(first_det_last_stn) are simpatico }}

    first_det = first_det[[datetimecol]],
    first_stn = first_det[[stationcol]],

    last_arrival = first_det_last_stn[[datetimecol]],
    last_stn = first_det_last_stn[[stationcol]],

    stringsAsFactors = FALSE
  )

}

