# FishTracker
A collection of functions for summarizing acoustic telemetry datasets.

# montable

A function that takes your detection file and creates a summary table that shows: Station, n (number of fish detected at that Station), min.date (minimum detection date at that Station), max.date (maximum detection date at that location) and range (the difference in time between max.date and min.date, to get a sense of how long each receiver was involved in detecting fish for this particular project).

Requires the following column header names in the input file (does not matter what order they are in): Station, DateTimeUTC, and TagID.  Any other columns in the detection file will be discarded in the final monitor table.
