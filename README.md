# fishtrackr
A collection of functions for summarizing acoustic telemetry datasets.

install:

`devtools::install_github("Myfanwy/fishtrackr")`
`library(fishtrackr)`

Developed using telemetry data from Vemco acoustic tags, though ultimate goal is cross-compatibility with JSATs, PIT tags, etc.

## Functionality Goals 

Will change depending on the objectives of the dataset in question, but in general:

#### Array-level

1. Summary functions for reporting/visualizing how many fish were detected at each receiver, detection windows, and automatic mapping of receiver array when lat/longs are supplied.
    - pie-in-the-sky: interactive leaflet map with tooltipping at receiver locations (scroll over the receivers pin to see a summary of the fish detected there), and scale map markers by density of fish detected at that receiver.

#### Outmigrating / Upmigrating Fish Functions

Typically applies to either juvenile fish moving downstream, or adult fish moving upstream; will likely need to greatly rely upon river kilometer metadata of receivers and the assumption that fish don't dally or reverse direction, which of course they do in practice (tricksy fisheses).

1. Calculate mean travel times and movement rates for outmigrating fish from release to final detection location.
    - currently sort of implemented with `getFirstLast()`.
2. Once upmigration array is defined, calculate mean travel times and movement rates for upmigrating fish.
3. Functions that summarise variability/variation in movement rate or travel time behavior would be neat.

#### Individual-level movement functions

1. `fishpaths()` function currently takes a full dataframe of detections for a group of tagged fish and summarises their bouts of movements.
    - residence time calculation between and within receivers is currently broken
2. Summarise time at receivers vs. time away from receivers, as a function of the fish's total residence in the array (time elapsed from first to last detection).
3. Visualize `fishpaths()`.

#### Group-level movement functions 

1. `fishpulse()` function to determine if fish travel together (i.e. do they arrive/depart receivers at similar times?)?
      - function name/fishtrackr 1.0 credit: Matt Peterson
2. `getReachMovements()` function summarises reach-specific movements (reachdistances and travel times).



