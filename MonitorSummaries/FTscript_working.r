# File-Name:       IEP_2012_FTworkingscript.r
# Date:            2014-02-12
# Author:          Myfanwy Johnston/Matt Peterson
# Email(s):        merowlands@ucdavis.edu; mattlpeterson@gmail.com
# Purpose:         Create a suite of summary and analysis tools for telemetry data
# Data Location:   C:\Dropbox\Bypass Study\Analysis\White Sturgeon_2012\IEP_Conference_2014\data_clean
# Data Used:       yb2012_detections.csv   
# Data Used:       yb2012_deployments.csv  
# Data Used:       TagIDs_linear.csv                 

# Packages Used:   ggplot2, maptools, maps, mapproj, PBSmapping, wq
# Machine:         Myfanwy's Dell

# For more information, contact Matt via Email
# All rights reserved.
#-------------------------------------------------------------------------------
# functions built
# getReceiverData
# Pdetection
# getDetDays
# SpatTempPlot
# getMovement
# fishPulse
# fishPulsePlot
# RkmPlot
# PlotIt
# se
# getPA
# LinkData (In progess 2/6/12)
# LinkEnvData (In progress 2/6/12)
# FDscreen (works, but still in progress 2/29/12)
#-------------------------------------------------------------------------------
# Install packages here
install.packages(c("maptools"), #  "wq","maps","mapproj","PBSmapping", "ggplot2"),
      dependencies=TRUE,
      repos="http://cran.cnr.berkeley.edu/")
# Load libraries (if required)
library(wq)            # converts electrical conductivity to salinity
library(ggplot2)
library(maps)
library(mapproj)
library(maptools)      # also loads sp
library(PBSmapping)
library(pingr)
## SET INPUT / OUTPUT LOCATIONS

   
#figure.dir <- "/Volumes/NO NAME/FISH_TRACKER/PROJECTS/FISH_TRACKER_revamp/outfiles_figures/"    # output directory - for figures
#data.dir <- "/Volumes/NO NAME/FISH_TRACKER/PROJECTS/FISH_TRACKER_revamp/outfiles_data/"         # output directory - for data
## LOAD DETECTION DATA
getwd()
dir <- "C:/Users/Myfanwy/Dropbox/Bypass Study/Analysis/AdultSalmon_fall2013/Detections/data_clean"
data.dir <- "C:/Users/Myfanwy/Dropbox/Bypass Study/Analysis/AdultSalmon_fall2013/Detections/outfiles_data"
setwd(dir)    
test <- read.csv("detections_fca_2013_merged.csv", header=T)
head(test)
dim(test)
## 
## LOAD MONITOR DEPLOYMENT DATA
test1 <- read.csv("deployments_fca_2013.csv", header=T)
head(test1)
dim(test1)


##


#########################################
## (1a) ? Number of detections / fish or receiver (or group of receivers) ## should be in beginning
#########################################
##================================================== BEGIN
## start table - simple table summarizing number of detections, date ranges of detections for each monitor
unique.tags <- unique(test$TagID)
unique.monitors <- sort(unique(test$Location))
num_fish_monitor <- num_fish_detections <- min.date <- max.date <- lats <- longs <- rep(NA, length(unique.monitors))
for( i in 1:length(unique.monitors)){ 
num_fish_monitor[i] <- length( unique(test$TagID[test$Location == levels(unique.monitors)[i]  ]))
num_fish_detections[i] <- length(test$TagID[test$Location == levels(unique.monitors)[i]  ])
lats[i] <- unique(test$Lat[test$Location == levels(unique.monitors)[i]  ])
longs[i] <- unique(test$Lon[test$Location == levels(unique.monitors)[i]  ])
#ds <- strptime( test$DetectDate[test$Location == levels(unique.monitors)[i]],"%m/%d/%y %H:%M:%S")
ds <- strptime( test$DetectDate,"%m/%d/%Y %H:%M:%S")
min.date[i] <-  as.character(min(na.exclude( as.POSIXct(ds))))
max.date[i] <-  as.character(max(na.exclude( as.POSIXct(ds))))
}

## Myfanwy troubleshooting ##
ds <- strptime( test$DetectDate,"%m/%d/%Y %H:%M:%S")
head(ds)
 ?difftime


###############
diffs <- difftime( Sys.time(), max.date, tz="", units="days")
monitor_table <- as.data.frame(cbind( levels(unique.monitors), num_fish_monitor, 
  num_fish_detections, min.date, max.date, lats, longs, round(as.numeric(diffs), 1)))
head(monitor_table)

## return monitors that have not had detections for over a year (365 days)
tail(monitor_table[which( diffs >= 365 ), ])
# output dataset #
project <- "Monitor_table"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S") 
# myfile <- gsub("( )", "", paste(data.dir, project, mytime, ".csv"))
setwd(data.dir)
write.table(monitor_table, file="MonitorTable_fca2013.csv", sep=",", row.names=F, col.names=T, quote=FALSE, append=FALSE) 

# clean up workspace #
objects()
rm(monitor_table, num_fish_monitor, ds, unique.tags, unique.monitors,i,  
  num_fish_detections, min.date, max.date, lats, longs, diffs)
objects()
##================================================== END
##
##

##================================================== BEGIN getReceiverData    
#Sys.setenv(TZ="Etc/GMT+8") will use Pacific standard time only for time calculations.
##==================================================
getReceiverData <- function ( x, time.units=c("hours","days"), dist.units=c("km"), 
          min_detection_rule, meanPdet, crit_level, 
          time_frame = c("one_year","all_years"), YEAR) {
          # FILTERED (ONLY 1 DETECTION ALLOWED PER MINUTE)
          # x = TagID you want to look at
          # time.units = only option is day
          # dist.units = only option is km
          # min_detection_rule = user-defined minimum req'd number of detections at the monitor
          # meanPdet = user-defined mean detection probability of all receivers (0.5 is a good ballpark guess)
          # crit_level = criteria, usually set at 1/1000 (conservative), to identify possible false detections based 
          # time_frame = specific year, or all years you want to detections returned for
          # on the number of monitors missed prior to and after each detection
          # YEAR (if "one_year" specified) = year you want to look at
          #x <- 2841; time.units <- "days"; dist.units <- "km"; min_detection_rule<-5
          #meanPdet <- 0.5; crit_level <- 0.001; YEAR = 2011; time_frame = "all_years"
          ### run function 
          sunrise.set <- function(lat, long, date, timezone="UTC", num.days=1){
                #this needs to be long lat#
                lat.long <- matrix(c(long, lat), nrow=1)
                day <- as.POSIXct(date, tz=timezone)
                sequence <- seq(from=day, length.out=num.days , by="days")
                sunrise <- sunriset(lat.long, sequence, direction="sunrise",
                  POSIXct=TRUE)
                sunset <- sunriset(lat.long, sequence, direction="sunset",
                  POSIXct=TRUE)
                ss <- data.frame(sunrise, sunset)
                ss <- ss[,-c(1,3)]
                colnames(ss)<-c("sunrise", "sunset")
                return(ss)
          }
          if( time_frame == "one_year" ) {
            if(missing(YEAR))
              stop("Warning: option one_year specified, but no specific year!")
          aa <- test[ test$TagID==x , ][,6]
          aa <- strptime(aa,"%m/%d/%Y %H:%M:%S") 
          #aa <- aa$min
          aay <- test[ ,6]
          aay <- strptime(aay,"%m/%d/%Y %H:%M:%S")
          aay <- aay$year + 1900
          time_frame
          aa <- test[ test$TagID==x & aay==YEAR, ][,6]
          if( length(aa) == 0 )
            stop(paste("Warning: No detections occurred for TagID",x,"in",YEAR))
          aa <- strptime(aa,"%m/%d/%Y %H:%M:%S")
          aa <- aa$min
          group <- c(0, cumsum(diff(aa) != 0))
          bb <- tapply(aa, group,)
          cc <- unique(bb)
          arrivals <- departures <- rep( NA, length(cc))
          for( j in 1:length(cc) ) {
             arrivals[j] <- min( which( bb==cc[j] ))
             departures[j] <- max( which( bb==cc[j] ))
          }
          ind <- sort(unique(c( arrivals, departures )))
          ## time of arrivals / departures 
          time.of.arrivals <- strptime(test[ test$TagID==x & aay==YEAR, ][arrivals, 6], "%m/%d/%Y %H:%M:%S")
          time.of.arrivals
          length(time.of.arrivals)
          time.of.departures <- strptime(test[ test$TagID==x & aay==YEAR, ][departures, 6], "%m/%d/%Y %H:%M:%S")
          length(time.of.departures)
          ## amount of time spent at monitors (from first to last detection for each encounter at a monitor)
          amt.time.spent <- difftime(as.POSIXct(time.of.departures), as.POSIXct(time.of.arrivals), units=time.units)
          length( amt.time.spent)
          julian.day.arrival <- time.of.arrivals$yday + 1
          julian.day.departure <- time.of.departures$yday + 1
          num.detections.i <-  (departures - arrivals) + 1
          ## tab1 summarizes information about time of arrival (first detection), time of departure (last detection), 
          ## and amt. of time spent within range of receiver, as well as julian days of arrival and departures and number of detections at monitor 
          ## during each "unique" encounter (defn: been detected at another monitor prior to the next arrival)
          tab1 <- as.data.frame(cbind( test[ test$TagID==x & aay==YEAR, ][arrivals, ], 
              as.character(time.of.arrivals), as.character(time.of.departures),  amt.time.spent, julian.day.arrival, julian.day.departure, 
              num.detections.i ))    
          head(tab1) } else   
          if( time_frame == "all_years") { 
          aa <- test[ test$TagID==x , ][,6]
          if( length(aa) == 0 )
             stop(paste("Warning: No detections occurred for TagID",x,"in any year."))
          aa <- strptime(aa,"%m/%d/%Y %H:%M:%S") 
          aa <- aa$min
          group <- c(0, cumsum(diff(aa) != 0))
          bb <- tapply(aa, group,)
          cc <- unique(bb)
          arrivals <- departures <- rep( NA, length(cc))
          for( j in 1:length(cc) ) {
             arrivals[j] <- min( which( bb==cc[j] ))
             departures[j] <- max( which( bb==cc[j] ))
          }
          ind <- sort(unique(c( arrivals, departures )))
          time.of.arrivals <- strptime(test[ test$TagID==x, ][arrivals, 6], "%m/%d/%Y %H:%M:%S")
          time.of.arrivals
          length(time.of.arrivals)
          time.of.departures <- strptime(test[ test$TagID==x, ][departures, 6], "%m/%d/%Y %H:%M:%S")
          length(time.of.departures)
          amt.time.spent <- difftime(as.POSIXct(time.of.departures), as.POSIXct(time.of.arrivals), units=time.units)
          length( amt.time.spent)
          julian.day.arrival <- time.of.arrivals$yday + 1
          julian.day.departure <- time.of.departures$yday + 1
          num.detections.i <-  (departures - arrivals) + 1
          tab1 <- as.data.frame(cbind( test[ test$TagID==x, ][arrivals, ], 
              as.character(time.of.arrivals), as.character(time.of.departures),  amt.time.spent, julian.day.arrival, julian.day.departure, 
              num.detections.i ))    
          head(tab1)
          }
          # summarize filtered tab1
          aa <- as.numeric(tab1[,9])
          group <- c(0, cumsum(diff(aa) != 0))
          bb <- tapply(aa, group,)
          cc <- unique(bb)
          arrivals <- departures <- num.day <- num.night <- num.dawn <- num.dusk <- rep( NA, length(cc))
          for( j in 1:length(cc) ) {
             #j <- 85
             arrivals[j] <- min( which( bb==cc[j] ))
             departures[j] <- max( which( bb==cc[j] ))
# begin working section
          # for each monitor encounter, count the number of dets within each time of day
          # enctimes short for "encounter times" during each monitor visit
             enctimes <- strptime(tab1[ c(arrivals[j]:departures[j]), 6], "%m/%d/%Y %H:%M:%S")
             encdates <- strptime(tab1[ c(arrivals[j]:departures[j]), 6], "%m/%d/%Y")
             enclat <- abs( mean( tab1[ c(arrivals[j]:departures[j]), 12]  ))
             enclon <- -abs(mean( tab1[ c(arrivals[j]:departures[j]), 13]  ))
             dst.indicator <- enctimes$isdst  # 1 = dst in place (PDT), 0 = dst not in place (PST)
             which( dst.indicator == -1 )
             dst.indicator
             sunrisePST <- sunsetPST <- nduskPST <- ndawnPST <- rep(NA, length(enctimes))
             # for each detection time at each monitor, determine time of sunrise and sunset, nautical dawn and dark for the
             # day that det occurred
             #Sys.setenv(TZ="Etc/GMT+8") will use standard time only for time calculations.
             for( k in 1:length(enctimes)) {
              #k <-1
              # sunrises and sunsets
              res <- sunrise.set(enclat,enclon  , encdates[k], timezone="America/Los_Angeles", num.days=1 )
              sunrisePST[k] <- res$sunrise
              sunsetPST[k] <-  res$sunset
              # nautical dawn and dusk
              this.time <- as.POSIXct(encdates[k], tz="America/Los_Angeles")
              g <- crepuscule(matrix(c(enclon,enclat  ), nrow=1), this.time , solarDep=12, direction="dawn", POSIXct.out=T)
              ndawnPST[k] <- g[1,2]
              gg <- crepuscule(matrix(c(enclon,enclat  ), nrow=1), this.time, solarDep=12, direction="dusk", POSIXct.out=TRUE)
              nduskPST[k] <- gg[1,2]
              #nduskPST[k] <- seq( nautduskUTC[1,2], length=2, by="-8 hours")[2]
             }
             sunrisePST
             sunsetPST
             ndawnPST
             nduskPST
             sunrise.t <- as.POSIXct( sunrisePST , origin = "1969-12-31 16:00:00", tz = "America/Los_Angeles")
             sunrise.t
             sunset.t <- as.POSIXct( sunsetPST ,  origin = "1969-12-31 16:00:00", tz = "")
             sunset.t
             ndawn.t <- as.POSIXct( ndawnPST , origin = "1969-12-31 16:00:00", tz = "")
             ndawn.t
             ndusk.t <- as.POSIXct( nduskPST , origin = "1969-12-31 16:00:00", tz = "")
             ndusk.t
             enctimes
             ndawn.t
             newenctimes <- format(enctimes, tz="America/Los_Angeles", usetz=T)

             # sum up information here...

              origin <- "2005-01-01 00:00:00"
              dt.secs <- as.numeric(difftime( enctimes, origin, units="secs" )) ## dt = detection times
              sr.secs <- as.numeric(difftime( sunrise.t, origin, units="secs" ))     ## sr = sunrise
              ss.secs <- as.numeric(difftime( sunset.t, origin, units="secs" ))      ## ss = sunsets
              ndawn.secs <- as.numeric(difftime( ndawn.t, origin, units="secs" ))      ##
              ndusk.secs <- as.numeric(difftime( ndusk.t, origin, units="secs" ))      ##
              clsv <- d.test <-d.text <- n.test <- n.text <- sr.secs.pst <- ss.secs.pst <- ndawn.secs.pst <- ndusk.secs.pst <- dawn.test <- dawn.text <- dusk.test <- dusk.text <- rep(NA, length(enctimes))
                      which( is.na(dt.secs))
                      for( m in 1:length(enctimes)) {
                      # 1 = dst in place (PDT), 0 = dst not in place (PST)
                      #m <- 1
                      sr.secs.pst[m] <- sr.secs[m] - (ifelse( dst.indicator[m]==1, 3600, 0 ))#{sr.secs.pst[m]<-sr.secs[m] - 3600}else{ sr.secs.pst[m]<-sr.secs[m] - 0}
                      ss.secs.pst[m] <- ss.secs[m] - (ifelse( dst.indicator[m]==1, 3600, 0 ))#
                      ndawn.secs.pst[m] <- ndawn.secs[m] - (ifelse( dst.indicator[m]==1, 3600, 0 ))#
                      ndusk.secs.pst[m] <- ndusk.secs[m] - (ifelse( dst.indicator[m]==1, 3600, 0 ))#
                      #if( dst.indicator[m]==1 ){ss.secs.pst[m]<-ss.secs[m] - 3600}else{ ss.secs.pst[m]<-ss.secs[m] - 0}
                      #if( dst.indicator[m]==1 ){ndawn.secs.pst[m]<-ndawn.secs[m] - 3600}else{ ndawn.secs.pst[m]<-ndawn.secs[m] - 0}
                      #if( dst.indicator[m]==1 ){ndusk.secs.pst[m]<-ndusk.secs[m] - 3600}else{ ndusk.secs.pst[m]<-ndusk.secs[m] - 0}
                      ## day classification day = sunrise to sunset
                      cls<- c(   (dt.secs[m] >= sr.secs.pst[m] & dt.secs[m] <= ss.secs.pst[m]),       # day (if TRUE)
                          (dt.secs[m] <= ndawn.secs.pst[m] | dt.secs[m] >= ndusk.secs.pst[m]), # night (if TRUE)
                          (dt.secs[m] >= ndawn.secs.pst[m] & dt.secs[m] <= sr.secs.pst[m]),    # dawn (if TRUE)
                          (dt.secs[m] >= ss.secs.pst[m] & dt.secs[m] <= ndusk.secs.pst[m]) )    # dusk (if TRUE)
                          enctimes[m]
                      clsv[m] <- which( cls==TRUE )
                      }
                clsv
                num.day[j] <- length(clsv[clsv==1])
                num.night[j] <- length(clsv[clsv==2])
                num.dawn[j] <- length(clsv[clsv==3])
                num.dusk[j] <- length(clsv[clsv==4])
                }
          num.day
          num.night
          num.dawn
          num.dusk
          cbind( num.day, num.night, num.dawn, num.dusk,  ((departures - arrivals) + 1 ) )

          ind <- sort(unique(c( arrivals, departures )))
          time.of.arrivals <- strptime(tab1[arrivals, 6], "%m/%d/%Y %H:%M:%S")
          time.of.arrivals
          time.of.departures <- strptime(tab1[departures, 6], "%m/%d/%Y %H:%M:%S")
          time.of.departures
          amt.time.spent <- difftime(as.POSIXct(time.of.departures), as.POSIXct(time.of.arrivals), units=time.units)
          julian.day.arrival <- time.of.arrivals$yday + 1
          julian.day.departure <- time.of.departures$yday + 1
          num.detections.i <-  (departures - arrivals) + 1
          screen1 <- which( num.detections.i <= min_detection_rule )
          reason1 <- rep("NA", length(amt.time.spent))
          reason1[ screen1 ] <- paste("LTE_",min_detection_rule,"_Detections", sep="")
          tab1a <- as.data.frame(cbind( rep(x, length(amt.time.spent)), tab1[arrivals,c(8:11)], 
              as.character(time.of.arrivals), as.character(time.of.departures),  as.numeric(amt.time.spent), julian.day.arrival, julian.day.departure, 
              num.detections.i, tab1[arrivals,c(12,13)], reason1, num.day, num.night, num.dawn, num.dusk ))
          AmtOfTimeSpent <- paste("AmtOfTimeSpent_",time.units, sep="")
          myColnames <- c("TagID", "Location", "RKM", "Release_Location", "Release_RKM","TimeOfArrival", "TimeOfDeparture", AmtOfTimeSpent, 
              "JulianDayArrival", "JulianDayDeparture", "NumDetectionsEncounter", "Latitude", "Longitude", "Potential_FD1", "Dets_day",
              "Dets_night", "Dets_dawn","Dets_dusk" )
          colnames( tab1a ) <- myColnames
          head(tab1a)
          min.detect.date <- as.character(min(na.exclude( as.POSIXct( tab1a[,6] )))) 
          max.detect.date <- as.character(max(na.exclude( as.POSIXct( tab1a[,7] )))) 
          starts <-  combined_monitor_table[,2]
          ss <- as.character(levels(starts))[starts]
          stops <-  combined_monitor_table[,3]
          st <- as.character(levels(stops))[stops]
          ind1 <- which( min.detect.date > ss & max.detect.date < st )
          newmontab <- combined_monitor_table[ind1,]
          if( nrow(tab1a)==1 ) { 
               tab1a <- cbind( tab1a, 1, 1, NA )
               colnames( tab1a )<-c(myColnames, "Detected", "MonitorOperating", "Potential_FD2" )
          return(tab1a) } else
          if( nrow(tab1a) > 1) { 
               myList <- tagList <- monList <- startatlarge <- endatlarge <- vector( 'list', (nrow(tab1a)-1) )
               signs <- rep(NA, (nrow(tab1a)-1)) 
               for(i in 1:(nrow(tab1a)-1)) {
                    signs[i] <- sign(tab1a[i,3] - tab1a[i+1,3])
                    #i<-3
                    if( signs[i] == 1 ) {
                         f <- newmontab[,4] 
                         fs <- as.numeric(levels(f))[f]
                         ind2 <- which(( fs < tab1a[i,3] & fs > tab1a[i+1,3] ) == TRUE)  # sign between mon1 and mon2 ( - ) e.g. downstream migration
                         ds <-newmontab[ind2,] } else   
                    if( signs[i] == -1) { 
                         f <- newmontab[,4] 
                         fs <- as.numeric(levels(f))[f]
                         ind3 <- which(( fs > tab1a[i,3] & fs < tab1a[i+1,3] ) == TRUE)  # sign between mon1 and mon2 ( + ) e.g. upstream migration
                         ds <-newmontab[ind3,] } 
                         f <- ds[,4] 
                         fs <- as.numeric(levels(f))[f]
                         myList[[i]] <- fs # rkms of monitors missed
                         tagList[[i]] <- rep( x, nrow(ds)) # repeat tag number length needed
                         monList[[i]] <- as.character(ds[,1]) # 
                         startatlarge[[i]] <- rep( as.POSIXct( tab1a[i,7] ), nrow(ds)) ## start of at large period
                         endatlarge[[i]] <- rep( as.POSIXct( tab1a[i+1,6] ), nrow(ds)) ## end of at large period
                    }
          myList
          tagList
          monList
          f <- combined_monitor_table[,4]  # missed rkms
          combined_monitor_table
          fs <- as.numeric(levels(f))[f]
          missed.mons <- unlist( myList )
          taglist <- unlist( tagList)
          start_at_large <- unlist( startatlarge )
          end_at_large <- unlist( endatlarge )
          missed.locs <- unlist( monList ) # missed locations
          tab <- as.data.frame( cbind(taglist, missed.mons, missed.locs,
              as.character(as.POSIXct(start_at_large, origin="1969-12-31 16:00:00")), 
              as.character(as.POSIXct(end_at_large, origin="1970-01-01 16:00:00"))))
          mat <- as.data.frame(matrix( rep(NA, nrow(tab) * ncol(tab1a)), byrow=T, ncol=ncol(tab1a)))
          mat[,1] <- tab[,1]
          mat[,3] <- tab[,2]
          mat[,11] <- rep(0, nrow(tab))
          mat[,2] <- tab[,3]
          mat[,6] <- tab[,4]
          colnames( mat ) <- myColnames
          tab1b <- rbind( tab1a, mat )
          detected <- rep(NA, nrow(tab1b))
          for(j in 1:nrow(tab1b)){ 
               detected[j] <- ifelse(tab1b[j,11]>1, 1, tab1b[j,11]) 
               }
          tab1b <- cbind( tab1b, detected )
          colnames( tab1b ) <- c(myColnames, "Detected")
          tab1c <- tab1b[order( as.POSIXct(tab1b$TimeOfArrival)),] 
          ## last check on receiver operation dates before calculation of P(detection occurring) 
          ## AND tabulation of number of detections/non-detections for each monitor 
          starts <-  combined_monitor_table[,2]
          ss <- as.character(levels(starts))[starts]
          stops <-  combined_monitor_table[,3]
          st <- as.character(levels(stops))[stops]
          monitor.operating <- rep(NA, nrow(tab1c))
          for( j in 1:nrow(tab1c)) {
               if( tab1c$Detected[j] == 1 ) { monitor.operating[j] <- 1 } else
                   if( tab1c$Detected[j] == 0 ) {
                   f <- combined_monitor_table[,1]  # missed monitors
                   fs <- as.character(levels(f))[f]
                   ind3 <- which( fs == as.character(tab1c$Location[j]) )
                   monitor.operating[j] <- ifelse( ( as.character(tab1c$TimeOfArrival[j]) > ss[ind3] & as.character(tab1c$TimeOfArrival[j]) < st[ind3]) == TRUE, 1, 0 )
              }
          }
          tab1c <- cbind( tab1c, monitor.operating) 
          tab1c <- subset( tab1c, monitor.operating==1 )
          colnames( tab1c ) <- c(myColnames, "Detected", "MonitorOperating")
          ## calculation of P(detection occurring)
          cbind( tab1c$Detected, tab1c$NumDetectionsEncounter )
          num1s <- which( tab1c$Detected == 1 )
          n <- length(num1s)
          num0s.prior <- num0s.after <- num0s <- rep(NA, length(num1s))
          for(i in 1) {
               num0s.prior[i] <- 0
               num0s.after[i] <- num1s[i+1] - num1s[i]
               num0s[i] <- num0s.after[i] - 1
          }
          MINIT <- 2 # constant
          iter <- ifelse( MINIT >= n, MINIT, n)
          iter
          for(i in MINIT:iter) {
               num0s.prior[i] <- num1s[i] - num1s[i-1]
               num0s.after[i] <- num1s[i+1] - num1s[i]
               num0s[i] <- num0s.prior[i] + num0s.after[i] - 2
          }
          for(i in n) {
               num0s.prior[i] <- num1s[i] - num1s[i-1]
               num0s.after[i] <- 0
               num0s[i] <- num0s.prior[i] - 1
          }
          cbind( num1s, tab1c$Detected[num1s] , tab1c$NumDetectionsEncounter[num1s], num0s, meanPdet^num0s )
          checkind <- which( (meanPdet^num0s) <= crit_level )
          fd.check <- rep(NA, nrow(tab1c))
          fd.check[ num1s[checkind] ] <- "CHECK"
          tab1d <- cbind( tab1c, fd.check )
          colnames( tab1d )<-c(myColnames, "Detected", "MonitorOperating", "Potential_FD2"  )
          return( tab1d )
  }
}
## test the function
getReceiverData(13715, time.units="days", min_detection_rule=3, meanPdet = 0.5, crit_level = 0.001, time_frame = "all_years")
getReceiverData(4325, time.units="days", min_detection_rule=5, meanPdet = 0.5, crit_level = 0.001, time_frame = "one_year", YEAR=2009)
getReceiverData(223, time.units="days", min_detection_rule=5,
    meanPdet = 0.5, crit_level = 0.001, time_frame = "one_year", YEAR=2010)
getReceiverData(46619, time.units="days", min_detection_rule=5, meanPdet = 0.5, crit_level = 0.001, time_frame = "all_years", YEAR=2011)


##======================================== BEGIN LOOPING SEQUENCE
tag.ids <- groupList$group12                  # just for your group(s)
tag.ids 
#all.tag.ids <- as.vector(unlist(groupList))  # or all groups
#all.tag.ids

##======================================== BEGIN LOOPING SEQUENCE
tag.ids <- unique(test$TagID)[1:2]           ## just for 2012 WS Yolo Bypass
tag.ids 

##======================================== BEGIN
#output dataset - loop works - d/n modify
project <- "getReceiverData_revamp_"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S") 
myfile <- paste( project, mytime, ".csv", sep="")
for( j in 1:length(tag.ids) ) {  
     b <- getReceiverData(tag.ids[j], time.units="days", min_detection_rule=5,
          meanPdet = 0.5, crit_level = 0.001, time_frame = "all_years", YEAR=2010)
     write.table( b, file=myfile,  quote=F, sep=",", row.names=F, append=T)
}
b

outfilename <- gsub("( )", "",paste(project, mytime, ".csv"))
# clean up workspace #
objects()
rm(b, j, myfile, mytime, other.ids, outfilename, project, tag.ids)
objects()




##======================================== END
## CALCULATE DETECTION PROBABILITIES BASED ON SUMMARIZED MONITOR DATA 
## FROM getReceiverData function
## SET INPUT / OUTPUT LOCATIONS
dir <- "E:/WORK/FISH_TRACKER/outfiles_data"                 # input directory
getReceiverData_revamp_Aug_23_2013_11_32_40.csv
getwd()
dir <- "/Volumes/NO NAME/FISH_TRACKER/PROJECTS/FISH_TRACKER_revamp/data_clean"                  # input directory
setwd(dir)
outfilename


mon_data <- read.csv("getRD_all.csv", stringsAsFactors=F,   header=T)  # load previously written .csv file
#mon_data <- read.csv(outfilename,  header=T)                                # load current written .csv file
ind <- which( mon_data[,1] == "TagID" )   ## get rid of excess headers
mdata <- mon_data[-c(ind),]
class( mdata$TimeOfDeparture)
length( unique( mdata$Location[mdata$Detected == 1 ] ))

dts <- strptime( mdata$TimeOfArrival, "%m/%d/%Y %H:%M:%S")
Year <- dts$year + 1900
Month <- as.numeric(dts$mon + 1)

# subset to include only months 4-6 in year 2010 (
#test2 <- subset( mdata, Year==2010 & Month == 4 |
#      Year==2010 &Month == 5 |  Year==2010 &Month == 6)

## MEJ Note: this function did not work as written (error message: Error in g.rkm[j] <- unique(as.character(newd$RKM[which(bb == cc[j])])) : 
# replacement has length zero); but the one built in Pdetection_v.2.0.1_IEP.r did.  ??

Pdetection <- function( data, gated = c(T,F) ) {
          # data  = data from getReceiverData function
          # gated = T for detection probs for gated design (grouped by RKM), F for single monitors (e.g. no design)
          data = mdata; gated = T
          unique.monitors <- unique( data$Location )
          Pdets <- loc <- rkm <- lat <- lon <- num1s <- totals <- obs.detections <- obs.time.spent <- rep(NA, length(unique.monitors))
          for( i in 1:length(unique.monitors)){
                i <- 1
                detects <- data$Detected[data$Location == unique.monitors[i]]
                detects <- as.numeric(as.character(detects))
                num1s[i] <- sum(detects)
                totals[i] <- length(detects)
                Pdets[i] <- sum(detects) / length(detects)
                loc[i] <- unique.monitors[i]
                rkm[i] <- unique(data$RKM[data$Location == unique.monitors[i]])
                lat[i] <- unique(test$Lat[test$Location == unique.monitors[i]])   # looks at combined_monitor_table aka test1
                lon[i] <- unique(test$Lon[test$Location == unique.monitors[i]])   # looks at combined_monitor_table aka test1
                obs.detections[i] <- sum( as.numeric(data$NumDetectionsEncounter[data$Location == unique.monitors[i]] ))
                obs.time.spent[i] <- sum( as.numeric(na.exclude(data$AmtOfTimeSpent_days[data$Location == unique.monitors[i]] ))) # in days
          }
          Pdet.tab <- as.data.frame(cbind( as.character(loc), rkm, lat, lon, num1s, totals, Pdets,
              obs.detections, obs.time.spent))
          Prob_Det <- Pdet.tab[order(as.numeric(rkm)),]
          colnames( Prob_Det ) <- c("Monitor","RKM","Lat","Lon","num1s","totals","Pdets","ObservedDetections","ObservedTimeDays")
          head(Prob_Det)
          newd <- Prob_Det
          head(newd)

          ## start to build Prob_Det_Gated table
          ## set up "gated" reaches
          aa <- as.numeric(newd$RKM)
          group <- c(0, cumsum(diff(aa) != 0))
          bb <- tapply(aa, group,)
          cc <- unique(bb)
          g.loc <- g.rkm <- g.num1s <- g.totals <- g.Pdets <- g.obs.detections <- g.time.spent <- g.reaches <- rep(NA, length(cc))
          for( j in 1:length(cc) ) {
                #j<-1
                g.loc[j] <- paste(unique( as.character(newd[,1][ which( bb==cc[j] )] )), collapse=", ")
                g.rkm[j] <- unique( as.character(newd$RKM[ which( bb==cc[j] )] ))
                g.num1s[j] <- sum(as.numeric(newd$num1s[ which( bb==cc[j] )]) )
                g.totals[j] <- sum(as.numeric(newd$totals[ which( bb==cc[j] )]) )
                g.Pdets[j] <- g.num1s[j] / g.totals[j]
                g.obs.detections[j] <- sum(as.numeric(newd$ObservedDetections[ which( bb==cc[j] )]) )
                f <- newd[,9][ which( bb==cc[j] )]
                fs <- as.numeric(levels(f))[f]
                g.time.spent[j] <- sum( fs )
          }
          Prob_Det_Gated <- as.data.frame( cbind( as.character(g.loc) , g.rkm , g.num1s , g.totals , g.Pdets , g.obs.detections , g.time.spent ))
          colnames( Prob_Det_Gated ) <- c("Monitors","RKM","num1s","totals","Pdets","ObservedDetections","ObservedTimeDays")
          head(Prob_Det_Gated)

          if( gated == T ){return( Prob_Det_Gated ) }else
          if( gated == F ){return( Prob_Det ) }

} # end function
# test function - data must be in same format that is outputted from getReceiverData function
a <- Pdetection( data = mdata, gated = T ) # will return Pdetections assuming gated design
dim(a)
b <- Pdetection( data = mdata, gated = F ) # will return Pdetections for single receivers
dim(b)

##======================================== BEGIN
#output dataset - d/n modify
project <- "Pdetection_gated_"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S")
myfile <- gsub("( )", "", paste(data.dir, project, mytime, ".csv"))
a <- Pdetection( data = test2, gated = T ) # will return Pdetections assuming gated design
write.table( a, file=myfile,  quote=T, sep=",", row.names=F)

##======================================== END
##
##
##
##================================================== BEGIN     
getDetDays <- function( x ) {
                ## x = TagID you want to look at
                #x <- 16235
                locs <- test[ test[,3]==x , ][,8]
                latitude <- test[ test[,3]==x , ][,12]
                longitude <- test[ test[,3]==x , ][,13]
                aa <- test[ test[,3]==x , ][,6]
                aa <- strptime(aa,"%m/%d/%Y %H:%M:%S") 
                julian.day <- as.numeric(format(aa, "%j"))  #     "%j" = julian day
                y <- as.numeric(format(aa, "%Y"))
                m <- as.numeric(format(aa, "%m"))
                group <- c(0, cumsum(diff(julian.day) != 0))
                bb <- tapply(julian.day, group,)
                cc <- unique(bb)
                j.day <- year <- month <- number.mons <- rep( NA, length(cc))
                monList <- jdayList <- rkmList <- yearList <- monthList <-  vector( 'list', length(j.day) )
                for( j in 1:length(cc) ) {
                     j.day[j] <- unique( julian.day[ which( bb==cc[j] )] )
                     year[j] <- unique( y[ which( bb==cc[j] )] )
                     month[j] <- unique( m[ which( bb==cc[j] )] )
                     monList[[j]] <- as.character(unique( locs[ which( bb==cc[j] )] ))
                     number.mons[j] <- length(unique( locs[ which( bb==cc[j] )] ))
                     jdayList[[j]] <- rep(j.day[j], number.mons[j])
                     yearList[[j]] <- rep(year[j], number.mons[j])
                     monthList[[j]] <- rep(month[j], number.mons[j])
                     }
                mons <- unlist(monList)
                jds <- unlist(jdayList)
                years <- unlist(yearList)
                monthss <- unlist(monthList)
                det.table <- as.data.frame(cbind( mons, jds, years, monthss))
                dettab <- det.table[order( det.table[,1] ),] 
                rkm <- lat <- lon <- rep(NA, nrow(dettab)) 
                for(i in 1:length(rkm)) {
                     #i<-2
                     f <- dettab[,1]
                     fs <- as.character(levels(f))[f]
                     ind <- which( fs == as.character(dettab[i,1]) )
                     rkm[ind] <- unique(test[,9][test$Location == fs[ind]])
                     lat[ind] <- unique(test$Lat[test$Location == fs[ind]])
                     lon[ind] <- unique(test$Lon[test$Location == fs[ind]])
                     }
                tab1 <- cbind( dettab, rkm, lat, lon)
                tab2 <- cbind( rep(x, nrow(tab1)), tab1)
                colnames( tab2 ) <- c("TagID","Monitor","UniqueJulianDays","Year","Month","RKM","Lat","Lon")
                return( tab2 )
}
## test
a <- getDetDays(2841)
#by( a$UniqueJulianDays, a$Monitor , length ) # number of unique julian days by monitor 
as.numeric( by( a$UniqueJulianDays, a$Monitor , length ))
as.character( unique( a$Monitor))
xx <- data.frame( as.character( unique( a$Monitor)), as.numeric( by( a$UniqueJulianDays, a$Monitor , length )) )
xx
##
##
##======================================== BEGIN LOOPING SEQUENCE

tag.ids <- c(56493, 56489, 56488, 56479, 56480, 56474, 2850, 2848, 2849, 2852, 2846, 56477, 2861, 2841, 2851, 2856,56494, 2854,56485,
             2870, 2847, 56486,2844, 2867, 2879, 2872, 2853,2871, 2869, 2868, 2883,
             2886,  56475, 56481, 2882, 56492, 2863, 56490, 2873, 2859, 56478, 2857,
             2878, 2877, 2845, 2876, 2855, 2858, 56491, 2875, 2860, 56483, 2885,
             2843, 56482,2842, 2862, 2866, 2884, 56484, 2880, 56476, 56473, 56487)
##======================================== BEGIN LOOP
# output dataset - loop works - d/n modify
project <- "getDetDays_2012"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S") 
myfile <- paste(project, mytime, ".csv")
for( j in 1:length(tag.ids) ) {  
     b <- getDetDays(tag.ids[j])  
     write.table( b, file=outfilename,  quote=F, sep=",", row.names=F, append=T) 
}
b
outfilename <- gsub("( )", "",paste(project, mytime, ".csv"))
# clean up workspace #
objects()
rm(b, j, myfile, mytime, other.ids, project, tag.ids)
objects()

##================================================== END
##
##
##
##
##================================================== BEGIN
## get PA will be a function to return hourly / daily / monthly presence or absence at a each monitor detected
getPA <- function(x, dstart, dend, time_units=c("hour","day","month") ) {
                # time_units are user-specified
                # setup for all time periods for each fish
                # setup for all possible time periods
                #x = 48422; dstart = "12/21/2011 16:01:00"; dend = "1/2/2012 0:00:00"; time_units = "month"
                require(utils)
                quickertest <- test[test$TagID == x, ]
                aa <- quickertest[ ,6 ]    # times of detections
                aa <- strptime(aa,"%m/%d/%Y %H:%M:%S")
                dstart <- strptime(dstart,"%m/%d/%Y %H:%M:%S")
                dend <- strptime(dend,"%m/%d/%Y %H:%M:%S")
                #test[ which( is.na( aa)) , ]
                #head(aa)
                jds <- as.numeric(format(aa, "%j"))  #     "%j" = julian day
                #range( jds)
                #unique( jds)
                y <- as.numeric(format(aa, "%Y"))
                m <- as.numeric(format(aa, "%m"))
                hr <- as.numeric(format(aa, "%H"))
                poi_start <- as.POSIXct( dstart - 1 )   # start date (usually at tagging) - 1 days to account for all dates/times
                poi_end <- as.POSIXct( dend + 86400 )      # end date (end of tag life or after a download cycle) + 2 days to account for all dates/times
                #time_units<-"month"  # TEST FUNCTION INTERALLY
                tinputs <- c( "hour", "%H" ,  "day", "%j" ,"month", "%m" )
                # HOURLY PRESENCE / ABSENCE
                if( time_units == "hour"   ){
                      timeinputs <- tinputs[1:2]
                      date1 <- seq(poi_start,poi_end,by= timeinputs[1] )
                      allhrs <- as.numeric( format( date1, timeinputs[2] )) # julian day of sequenced dates
                      allys <- as.numeric( format( date1, "%Y" ))  # year of sequenced dates
                      alljds <- as.numeric( format( date1, "%j" ))  # year of sequenced dates
                      # build overall grid (e.g. each monitor x each day combo)
                      hourgrid <- expand.grid( tagids=x, monitors=unique( test[,9] ), allhrs, unique(allys) )
                      addgrid <- expand.grid( tagids= x, monitors=unique( test[,9] ), alljds, unique(allys) )
                      if( length( which( duplicated( hourgrid )) ) == 0){ hourgrid <- hourgrid }else
                      if( length( which( duplicated( hourgrid )) ) >= 1){
                      hourgrid <- hourgrid[ -c(which( duplicated( hourgrid ))), ]
                      }
                      # begin loop to logically test whether or not each combination in daygrid cols 1-3 occurred
                      pa.hour <- count.hour <- rep(NA, nrow(hourgrid))
                      for( j in 1:length(pa.hour)) {
                          ind <- which( test[,3] ==hourgrid[j,1] & test[,9] ==hourgrid[j,2] & hr==hourgrid[j,3] & jds==addgrid[j,3] & y==hourgrid[j,4] )
                          count.hour[j] <- length( ind )
                          if( length(ind)==0){pa.hour[j]<-0}else
                          if( length(ind)!=0){pa.hour[j]<-1}
                          }
                      tdat <- data.frame( hourgrid, addgrid[,3], pa.hour, count.hour )
                      tdat <- tdat[ order(tdat[,1], tdat[,2]), ]    # orders by tagID then by rkm, then by var3
                      colnames(tdat) <- c( "tagid", "rkm", time_units, "year", "julianday", "pa", "count" )
                      return( tdat )
                }else
                # DAILY PRESENCE / ABSENCE
                if( time_units == "day"    ){
                      timeinputs <- tinputs[3:4]
                      date1 <- seq(poi_start,poi_end,by= timeinputs[1] )
                      alljds <- as.numeric( format( date1, timeinputs[2] )) # julian day of sequenced dates
                      allys <- as.numeric( format( date1, "%Y" ))  # year of sequenced dates
                      allms <- as.numeric( format( date1, "%m" ))  # year of sequenced dates
                      # build overall grid (e.g. each monitor x each day combo)
                      daygrid <- expand.grid( tagids=x , monitors=unique( test[,9] ), alljds, unique(allys) )
                      dim(daygrid)
                      if( length( which( duplicated( daygrid )) ) == 0){ daygrid <- daygrid }else
                      if( length( which( duplicated( daygrid )) ) >= 1){
                      daygrid <- daygrid[ -c(which( duplicated( daygrid ))), ]
                      }
                      dim( daygrid )
                      # begin loop to logically test whether or not each combination in daygrid cols 1-3 occurred
                      pa.day <- count.day <- rep(NA, nrow(daygrid))
                      for( j in 1:length(pa.day)) {
                          ind <- which( quickertest[ ,3 ] ==daygrid[j,1] & quickertest[ ,9 ] ==daygrid[j,2] & jds==daygrid[j,3] & y==daygrid[j,4]  )
                          count.day[j] <- length(ind)
                          if( length(ind)==0){pa.day[j]<-0}else
                          if( length(ind)!=0){pa.day[j]<-1}
                          }
                      tdat <- data.frame( daygrid, pa.day, count.day )
                      tdat <- tdat[ order(tdat[,2], tdat[,4], tdat[,3]), ]    # orders by tagID then by rkm, then by var3
                      colnames(tdat) <- c( "tagid", "rkm", time_units, "year", "pa", "count" )
                      # trim excess days from beginning (before tagging or start of study)
                      e.year <- as.numeric(format(poi_start, "%Y"))
                      e.jday <- as.numeric(format(poi_start, "%j"))
                      dim( tdat )
                      ind <- which( tdat$year == e.year & tdat[,3] < e.jday )
                      #if( length( ind == 0)){ tdat1 <- tdat }else
                      #if( length( ind >= 1)){ tdat1 <- tdat[ -c(ind) , ]  }
                      if( length( ind != 0)){ tdat <- tdat[ -c(ind) , ]  }
                      dim( tdat )
                      # trim excess days after end (after tag life or end of study)
                      e.year <- as.numeric(format(poi_end, "%Y"))
                      e.jday <- as.numeric(format(poi_end, "%j"))
                      dim( tdat )
                      ind <- which( tdat$year == e.year & tdat[,3] > e.jday )
                      #if( length( ind == 0)){ tdat1 <- tdat }else
                      #if( length( ind >= 1)){ tdat1 <- tdat[ -c(ind) , ]  }
                      if( length( ind != 0)){ tdat <- tdat[ -c(ind) , ]  }
                      dim( tdat )
                      return( tdat )
                }else
                # MONTHLY PRESENCE / ABSENCE
                if( time_units == "month"  ){
                      timeinputs <- tinputs[5:6]
                      date1 <- seq(poi_start,poi_end,by= timeinputs[1] )
                      alljds <- as.numeric( format( date1, timeinputs[2] )) # julian day of sequenced dates
                      allys <- as.numeric( format( date1, "%Y" ))  # year of sequenced dates
                      allms <- as.numeric( format( date1, "%m" ))  # year of sequenced dates
                      # build overall grid (e.g. each monitor x each day combo)
                      monthgrid <- expand.grid( tagids=x, monitors=unique( test[,9] ), allms, unique(allys) )
                      if( length( which( duplicated( monthgrid )) ) == 0){ monthgrid <- monthgrid }else
                      if( length( which( duplicated( monthgrid )) ) >= 1){
                      monthgrid <- monthgrid[ -c(which( duplicated( monthgrid ))), ]
                      }
                      # begin loop to logically test whether or not each combination in monthgrid cols 1-3 occurred
                      pa.month <- count.month <- rep(NA, nrow(monthgrid))
                      for( j in 1:length(pa.month)) {
                          ind <- which( quickertest[ ,3 ] ==monthgrid[j,1] & quickertest[ ,9 ] ==monthgrid[j,2] & m==monthgrid[j,3] & y==monthgrid[j,4]  )
                          count.month[j] <- length(ind)
                          if( length(ind)==0){pa.month[j]<-0}else
                          if( length(ind)!=0){pa.month[j]<-1}
                          }
                          head(pa.month)
                      tdat <- data.frame( monthgrid, pa.month, count.month )
                      tdat <- tdat[ order(tdat[,1], tdat[,2]), ]    # orders by tagID then by rkm, then by var3
                      colnames(tdat) <- c( "tagid", "rkm", time_units, "year", "pa", "count" )
                      which( duplicated( monthgrid ))

                      # trim excess months from beginning (before tagging or start of study)
                      e.year <- as.numeric(format(poi_start, "%Y"))
                      e.jday <- as.numeric(format(poi_start, "%m"))
                      dim( tdat )
                      ind <- which( tdat$year == e.year & tdat[,3] < e.jday )
                      if( length( ind != 0)){ tdat <- tdat[ -c(ind) , ]  }
                      dim( tdat )
                      # trim excess months after end (after tag life or end of study)
                      e.year <- as.numeric(format(poi_end, "%Y"))
                      e.jday <- as.numeric(format(poi_end, "%m"))
                      dim( tdat )
                      ind <- which( tdat$year == e.year & tdat[,3] > e.jday )
                      if( length( ind != 0)){ tdat <- tdat[ -c(ind) , ]  }
                      dim( tdat )
                      #sum( tdat[,6] )
                      #tdat[ tdat[,5]==1, ]
                      #tdat
                      return( tdat )
                }
      } # end function
# test function
a <- getPA(2841, dstart = "3/1/2012 12:00:00",
      dend = "6/1/2012 12:00:00",
      time_units = "month")
a
      # hour does not work - 8/23/13; day and month are fine


par( mfrow=c(1,1))

# Set up a boxplot of mean daily counts # all sites
boxplot( as.numeric(as.character(a$count)) ~ a$rkm ,  ylim=c(0,100))  #
# Set up a series of boxplots of mean daily counts # reach above IF
uis <- unique(a$rkm)
par(mfrow=c(1,1))
for (i in uis) {
ss <- subset(a, a$rkm == uis[i] )
boxplot( as.numeric(as.character(ss$count)) , main= paste(uis[i]), ylim=c(0,10))  # ~ as.character(ss$rkm)
}
##

a <- getPA(1, dstart = "1/1/2006 12:00:00", dend = "10/10/2011 12:00:00", time_units = "month")
a <- getPA(1, dstart = "10/6/2011 12:00:00", dend = "10/10/2011 12:00:00", time_units = "hour")
dim(a)

##======================================== BUILD TABLE
## simple table: col1 = fish.ids; col2 = tag.ids; col3 = start of date/times for tags;
## col4 = end of date/times for tags
## load tag info table here
taginfo <- read.csv("tag_start_end.csv",header=T )
head(taginfo)

##======================================== BEGIN LOOP
# output dataset - loop works - d/n modify
project <- "getPA_month_"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S")
myfile <- gsub("( )", "", paste(data.dir, project, mytime, ".csv"))
ids <- c(1:8)
ids
for( j in ids ) {
     #j<-1
     b <- getPA(taginfo[j,1], dstart = taginfo[j,3], dend = taginfo[j,4], time_units = "month")
     write.table( b, file=myfile,  quote=F, sep=",", row.names=F, append=T)
}
b

##======================================== END LOOP
##
##
##
## SUMMARIZE DETECTION DAYS BASED ON SUMMARIZED MONITOR DATA 
## FROM getDetDays function
## SET INPUT / OUTPUT LOCATIONS
dir <- "C:\\Users\\Myfanwy\\Dropbox\\Bypass Study\\Analysis\\White Sturgeon_2012\\IEP_Conference_2014\\FishTracker_IEP_0312-1212\\outfiles_data"               # input directory
setwd(dir)
# trimmed to only include days from months Feb - June
getwd()
t3 <- read.csv(   "getDetDays_2012Feb_19_2014_20_09_00.csv" , stringsAsFactors=FALSE, header=T)  # load previously written .csv file
ind <- which( t3[,1] == "TagID" )
test3 <- t3
head(test3)

SpatTempPlot <- function( data, year, urkm, lrkm, spec.locations, month.nums) {
# data =
# lrkm =
# urkm =
# spec.locations =
# year =
# month.nums
#data = test3  ; year = 2012    ; urkm = 300   ; lrkm = 100  ;spec.locations = c(225, 260); month.nums
d1 <- subset(data, test3$Year==year & test3$RKM < urkm & test3$RKM > lrkm)
if( nrow( d1 ) == 0 )
stop(paste("Warning: For your dataset, no detections occurred in",year,"... OR between RKM",lrkm,"and",urkm,"in",year))
#year <- unique(d1[,4])
ds <- as.numeric(d1[,3])
#ds <- as.numeric(levels(f))[f]
d2 <- d1[order(ds),]
head(d2)
aa <- as.numeric(d2$UniqueJulianDays)
group <- c(0, cumsum(diff(aa) != 0))
bb <- tapply(aa, group,)
cc <- unique(bb)
numfish <- aday <- numdetectdays <- mnth <- lat <- lon <- rep(NA, length(cc))
#weekly.mean.rkm <- sds <- nums <- num.mons 
for( j in 1:length(cc) ) {
     #j <- 1
     numfish[j]<- length( unique( d2$TagID[which( bb==cc[j] )] ))
     # of detections per day summed over all fish
     numdetectdays[j] <- length( d2$RKM[which( bb==cc[j] )] )
     aday[j] <- unique( as.numeric(d2$UniqueJulianDays)[which( bb==cc[j] )] )
     mnth[j] <- unique( as.numeric(d2$Month)[which( bb==cc[j] )] )
     lat[j] <- unique( as.numeric(d2$Lat)[which( bb==cc[j] )] )
     lon[j] <- -abs(unique( as.numeric(d2$Lon)[which( bb==cc[j] )] ))
     #num.mons[j] <- 
     }
numfish
aday
numdetectdays
mnth
aa <- as.numeric(d2$Month)
group <- c(0, cumsum(diff(aa) != 0))
bb <- tapply(aa, group,)
cc <- unique(bb)
numfish.month <- rep(NA, length(cc))
for( j in 1:length(cc) ) {
     numfish.month[j]<- length( unique( d2$TagID[which( bb==cc[j] )] ))
     }
numfish.month
#month.nums <- min(month.nums):max(month.nums)
month.nums <- 1:12

missing.aa <- setdiff(month.nums, unique(aa))
length( missing.aa)
if( length( missing.aa)==0 ){
mfd <- cbind(unique(aa), numfish.month) }else
if( length( missing.aa) > 1 ){
mfd <- rbind(cbind(missing.aa, rep(0,length(missing(aa)))), cbind(unique(aa), numfish.month))
}
mfd  # has 0s for months that no fish were detected (if fish not detected in all months)
mfd <- mfd[order(mfd[,1]),]
jdays <- as.numeric(d1[,3])
jdays
rkms <- as.numeric(d1[,6])
rkms

## setup for upper right plot (#2)
rtab <- as.data.frame(table(rkms))
f <- rtab[,1]
rs <- as.numeric(levels(f))[f]
rtab[,2]

## setup for plot w/ detection day frequency
ltab <- as.data.frame( unique( cbind(as.numeric(d2$Lat), -abs(as.numeric(d2$Lon)))))
freqtab <- as.data.frame( table( paste( d2$Lat, -abs(as.numeric(d2$Lon)))))
ltab
freqtab
newltab <- cbind(ltab, freqtab[,2])
colnames(newltab) <- c("lat","lon", "num_detection_days")
head(newltab) # do not remove - see new mapping function below #

##================================================== BEGIN GRAPH   
par(mfrow=c( 1,1 ))
ylim<-c( lrkm, urkm)
nf <- layout(matrix(c(1,1,2, 1,1,2, 3,3,0), 3, 3, byrow = TRUE),
   widths=c(2,1), heights=c(1,2))
layout.show(nf)  # for 3-panel plot
cex <- cex.axis <- cex.lab <- 1.5
## upper left plot - main (#1)
par(mar = c(2, 2, 1, 2) + 0.1) # c(bottom, left, top, right)
Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
xlim <- c(0,365)
ylim<- c(lrkm, 1.1*urkm)
x.ats <- xlim[1]:xlim[2]
x.labs <- c(x.ats[1:max(x.ats)])#, x.ats[1: (length( x.ats)  - length(1:365))] )
x.labs <- x.ats
clean.x <- cumsum(c(0,31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
clean.xlabs <- clean.x
# adjustable scale from RKMplot
diff.rkm <- (urkm - lrkm)
if( diff.rkm <=100){seq.by <- 10}else
if( diff.rkm > 100 & diff.rkm < 300){seq.by <- 25}else
if( diff.rkm >= 300 ){seq.by<-100}
y.ats <- seq( ylim[1],ylim[2], seq.by )
y.labs <- y.ats
plot( jdays, rkms, xlim=xlim, ylim=ylim, type="n", 
     las=1, axes=F, ann=F, cex=cex, cex.axis=cex.axis, 
     cex.lab=cex.lab )
#abline( h = 411, lty=3, col="grey10")
segments( xlim[1] ,spec.locations, xlim[2] , spec.locations  , lty=3, col="grey10")
segments( clean.x, ylim[1], clean.x, ylim[2]-(.05*urkm),  lty=1, col="black")
points(jdays, rkms, cex=0.7, pch=15 )
axis(1, at=clean.x, labels=F)
axis(4, at=y.ats, labels=F, las=1, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab )
mtext(side=4, line=1, at=y.ats, y.labs, las=1, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab )
mtext(side=1, line=1.5, at=clean.x, clean.xlabs, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab ) # row of julian days
text(clean.x[-c(length(clean.x))]+0, rep( urkm+(.05*urkm),length(clean.x)-1), paste(as.character(Month),year), pos=4, cex=cex+0.2, cex.axis=cex.axis, cex.lab=cex.lab )
text(clean.x[-c(length(clean.x))]+0, rep( (urkm  ),length(clean.x)-1), paste("n =", mfd[,2]), pos=4, cex=cex+0.2, cex.axis=cex.axis, cex.lab=cex.lab )
#text(side=3, line=2, at=clean.x, paste(as.character(Month)) )
mtext(side=2, line=-2, "River Kilometer", cex=cex, cex.axis=cex.axis, cex.lab=cex.lab )
#abline(v=clean.x, lty=1, col="black")

## upper right plot (#2)
par(mar = c(2, 3, 1, 1) + 0.1) # c(bottom, left, top, right)
plot( rtab[,2], rs , type="n", ylim=ylim, 
     ann=F, axes=F, cex=cex, cex.axis=cex.axis, 
     cex.lab=cex.lab )
segments( rep(0,length(rtab[,2])), rs, rtab[,2], rs, lwd=2, col="black" )
segments( xlim[1] ,spec.locations, xlim[2] , spec.locations  , lty=3, col="grey10")
x.ats <- seq(0,max(rtab[,2]),50) 
axis(1, at=x.ats, labels=F, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab )
mtext(side=1, line=1.5, at=x.ats, x.ats, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab )
mtext(side=1, line=3, "Detection Days", cex=cex, cex.axis=cex.axis, cex.lab=cex.lab )
axis(2, at=y.ats, labels=F )
## lower plot (#3)
par(mar = c(2, 2, 1, 2) + 0.1) # c(bottom, left, top, right)
ylim<- c(0, max( numfish ) )
# adjustable scale needed
diff. <- max(numfish)
if( diff. >=0 & diff. < 10){seq.by <- 1}else
if( diff. >= 10 & diff. < 50){seq.by<-5}else
if( diff. >= 100 ){seq.by<-25}
y.ats <- seq( ylim[1],ylim[2], seq.by )
plot( aday, numfish, xlim=xlim, ylim=ylim, type="h", 
     lwd=2, las=1, ann=F, axes=F, cex=cex, cex.axis=cex.axis, 
     cex.lab=cex.lab )
#abline( h = y.ats, lty=3, col="grey10")
#abline(v=clean.x, lty=1, col="black")
segments( xlim[1], y.ats, xlim[2], y.ats,  lty=3, col="grey10")
segments( clean.x, ylim[1], clean.x, ylim[2],  lty=1, col="black")
axis(3, at=clean.x, labels=F)
axis(4, at=y.ats, labels=F, las=1 , cex=cex, cex.axis=cex.axis, cex.lab=cex.lab ) 
mtext(side=4, line=1, at=y.ats, y.ats, las=1 , cex=cex, cex.axis=cex.axis, cex.lab=cex.lab ) 
mtext(side=2, line=-2, "Number / Day", cex=cex, cex.axis=cex.axis, cex.lab=cex.lab )
##================================================== END GRAPH   


     } # end function
# run function
windows()
SpatTempPlot( data = test3,
    year = 2012, urkm = 175,
    lrkm = 0, spec.locations = c(106, 160.69)  )



##
##
##
##
##================================================== BEGIN 

getMovement <- function( x, time.units=c("hours","days"), dist.units=c("km"), rate_filter ) {
            ## getMovment function will return transit rates between monitors - v1 12/1/2011
            ## x = TagID you want to look at
            ## time.units = only option is days
            ## dist.units = only option is km
            ## rate_filter = user-defined maximum movement rate by fish 
            #x <- 48419; time.units <- "days"; dist.units <- "km"; rate_filter=100
            aa <- test[ test$TagID==x, ][,9]
            group <- c(0, cumsum(diff(aa) != 0))
            bb <- tapply(aa, group,)
            cc <- unique(bb)
            arrivals <- departures <- rep( NA, length(cc))
            for( j in 1:length(cc) ) {
               arrivals[j] <- min( which( bb==cc[j] ))
               departures[j] <- max( which( bb==cc[j] ))
            }
            ind <- sort(unique(c( arrivals, departures )))
            ## time of arrivals / departures 
            time.of.arrivals <- strptime(test[ test$TagID==x, ][arrivals, 6], "%m/%d/%Y %H:%M:%S")
            time.of.departures <- strptime(test[ test$TagID==x, ][departures, 6], "%m/%d/%Y %H:%M:%S")
            ## amount of time spent at monitors (from first to last detection for each encounter at a monitor)
            amt.time.spent <- difftime(as.POSIXct(time.of.departures), as.POSIXct(time.of.arrivals), units=time.units)
            amt.time.spent # works 9/28/11
            length( amt.time.spent)
            julian.day.arrival <- time.of.arrivals$yday + 1
            julian.day.departure <- time.of.departures$yday + 1
            num.detections.i <-  (departures - arrivals) + 1
            ## movement rates - currently user - specified
            rkm1 <- test[ test$TagID==x, ][arrivals[1:(length(arrivals)-1)], 9]
            rkm2 <- test[ test$TagID==x, ][departures[2:length(departures)], 9]
            mon1 <- test[ test$TagID==x, ][arrivals[1:(length(arrivals)-1)], 8]
            mon2 <- test[ test$TagID==x, ][departures[2:length(departures)], 8]
            dt1 <- test[ test$TagID==x, ][departures[1:(length(departures)-1)], 6]
            dt2 <- test[ test$TagID==x, ][arrivals[2:length(arrivals)], 6]
            adj.dt1 <- strptime(dt1, "%m/%d/%Y %H:%M:%S")
            adj.dt2 <- strptime(dt2, "%m/%d/%Y %H:%M:%S")
            ## time spent away from monitors
            #amt.time.away <- difftime( adj.dt2[2:length(dt2)], adj.dt1[1:(length(dt1)-1)]   , units=time.units)
            amt.time.away1 <- difftime( adj.dt2, adj.dt1   , units=time.units)
            rkm.diff <- rkm2 - rkm1
            r <- rkm.diff / as.numeric( amt.time.away1 )
            rate <- abs(r)
            ## direction of movement (up or down)
            movement.direction <- ifelse(r<0, "downstream", "upstream") #<0, "downstream", NA))
            screen2 <- which( rate >= rate_filter ) 
            reason2 <- rep("NA", length(rate))
            reason2[ screen2 ] <- paste("Rate_GTE_",rate_filter,"_",dist.units,"_",time.units, sep="")
            tab2 <- as.data.frame(cbind( rep(x,length(rkm1)), rkm1, rkm2, as.character(mon1), as.character(mon2), 
                as.character(dt1), as.character(dt2), rkm.diff , rate, movement.direction, amt.time.away1, reason2))
            ## summarizes movement patterns, rate of movement, direction, and amt. of time away from receivers or detection
            tab2
            AmtOfTimeAway <- paste("AmtOfTimeAway_",time.units, sep="")
            Rate <- paste("Rate_",dist.units,"_per_",time.units, sep="")
            colnames( tab2 )<-c("TagID", "RKM1", "RKM2", "Location1","Location2", "TimeOfDeparture_L1", "TimeOfArrival_L2", "RKMdiff", 
                Rate, "MovementDirection", AmtOfTimeAway, "Potential_FD")
            return(tab2)
}
## test function
a <- getMovement(2841, time.units="days",
    dist.units="km", rate_filter=10)
head(a)
##======================================== BEGIN LOOPING SEQUENCE

tag.ids <- c(56493, 56489, 56488, 56479, 56480, 56474, 2850, 2848, 2849, 2852, 2846, 56477, 2861, 2841, 2851, 2856,56494, 2854,56485,
             2870, 2847, 56486,2844, 2867, 2879, 2872, 2853,2871, 2869, 2868, 2883,
             2886,  56475, 56481, 2882, 56492, 2863, 56490, 2873, 2859, 56478, 2857,
             2878, 2877, 2845, 2876, 2855, 2858, 56491, 2875, 2860, 56483, 2885,
             2843, 56482,2842, 2862, 2866, 2884, 56484, 2880, 56476, 56473, 56487)
##======================================== BEGIN LOOP
# output dataset - loop works - d/n modify
project <- "getMovement_"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S") 
# myfile <- paste( project, mytime, ".csv", sep="")
outfilename <- gsub("( )", "",paste(project, mytime, ".csv"))
for( j in 1:length(tag.ids) ) {  
     b <- getMovement(tag.ids[j], time.units="days", dist.units="km", rate_filter=75)  
     write.table( b, file=outfilename,  quote=F, sep=",", row.names=F, append=T) 
}
ping(2)
b

# clean up workspace #
objects()
rm(b, j, myfile, mytime, other.ids, project, tag.ids)
objects()

##==================================================
# begin plot of reach-specific mean movement rates / mean residence times similar to
# the code used in the rescue GS paper
dir <- "E:/WORK/FISH_TRACKER/outfiles_data"                 # input directory
setwd(dir)
t1 <- read.csv("getMovement_Feb_17_2014_13_56_56.csv", stringsAsFactors=FALSE, header=T)  # load previously written .csv file
ind <- which( t1[,1] == "TagID" )
t1 <- t1[-c(ind),]
head(t1) 
unique(t1[,1])

# write function to calculate standard error
se <- function(x) { sqrt(var(na.exclude(x))/length(na.exclude(x))) }         

unique_moves <- as.data.frame(unique(cbind(t1$RKM1, t1$RKM2)))
unique_moves
nind <- which(is.na(unique_moves[,2]))
nind
unique_moves <- unique_moves[- c(nind), ]

dsind <- which( as.numeric(as.character(unique_moves[,1])) > as.numeric(as.character(unique_moves[,2])) )
dsind
dsum <- unique_moves[dsind,]
dim(dsum)
n <- nrow(dsum)
n
ns_ds <- nfish_ds <- mr_ds <- se_ds <- reach_rkm1 <- reach_rkm2 <- reach_names <-    rt_ds <- rtse_ds <- reach_leng <- rep(NA, n)
for( i in 1:n ) {
      #i<-1
      # summarize downstream moves
      dsd <- subset(t1, t1$RKM1 == as.character(dsum[i,1]) & t1$RKM2 == as.character(dsum[i,2]) ) 
      #par(mfrow=c(1,1));hist(as.numeric(dsd$Rate_km_per_days))
      if(nrow(dsd) == 0) {
            mr_ds[i] <- se_ds[i] <- rt_ds[i] <- rtse_ds[i] <- NA
            ns_ds[i] <-nfish_ds[i] <- 0 }else
      if(nrow(dsd) >= 1) {
            mr_ds[i] <- mean(as.numeric(dsd[,9]))
            ns_ds[i] <- length( dsd[,9] )
            nfish_ds[i] <- length(unique( dsd[,1] ))
            se_ds[i] <- se( as.numeric(dsd[,9]))
            rt_ds[i] <- mean( as.numeric(dsd[,11]))
            rtse_ds[i] <- se( as.numeric(dsd[,11]))
      }
      # summarize reach info
      reach_rkm1[i]     <- as.character(dsum[i,1])
      reach_rkm2[i]     <- as.character(dsum[i,2]) 
      reach_names[i]    <- paste(unique(dsd[,4]),unique(dsd[,5]), collapse=" ")
      reach_leng[i]     <- unique(abs( as.numeric(as.character(dsum[i,1])) - as.numeric(as.character(dsum[i,2]))  ))
  
}
dsmt <- data.frame(reach_names, reach_rkm1, reach_rkm2, reach_leng, ns_ds, nfish_ds, mr_ds, se_ds, rep("downstream",length(se_ds)), rt_ds, rtse_ds )
myColnames <- c( "reach_names", "reach_rkm1", "reach_rkm2", "reach_leng", "NumberMoves",
          "NumFish","MeanMovementRate", "SE", "Direction","MeanResidenceTime","RT_SE")
colnames( dsmt ) <- myColnames
head(dsmt)
dsind
usum <- unique_moves[-c(dsind),]
n <- nrow(usum)
reach_rkm1 <- reach_rkm2 <- reach_names <- reach_leng<- ns_us <- nfish_us <- mr_us <- se_us <- rt_us <- rtse_us<- rep(NA, n)
for( i in 1:n ) {
      # summarize upstream moves
      #i<-1
      usd <- subset(t1, t1$RKM1 == as.character(usum[i,1]) & t1$RKM2 == as.character(usum[i,2]))  
      #par(mfrow=c(1,1));hist(as.numeric(usd$Rate_km_per_days))
      if( nrow(usd) == 0) {
            mr_us[i] <- se_us[i] <- rt_us[i] <- rtse_us[i] <- NA
            ns_us[i] <- nfish_us[i] <- 0 }else
      if( nrow(usd) >= 1) {
            mr_us[i] <- mean(as.numeric(usd[,9]))
            ns_us[i] <- length( usd[,9] )
            nfish_us[i] <- length(unique( usd[,1] ))
            se_us[i] <- se( as.numeric(usd[,9]))
            rt_us[i] <- mean( as.numeric(usd[,11]))
            rtse_us[i] <- se( as.numeric(usd[,11]))
      }
      # summarize reach info
      reach_rkm1[i]     <- as.character(usum[i,1])
      reach_rkm2[i]     <- as.character(usum[i,2]) 
      reach_names[i]    <- paste(unique(usd[,4]),unique(usd[,5]), collapse=" ")
      reach_leng[i]     <- unique(abs( as.numeric(as.character(usum[i,1])) - as.numeric(as.character(usum[i,2]))  ))
      }
usmt <- data.frame(reach_names, reach_rkm1, reach_rkm2, reach_leng, ns_us, nfish_us, mr_us, se_us, rep("upstream",length(se_us)), rt_us, rtse_us )
colnames( usmt ) <- myColnames
head(usmt)
mt <- rbind( dsmt, usmt)
head(mt)

#data.dir <- "E:/WORK/FISH_TRACKER/outfiles_data/"         # output directory - for data
project <- "MovementTable_"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S") 
#myfile <- gsub("( )", "", paste(data.dir, project, mytime, ".csv"))
outfilename <- gsub("( )", "",paste(project, mytime, ".csv"))
write.table( mt, file=outfilename,  quote=F, sep=",", row.names=F, append=F) 
 
# clean up workspace #
objects()
rm(myfile,mytime,dsd,dsind,dsum,i,ind,
mr_ds,mr_us,myColnames,myfile,mytime,n,
nfish_ds,nfish_us,nind,ns_ds,ns_us,project,reach_leng,reach_names,reach_rkm1,
reach_rkm2,se_ds,se_us,unique_moves,usd,rt_us,rt_ds,rtse_us,rtse_ds,
usum)
objects()

head(dsmt)
head(usmt)
head(mt)

# begin plotting section - reach-specific movement rates by reach broken out by direction of move
newmt <- mt[which(mt$NumberMoves >= 5 & mt$MeanMovementRate <=200), ]
f <- newmt$reach_rkm1
nrkm1 <- as.numeric( levels(f))[f]
f1 <- newmt$reach_rkm2
nrkm2 <- as.numeric( levels(f1))[f1]
rkm_tab <- data.frame(nrkm1,nrkm2)
rkm_tab <- rkm_tab[order(nrkm1),]
nmt <- newmt[order(nrkm1),]
head(nmt)
# begin plot
par(mar=c(8,6,2,2))
ylim<-c(0,200)
plot( 1:nrow(nmt), nmt$MeanMovementRate, type="n", ann=F, axes=F, bty="n", ylim=ylim )
for( i in 1:nrow(nmt)) {
points( i, nmt$MeanMovementRate[i], pch=ifelse(nmt$Direction[i]=="upstream", 15, 19),
      col=ifelse(nmt$Direction[i]=="upstream", "red", "blue") )
arrows( i, nmt$MeanMovementRate[i], i, nmt$MeanMovementRate[i] + nmt$SE[i], leng=0.1, angle=90,
      col=ifelse(nmt$Direction[i]=="upstream", "red", "blue"))
arrows( i, nmt$MeanMovementRate[i], i, nmt$MeanMovementRate[i] - nmt$SE[i], leng=0.1, angle=90,
      col=ifelse(nmt$Direction[i]=="upstream", "red", "blue"))
}
x.ats <- 1:nrow(nmt)
x.labs <- paste(round(rkm_tab[,1],1),  round(rkm_tab[,2],1) )
y.ats <- y.labs <- seq(ylim[1],ylim[2],25)
axis(side=1, at=x.ats, labels=F )
axis(side=2, at=y.ats, labels=y.labs, las=1)
mtext(side=1, line=1, at=round(x.ats,2), x.labs, las=2)
mtext(side=2, line=3, "Movement Rate (km/day)" )
# end plot

# simple histogram to check distribution of mean movement rates (remember, these are trimmed already!) 
windows()
par(mfrow=c(2,1))
xlim <- c(0,200) # adjust to your fish movement rates
breaks <- seq(xlim[1], xlim[2], 10 )
hist(nmt$MeanMovementRate[nmt$Direction=="upstream"], breaks=breaks)
hist(nmt$MeanMovementRate[nmt$Direction=="downstream"], breaks=breaks)
# end plot

##================================================== BEGIN
# begin section to: 
# 1) DETERMINE whether fish are traveling together
#    e.g. arriving or departing monitors within the same (hour or day)
#    write function using getMovement data (see t1 data above) AND:
# 2) DETERMINE if movement more likely at certain times (e.g. at night, dusk, dawn),
#    will serve as good first step for diel analysis ( can link solar times in here from 
#    maptools package

dir <- "E:/WORK/FISH_TRACKER/outfiles_data"                 # input directory
setwd(dir)  
t1 <- read.csv("getMovement_Feb_20_2014_10_30_22.csv", stringsAsFactors=FALSE, header=T)  # load previously written .csv file
ind <- which( t1[,1] == "TagID" )
NAind <- which(is.na(t1$TimeOfArrival_L2))
t1 <- t1[-c(ind, NAind),]
head(t1)
unique(t1[,1])
length(unique(t1[,1]))
unique.locs <- sort(unique(as.numeric(na.exclude(t1$RKM2))))
location.table <- data.frame(1:length(unique.locs), unique.locs)
location.table

# enter number from table above to get that specific data from monitor
fishPulse <- function( x, time_units=c("hours","days"), crit) {
#which.monitor <- 79  # enter index number of RKM from list
      #x <- 15; time_units = "days"; crit=2
      nt1 <- t1[t1$RKM2==unique.locs[x], ]
      # sort by as.POSIX(als) # look at arrivals first
      als <- strptime( nt1$TimeOfArrival_L2,"%m/%d/%Y %H:%M:%S")
      newt1 <- nt1[order( as.POSIXct( als )),]
      head(newt1)
      nals <- strptime( newt1$TimeOfArrival_L2,"%m/%d/%Y %H:%M:%S")
      if( time_units == "hours" ) {
      aa <- (nals$year + 1900) + nals$yday + (nals$hour / 100) + 0.1
      #data.frame(aa, as.character(newt1$TimeOfArrival_L2))
      group <- c(0, cumsum(diff(aa) != 0))
      bb <- tapply(aa, group,)
      cc <- na.exclude(unique(bb))
      numberHour <- monList <- tagList <- rkmList <- toaList <- hodList <- diffFish <- vector( 'list', length(cc) )
      for( j in 1:length(cc) ) {
           #j<-1
           ind <- which( bb==cc[j] )
           newt1[ind,]
           numberHour[[j]] <- rep( length( ind ), length(ind) )
           monList[[j]] <- rep( unique(newt1$Location2), length( ind ) )
           rkmList[[j]] <- rep( unique.locs[x], length( ind ))
           tagList[[j]] <- newt1$TagID[ind]
           toaList[[j]] <- newt1$TimeOfArrival_L2[ind]
           hodList[[j]] <- nals$hour[ind]
           diffFish[[j]] <- rep( ifelse( length(unique(tagList[[j]])) >= crit, "True","False"), length(ind))
           #dayList[[j]] <- nals$yday[ind]
           }
          numberhour <- unlist( numberHour )
          monlist    <- unlist( monList    )
          taglist    <- unlist( tagList    )
          rkmlist    <- unlist( rkmList    )
          toalist    <- unlist( toaList    )
          hodlist    <- unlist( hodList    )
          difffish   <- unlist( diffFish   )
          a <- data.frame( numberhour, monlist, taglist, rkmlist, toalist, hodlist, difffish )
          #a <- a[-c( which(duplicated(a)) ),]
          hourNames <- c("NumHour","Monitor","TagIDs","RKM","TimeOfArrival","HourOfDay","DifftFish" )
          colnames(a) <- hourNames
          if( length( which(duplicated(a)) ) < 1){return(a)}else
          if( length( which(duplicated(a)) ) >= 1){
          a <- a[-c( which(duplicated(a)) ),]
          return(a)
       }
}else
      if( time_units == "days" ) {
      #x <- 14; time_units = "days"; crit=2#x <- 79
      aa <- (nals$year + 1900) + nals$yday
      group <- c(0, cumsum(diff(aa) != 0))
      bb <- tapply(aa, group,)
      cc <- na.exclude(unique(bb))
      numberDay <- monList <- tagList <- rkmList <- toaList <- hodList <- diffFish <- unFish <- vector( 'list', length(cc) )
      for( j in 1:length(cc) ) {
           #j<-11
           ind <- which( bb==cc[j] )
           newt1[ind,]
           numberDay[[j]] <- rep( length( ind ), length(ind) )
           monList[[j]] <- rep( na.exclude(unique(newt1$Location2)), length( ind ) )
           rkmList[[j]] <- rep( unique.locs[x], length( ind ))
           tagList[[j]] <- newt1$TagID[ind]
           toaList[[j]] <- newt1$TimeOfArrival_L2[ind]
           hodList[[j]] <- nals$hour[ind]
           diffFish[[j]] <- rep( ifelse( length(unique(tagList[[j]])) >= crit, "True","False"), length(ind))
           #dayList[[j]] <- nals$yday[ind]
           }
          numberday  <- unlist( numberDay  )
          monlist    <- unlist( monList    )
          taglist    <- unlist( tagList    )
          rkmlist    <- unlist( rkmList    )
          toalist    <- unlist( toaList    )
          hodlist    <- unlist( hodList    )
          difffish   <- unlist( diffFish   )
          a <- data.frame( numberday, monlist, taglist, rkmlist, toalist, hodlist, difffish )
          dayNames  <- c("NumDay","Monitor","TagIDs","RKM","TimeOfArrival","HourOfDay","DifftFish" )
          colnames(a) <- dayNames
          if( length( which(duplicated(a)) ) < 1){return(a)}else
          if( length( which(duplicated(a)) ) >= 1){
          a <- a[-c( which(duplicated(a)) ),]
          return(a)
       }
   }

} # end function
# test it!
location.table
location.table[1,]
a <- fishPulse(1, time_units="days", crit=5)
b <- fishPulse(1, time_units="hours", crit=2)
a
write.csv(b, "BCEW2.csv")

fishPulsePlot <- function( data, crit=crit) {
      # begin plotting section
      # set up for upper plot - done ("a" contains all necessary info)
      # set up for lower plot
      # by day
      if( dimnames(data)[[2]][1] == "NumDay"){
      ndts <- strptime(data$TimeOfArrival, "%m/%d/%Y %H:%M:%S")
      nday <- as.numeric(format(ndts, "%Y.%j"))
      nday
      aa <- nday
      group <- c(0, cumsum(diff(aa) != 0))
      bb <- tapply(aa, group,)
      cc <- na.exclude(unique(bb))
      uniqueDay <- d <- rep( NA, length(cc) )
      for( j in 1:length(cc) ) {
           #j<-11
           ind <- which( bb==cc[j] )
           uniqueDay[j] <- length(unique( a$TagIDs[ind] ))
           sdts <- strptime(a$TimeOfArrival[ind], "%m/%d/%Y")
           d[j] <- as.character(unique( sdts ))
           if( j == length(cc)) { d <- unlist( d )}
           }
      }else
      if( dimnames(data)[[2]][1] == "NumHour"){
      ndts <- strptime(data$TimeOfArrival, "%m/%d/%Y %H:%M:%S")
      nday <- as.numeric(format(ndts, "%Y%j.%H"))
      aa <- nday
      group <- c(0, cumsum(diff(aa) != 0))
      bb <- tapply(aa, group,)
      cc <- na.exclude(unique(bb))
      uniqueDay <- d <- rep( NA, length(cc) )
      for( j in 1:length(cc) ) {
           #j<-11
           ind <- which( bb==cc[j] )
           uniqueDay[j] <- length(unique( data$TagIDs[ind] ))
           sdts <- strptime(data$TimeOfArrival[ind], "%m/%d/%Y %H")
           d[j] <- as.character(unique( sdts ))
           if( j == length(cc)) { d <- unlist( d )}
           }

       }
       # end set up for lower plot
          # begin plotting
          # SHOW NUMBER OF FISH PER HOUR FIRST
          toas <- strptime( data$TimeOfArrival,"%m/%d/%Y %H:%M:%S")
          #crit <- 2   # user-defined criteria of number of fish in specified period
          par(mar=c(2,3,0,1)) #  c(bottom, left, top, right)
          par(mfrow=c(2,1))
          ylim <- c(0, max(data[,1])+crit+1)
          plot(toas, data[,1], type="n", ylim=ylim, las=1, ann=F, axes=F )
          points(toas, data[,1], type="h")
          time.int <-c(toas[1], toas[nrow(data)])
          rs <- as.POSIXct(time.int)
          at <- seq(rs[1], rs[2], by="days")
          at1 <- seq(rs[1], rs[2], by="hours")
          points(  at1, rep(0,length(at1)), type="h" )
          axis.POSIXct(1, at=at, format="%x", tcl = -0.5 )
          ind <- which( a[,1] >= crit)
          text( toas[1], max(data[,1])+crit, paste("( RKM", unique(data$RKM),")", sep=" "), pos=4 ) #unique(data$Monitor),
          axis(2, at=seq(ylim[1], ylim[2], 1), labels= seq(ylim[1], ylim[2], 1), las=1 )
          which.lab <- dimnames(a)[[2]][1]
          mtext( side=2, line=2, ifelse(which.lab == "NumDay", "Number of Arrivals per Day", "Number of Arrivals per Hour") )
          # plot number of unique fish per time period
          #par(mar=c(4,3,2,1))
          ylim <- c(0, max(data[,1])+crit+1)
          plot(as.POSIXct(d), uniqueDay, type="n", ylim=ylim, las=1, ann=F, axes=F )
          points(as.POSIXct(d), uniqueDay, type="h")
          time.int <-c(d[1], d[length(d)])
          rs <- as.POSIXct(time.int)
          at <- seq(rs[1], rs[2], by="days")
          at1 <- seq(rs[1], rs[2], by="hours")
          points(  at1, rep(0,length(at1)), type="h" )
          axis.POSIXct(1, at=at, format="%x", tcl = -0.5 )
          ind <- which( data[,1] >= crit)
          #text( toas[1], max(data[,1])+crit, paste(unique(data$Monitor), "( RKM", unique(data$RKM),")", sep=" "), pos=4 )
          axis(2, at=seq(ylim[1], ylim[2], 1), labels= seq(ylim[1], ylim[2], 1), las=1 )
          which.lab <- dimnames(data)[[2]][1]
          mtext( side=2, line=2, ifelse(which.lab == "NumDay", "Number of Unique Fish per Day", "Number of Unique Fish per Hour") )
          }# end plotting
fishPulsePlot( b, crit=2 )

head(a)
length(unique(a$TagIDs))

# figure out number of UNIQUE fish each time unit
ndts <- strptime(a$TimeOfArrival, "%m/%d/%Y %H:%M:%S")
nday <- as.numeric(format(ndts, "%Y.%j"))
nday
aa <- nday
      group <- c(0, cumsum(diff(aa) != 0))
      bb <- tapply(aa, group,)
      cc <- na.exclude(unique(bb))
      uniqueDay <- d <- rep( NA, length(cc) )
      for( j in 1:length(cc) ) {
           #j<-11
           ind <- which( bb==cc[j] )
           uniqueDay[j] <- length(unique( a$TagIDs[ind] ))
           sdts <- strptime(a$TimeOfArrival[ind], "%m/%d/%Y")
           d[j] <- as.character(unique( sdts ))
           if( j == length(cc)) { d <- unlist( d )}
           }
uniqueDay
d
head(aa)
plot( as.POSIXct(d), uniqueDay, type="h" )

head(a)
write.csv(a, "BCEW_uniquearrivals", row.names=F, col.names=T)
#b <- fishPulse( 79, time_units="hours", crit=2)
#head(b)
#dim(b)
# 1) DETERMINE whether fish are traveling together
#    e.g. arriving or departing monitors within the same (hour or day)
#    write function using getMovement data (see t1 data above) AND:
# BEGIN PLOT
# SHOW NUMBER OF FISH PER HOUR FIRST
toas <- strptime( a$TimeOfArrival,"%m/%d/%Y %H:%M:%S")
crit <- 2   # user-defined criteria of number of fish in specified period
par(mar=c(2,3,0,1)) #  c(bottom, left, top, right)
par(mfrow=c(2,1))
ylim <- c(0, max(a[,1])+crit+1)
plot(toas, a[,1], type="n", ylim=ylim, las=1, ann=F, axes=F )
points(toas, a[,1], type="h")
time.int <-c(toas[1], toas[nrow(a)])
rs <- as.POSIXct(time.int)
at <- seq(rs[1], rs[2], by="days")
at1 <- seq(rs[1], rs[2], by="hours")
points(  at1, rep(0,length(at1)), type="h" )
axis.POSIXct(1, at=at, format="%x", tcl = -0.5 )
ind <- which( a[,1] >= crit)
text( toas[1], max(a[,1])+crit, paste(unique(a$Monitor), "( RKM", unique(a$RKM),")", sep=" "), pos=4 )
axis(2, at=seq(ylim[1], ylim[2], 1), labels= seq(ylim[1], ylim[2], 1), las=1 )
which.lab <- dimnames(a)[[2]][1]
mtext( side=2, line=2, ifelse(which.lab == "NumDay", "Number of Arrivals per Day", "Number of Arrivals per Hour") )
# plot number of unique fish per time period
#par(mar=c(4,3,2,1))
ylim <- c(0, max(a[,1])+crit+1)
plot(as.POSIXct(d), uniqueDay, type="n", ylim=ylim, las=1, ann=F, axes=F )
points(as.POSIXct(d), uniqueDay, type="h")
time.int <-c(d[1], d[length(d)])
rs <- as.POSIXct(time.int)
at <- seq(rs[1], rs[2], by="days")
at1 <- seq(rs[1], rs[2], by="hours")
points(  at1, rep(0,length(at1)), type="h" )
axis.POSIXct(1, at=at, format="%x", tcl = -0.5 )
ind <- which( a[,1] >= crit)
text( toas[1], max(a[,1])+crit, paste(unique(a$Monitor), "( RKM", unique(a$RKM),")", sep=" "), pos=4 )
axis(2, at=seq(ylim[1], ylim[2], 1), labels= seq(ylim[1], ylim[2], 1), las=1 )
which.lab <- dimnames(a)[[2]][1]
mtext( side=2, line=2, ifelse(which.lab == "NumDay", "Number of Unique Fish per Day", "Number of Unique Fish per Hour") )



# add legend (using mtext function)
par(new=T)
par(mar=c(4,3,2,11))
text.labs <- paste( a$TagIDs[ind], " at ", toas[ind])
plot( 1:length( text.labs ), 1:length( text.labs ), type="n", ann=F, axes=F)
y.ats <- length( text.labs ):1
mtext( side=4, line=1, at=y.ats, text.labs , las=2, cex=0.8 )
which.lab <- dimnames(a)[[2]][1]
mtext( side=2, line=2, ifelse(which.lab == "NumDay", "Number of Fish per Day", "Number of Fish per Hour") )
mtext( side=1, line=2.1, "Date" )
# END PLOT

# 2) DETERMINE if movement more likely at certain times (e.g. at night, dusk, dawn),
#    will serve as good first step for diel analysis ( can link solar times in here from 
#    maptools package
# BEGIN PLOT

hist( hodlist, breaks=seq(0,24,1)) # hour of day of arrival

# END PLOT




##================================================== BEGIN     
## get RKM plot - v1 - options in function are just a zoom in/out
RkmPlot <- function(x, Urkm, Lrkm, time1, time2, specific.locations) {
              ## x     = TagID you want to look at
              ## Urkm  = upper rkm of reach
              ## Lrkm  = lower rkm of reach
              ## time1 = oldest time (just before tagging typically)
              ## time2 = newest time
              ## specific.locations = highlights specific locations of interest 
              cex <-cex.axis <- cex.lab<- 0.8
              aa <- test[ test$TagID==x, ][,9]
              group <- c(0, cumsum(diff(aa) != 0))
              bb <- tapply(aa, group,)
              cc <- unique(bb)
              arrivals <- departures <- rep( NA, length(cc))
              for( j in 1:length(cc) ) {
                 arrivals[j] <- min( which( bb==cc[j] ))
                 departures[j] <- max( which( bb==cc[j] ))
              }
              ind <- sort(unique(c( arrivals, departures )))
              rkm.times  <- strptime(test[ test$TagID==x, ][ind, 6], "%m/%d/%Y %H:%M:%S")
              ylim <- c(Lrkm, Urkm)
              diff.rkm <- (Urkm - Lrkm)
              if( diff.rkm <=100){seq.by <- 10}else
              if( diff.rkm > 100 & diff.rkm < 300){seq.by <- 25}else
              if( diff.rkm >= 300 ){seq.by<-100}
              #seq.by <- c( 10, 25, 100 )
              y.ats <- seq( ylim[1],ylim[2], seq.by )
              y.labs <- y.ats
              xlim <- c(as.POSIXct(time1), as.POSIXct(time2) )
              par(mar=c(2,2.8,1,1))    # c(bottom, left, top, right) 
              plot( rkm.times, test[ test$TagID==x, ][ind, 9], xlim=xlim, ylim=ylim,  las=1, ann=F, axes=F, type="b")
              abline(h=c(specific.locations), lty=2:length(specific.locations) )
              rs <- as.POSIXct( xlim )
              at1 <- seq(rs[1], rs[2], by="months")
              axis.POSIXct(1, at=at1, labels=T, format="%m/%y")
              axis(2, at=y.ats, labels=y.labs, cex=cex, cex.lab=cex.lab, cex.axis=cex.axis, las=1)
              mtext(side=2, line=2.7, "River Kilometer", cex=cex)
              mtext(side=1, line=2, "Date", cex=cex)
              mtext(side=3, line=-2, paste(x), cex=cex )
}
## test 
RkmPlot(2841, Lrkm=100, Urkm=170, time1 = "2012-03-01 00:00:00", 
    time2 = "2012-06-01 00:00:00", specific.locations=c(225, 285, 411.8))
##================================================== END
##
##
##
##

getResidence <- function( x,  group_by=c("rkm","monitor_name") ) {
  # x = tag you want to look at
  # group_by = element you want to group by (options by RKM or monitor)
  #x = 2841; group_by="rkm"# "monitor_name" #  "rkm"
  stest <- subset( test, test$TagID == x )
  if( group_by=="monitor_name" ){ us <- sort(unique(stest$Location)) }else
    if( group_by=="rkm" ){ us <- sort(unique(stest[,9])) }
  for( j in 1:length(us)) {
    #j <- 15
    # deal with the two options (the group_by option - either rkm or site)
    if( class(us) == "numeric" ){
      ind <- which( stest$RiverKm == us[j] )}else
        if( class(us) == "factor" ){
          ind <- which( stest$Location == us[j] )}
    ntest <- stest[ind,]
    interval <- rep(NA, nrow(ntest))
    for( k in 1:(length(interval)-1)) {
      
      d <- as.character(ntest[,6])
      dts <- strptime(d,"%m/%d/%Y %H:%M:%S")
      #k <- 1
      interval[k] <-as.numeric(difftime( as.POSIXct(dts[k+1]) ,
                                         as.POSIXct(dts[k]), units="days" ))
    }
    break_inds <- which( interval > 1 )
    istart <- 1
    iend   <- nrow( ntest ) - 1
    all_inds <- sort( c( istart, iend, break_inds ))
    crt_inds <- data.frame( all_inds[1:(length(all_inds)-1)]+1,
                            all_inds[2:length(all_inds)] )
    crt_inds[1,1] <- 1
    # this happens when a single last detection occurs at a monitor
    if( length( which( crt_inds[,1] > crt_inds[,2] ) ) > 0 ){
      ind <- which( crt_inds[,1] > crt_inds[,2] )
      crt_inds[ind,2] <- crt_inds[ind,1]
    }
    colnames(crt_inds) <- c("starts", "stops")
    crtimes <- rep(NA, nrow(crt_inds))
    for( m in 1:nrow(crt_inds)){
      #m <- 3
      crtimes[m]<- as.numeric(difftime( as.POSIXct(dts[ crt_inds$stops[m]]) ,
                                        as.POSIXct(dts[crt_inds$starts[m]]), units="days" ))
    }
    if( j == 1){
      CRT <- data.frame( rep(x, nrow(crt_inds)), rep( us[j], nrow(crt_inds)),
                         round(crtimes,6), dts[crt_inds$starts] )}
    if( j > 1) {
      CRT <- rbind( CRT , data.frame( rep(x, nrow(crt_inds)), rep( us[j],
                                                                   nrow(crt_inds)), round(crtimes,6), dts[crt_inds$starts] ))}
  }
  colnames(CRT) <- c("tagid", "location", "CRT","CRTstart")
  return( CRT )
} # end function
# test function - will return Continuous Residence Times (CRT) (in days)
# EXAMPLE CODE #
a <- getResidence( 2841,  group_by="monitor_name" )
a
#b <- getResidence( 46617,  group_by="monitor_name" )
##-------------------------------------------------------------------------- END
a <- getResidence( 13715,  group_by="rkm" )
b <- getResidence( 13715,  group_by="monitor_name" )
a
b
tag.ids <- sort(unique(test$TagID))
tag.ids
#output dataset - Myfanwy's tinkering
project <- "getResidence_"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S") 
myfile <- paste( project, mytime, ".csv", sep="")
for( j in 1:length(tag.ids) ) {  
  b <- getResidence(tag.ids[j], group_by="rkm")
  write.table( b, file=myfile,  quote=F, sep=",", row.names=F, append=T)
}
ping(5)
b
## Remove header rows:

R1 <- read.csv("getResidence_Jun_06_2014_09_37_39.csv", stringsAsFactors=FALSE, header=T)  # load previously written .csv file
R1[1:10,]
     ind <- which( R1[,1] == "tagid" )
R1 <- R1[-ind,]
R1[1:10,]
unique(R1[,1])

## write file with header rows removed:
write.csv(R1, "getResidence_fca.csv", row.names=F)









##================================================== BEGIN  #Matt says it doesn't work #

PlotIt <- function( x, time.units=c("hours","days"), dist.units=c("km"), min_detection_rule, top ) {
              ## Insert "getReceiverData()" here
              #x<-29; time.units="days"; dist.units="km"; top=3
              dat <- getReceiverData(x, time.units=time.units, dist.units=dist.units, min_detection_rule = min_detection_rule, time_frame="one_year", YEAR=2012 )
              latitude <- dat[,12]; longitude <- dat[,13]; site.labels <- dat[,2]
              xlim <- c(-123, -121); ylim <- c(37, 42)
              pcex <- dat[,11] / sum( dat[,11]) 
              plot( longitude, latitude, las=1, xlim=xlim, ylim=ylim, cex=pcex*20  )
              top1 <- ifelse( (length(pcex) <= top)==TRUE, length(pcex), top)
              topind <- which( dat[,11] >= sort( dat[,11], decreasing=T )[top1] )
              text( longitude[topind], latitude[topind], c(1:length(topind)), pos=4)
              legend( -123, 42, paste( 1:length(topind),"=", site.labels[topind], ",",dat[,11][topind],"detections"), pch=NA, bty="n" )
              mtext( side=3, line=-1.5, paste(x) )
}
## test 
PlotIt(2841, time.units="days", dist.units="km",  min_detection_rule=5, top=5 )
# error message from 2/17: "Error in meanPdet^num0s : 'meanPdet' is missing "
##================================================== END 
##
##
##
##
## SUMMARIZE DETECTION DAYS BASED ON SUMMARIZED MONITOR DATA
## FROM getDetDays function NEW PLOT IT
## SET INPUT / OUTPUT LOCATIONS
dir <- "E:/WORK/FISH_TRACKER/outfiles_data"                 # input directory
setwd(dir)
t3 <- read.csv("getDetDays_Dec_06_2011_15_31_43.csv", stringsAsFactors=FALSE, header=T)  # load previously written .csv file
#t3 <- read.csv(outfilename, stringsAsFactors=FALSE, header=T)                                # load current written .csv file
ind <- which( t3[,1] == "TagID" )
test3 <- t3[-c(ind),]


## using packages: maps and mapproj - works 
head(newltab) # do not remove - see new mapping function below #
range(newltab$lon) 
range(newltab$lat)
par(mfrow=c(1,1))
#par(mar=c(0,0,0,0))
xlim <- c( -123.2, -121.4) # lon 
ylim <- c( 37.2, 40.5)     # lat
m <- map("state", plot=F, xlim=xlim, ylim=ylim)
map("state", xlim=xlim, ylim=ylim, 
    names=F, col=c("grey90", "white"), fill=T)
points( newltab$lon, newltab$lat, col = "red", pch = 20)
pointLabel(newltab$lon, newltab$lat, as.character(newltab$num_detection_days), cex=0.7, offset=0)
box()
map.axes()
map.scale( relwidth = 0.25, metric = TRUE, ratio = TRUE)
map.grid(m, nx=5,ny=5,labels=F,pretty=TRUE,cex=0.7,col="black",lty=3)



## WORKING SECTION FOR FUNCTIONS: LinkData ( for basic linking info - ( link fish data to detections ))
##                                LinkEnvData ( for linking env. data to fish detections )
## LinkData FUNCTION

# simple case
tags <- sort( rep( 1:10, 12 ) )
utags <- 1:10
fls <- c( 124, 153, 142, 55, 66, 77, 45, 46, 34, 125 )
biodata <- data.frame( utags, fls )
biodata
# link tags and biodata

# for vector ## can't get to work 2/6/12 ##
LinkData <- function( data1, data2, new.vector, pos1, pos2  ) {
    data1 = biodata; data2 = tags; pos1 = 1; pos2 = 2
    #i <- 1
    new.vector <- rep( NA , length(  data2  ))
    for( i in 1:length( data2 )) {
    i <- 2
    ind1 <- which( data2 == data1[i,pos1] )
    ind1
    ind2 <- which( data1[ ,pos1] == data2[i] )
    ind2
    new.vector[ ind1 ] <- data1[ ind2, pos2 ]
    print( new.vector )
    }
    ldf <- data.frame( data2, new.vector )
    return( ldf )
}
# test function

LinkData( data1 = biodata, data2 = tags, new.vector = "linked.fls", pos1 = 1, pos2 = 2  )







# complex case
#### Link the site info with the dailycounts
Island <- rep( NA, nrow(dailycounts)   )
  Island

for ( i in 1:nrow (siteinfo)) {

 ##  i <- 1              ##### this is a test to set i as 1 just to run the script. If it works, remove####
    ind1 <- which ( dailycounts$rkm == siteinfo$Site[i]   )
    ind1
    ind2 <-which ( siteinfo$Site ==siteinfo$Site[i])
    ind2
    Island [ ind1 ] <- siteinfo [ind2, 1]
     }


Islandcounts <- data.frame(dailycounts, Island)


tags <- rep( 1:3, 4 )
utags <- 1:3
fls <- c( 124, 153, 142 )
biodata <- data.frame( utags, fls )
biodata

sort(tags)
linked.fls <- rep( NA , length(tags)  )
linked.fls
for( i in 1:length( utags )) {
    #i <- 3
    ind1 <- which( tags == utags[i] )
    ind1
    ind2 <- which( utags == utags[i] )
    ind2
    linked.fls[ ind1 ] <- biodata[ ind2, 2 ]
    print( linked.fls )
}
linked.fls







## LinkEnvData FUNCTION ( for linking env. data to fish detections )

# start section of code to link monitor data (specific location(s)) to water quality / quantity
# data from CDEC stations or UCD maintained sites
x <- "SJ_MedfordChannel"
aa <- test[ test$Location==x , ]
head(aa)
new.date <- paste(aa$DetectDate)
new.date.time <- strptime(new.date, "%m/%d/%Y %H:%M:%S")
head(new.date)
head(new.date.time)

##==============================================================================
## wq data
wq_pri <- read.csv("PRI_wqdata.csv", stringsAsFactors=FALSE, header=T )    # water quality at CDEC station PRI (Prisoners Point on SJ river)
#attach(flows)
head(wq_pri)
dim(wq_pri)
n.date.time <- paste(wq_pri$date,wq_pri$time)
date.time <- strptime(n.date.time, "%m/%d/%Y %H:%M")
head(date.time)



##==============================================================================
# convert dates/times to seconds after fixed date/time
origin <- "2009-01-01 00:00:00"
dettime.secs  <- as.numeric( difftime(new.date.time, origin, units="secs"))     ## detections
flowtime.secs   <- as.numeric( difftime(date.time, origin, units="secs"))       ## flows / wq parameters


syncd.flows <- as.data.frame(matrix(rep(NA, nrow(aa)*(ncol(wq_pri)-2)), byrow=T,ncol=(ncol(wq_pri)-2)))
head(syncd.flows)
for( i in 1:length(new.date.time))  {
#i <- 1  # index of detections
  #for( j in 1:(length(Deg.C)-1)) {
      ## narrow "search" by matching Reach (flows) and River.Reach (detections)
      ## and by only searching a given interval around specific detection date
      detection.range <- c( dettime.secs[i] - 1800, dettime.secs[i] + 1800)
      detection.range
      eff1 <- which( flowtime.secs >= detection.range[1] & flowtime.secs <= detection.range[2] )
      eff1  # index of flows

      min.abs <- which.min( abs( flowtime.secs[eff1] - dettime.secs[i] ))

      syncd.flows[i,]  <- wq_pri[ eff1[min.abs] , c(3:ncol(wq_pri)) ]

      }


##============= CONVERT CONDUCTIVITY TO SALINITY ===============================
## Author Alan D. Jassby and James E. Cloern
#Maintainer Alan Jassby <adjassby@ucdavis.edu>
#Description Functions to assist in the processing and exploration of
#data from monitoring programs for aquatic ecosystems. The focus
#is on time series data for physical and chemical properties of
#water, as well as the plankton. The package is intended for
#programs that sample approximately monthly at discrete stations.

#ec2pss converts electrical conductivity data to salinity using the Practical Salinity Scale 1978 in
#the range of 2-42 (Fofonoff and Millard 1983). Salinities below 2 are calculated using the extension
#of the Practical Salinity Scale (Hill et al. 1986).
#R2pss is the same function, except that conductivity ratios rather than conductivities are used as
#input.
##============================
## load package "wq"
library(wq)
temp_c <- (as.numeric(syncd.flows$temp_f) - 32) *(5/9)
salinity_ppt <- ec2pss( as.numeric(syncd.flows$cond_uscm)/1000, temp_c, rep(0,length(temp_c)))
discharge_m3s <-  (as.numeric(syncd.flows[,2]) / 35.31467)
swq <- data.frame( syncd.flows, discharge_m3s, temp_c, salinity_ppt)
      colnames(swq) <- c(names(wq_pri)[3:ncol(wq_pri)] , "discharge_m3s", "temp_c", "sal_ppt" )

new_aa <- data.frame( aa, swq )
head( new_aa )

pri_temp_c <- (as.numeric(wq_pri$temp_f) - 32)*(5/9)
pri_sal <- ec2pss( as.numeric(wq_pri$cond_uscm)/1000, pri_temp_c, rep(0,length(pri_temp_c)))

#class( new_aa$DetectDate )
new_dt <- strptime(as.character(new_aa$DetectDate), "%m/%d/%Y %H:%M:%S")
head(new_dt)
# plot linked wq parameters
par(mfrow=c(2,2))
plot( new_dt, new_aa$temp_c, pch=19, cex=0.8)
plot( new_dt, new_aa$discharge_m3s, pch=19, cex=0.8)
plot( new_dt, new_aa$sal_ppt, pch=19, cex=0.8)
plot( new_dt, new_aa$turb_ntu, pch=19, cex=0.8)

# plot wq_pri data with detections overlayed
par(mfrow=c(2,2))
head(wq_pri)
plot( date.time, pri_temp_c , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$temp_c, pch=19, cex=0.8)
plot( date.time, (as.numeric(wq_pri$Qcfs) / 35.31467) , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$discharge_m3s, pch=19, cex=0.8)
plot( date.time, pri_sal , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$sal_ppt, pch=19, cex=0.8)
plot( date.time, wq_pri$turb_ntu , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$turb_ntu, pch=19, cex=0.8)



##====================================================
## wq data
wq_mrz <- read.csv("MRZ_wqdata.csv", stringsAsFactors=FALSE, header=T )    # water quality at CDEC station MRZ (Martinez Marina)
#attach(flows)
head(wq_mrz)
dim(wq_mrz)
n.date.time <- paste(wq_mrz$date,wq_mrz$time)
date.time <- strptime(n.date.time, "%m/%d/%Y %H:%M")
head(date.time)

head(test)
x <- 51.69
bb <- test[ test$RiverKm==x , ]
head(bb)
new.date <- paste(bb$DetectDate)
new.date.time <- strptime(new.date, "%m/%d/%Y %H:%M:%S")
head(new.date)
head(new.date.time)

##==============================================================================
# convert dates/times to seconds after fixed date/time
origin <- "2009-01-01 00:00:00"
dettime.secs  <- as.numeric( difftime(new.date.time, origin, units="secs"))     ## detections
flowtime.secs   <- as.numeric( difftime(date.time, origin, units="secs"))       ## flows / wq parameters


syncd.flows <- as.data.frame(matrix(rep(NA, nrow(bb)*(ncol(wq_mrz)-2)), byrow=T,ncol=(ncol(wq_mrz)-2)))
head(syncd.flows)
for( i in 1:length(new.date.time))  {
#i <- 1  # index of detections
  #for( j in 1:(length(Deg.C)-1)) {
      ## narrow "search" by matching Reach (flows) and River.Reach (detections)
      ## and by only searching a given interval around specific detection date
      detection.range <- c( dettime.secs[i] - 1800, dettime.secs[i] + 1800)
      detection.range
      eff1 <- which( flowtime.secs >= detection.range[1] & flowtime.secs <= detection.range[2] )
      eff1  # index of flows

      min.abs <- which.min( abs( flowtime.secs[eff1] - dettime.secs[i] ))

      syncd.flows[i,]  <- wq_mrz[ eff1[min.abs] , c(3:ncol(wq_mrz)) ]
colnames(syncd.flows) <- c(names(wq_mrz)[3:ncol(wq_mrz)])
      }

head(syncd.flows)











## IDEA FOR LONG-TERM ADULT GREEN STURGEON PROJECT
## CALCULATE REACH USAGE INDICES (RUI) BASED ON SUMMARIZED MONITOR DATA
g.reaches <- reach.rkms <- reach.rkmsB <- reach.len <- rep(NA, length(cc)-1)
for( i in 1:length(g.reaches)) {
      g.reaches[i] <- paste( g.loc[i], "to", g.loc[i+1], collapse=" ")
      reach.rkms[i] <- paste( g.rkm[i], "to", g.rkm[i+1], collapse=" ")
      reach.rkmsB[i] <- paste( g.rkm[i+1], "to", g.rkm[i], collapse=" ")
      reach.len[i] <- as.numeric(g.rkm[i+1]) - as.numeric(g.rkm[i])
}
reach_table <- as.data.frame(cbind(1:length(g.reaches), g.reaches, reach.rkms, reach.rkmsB, round(reach.len,2) ))
colnames( reach_table) <- c("ReachNumber","ReachLimits","ReachRkms","ReachRkmsB","ReachLength")
reach_table

## BEGIN of RUI analysis section for whole season
# subset data to pull out only detections for 2011 - mdata already subset to be only
# detections above rkm 411
t2 <- subset( mdata, Year==2011 & Detected==1 )
head(t2)
# sum up all the time away for all fish over the season
tag.ids <- as.numeric(unique(t2$TagID))
tag.ids
reachList <- amtTimeList <- tagList <- rkmListA <- rkmListB <- vector( 'list', length(tag.ids) )
for( j in 1:length(tag.ids) ) {
     #j<-1
     t3 <- t2[t2$TagID==tag.ids[j],]
     r <- times <- rkmA <- rkmB <- rep(NA, nrow(t3)-1)
     for( i in 1:(nrow(t3)-1)) {
          rkmA[i] <- paste(t3$RKM[i],"to", t3$RKM[i+1], collapse=" ")
          #rkmB[i] <- paste(t3$RKM[i+1],"to", t3$RKM[i], collapse=" ")
          #i<-1
          r[i] <- paste(t3$Location[i],"to", t3$Location[i+1], collapse=" ")
          t3.arr <- strptime(t3$TimeOfArrival, "%m/%d/%Y %H:%M:%S")
          t3.dep <- strptime(t3$TimeOfDeparture,"%m/%d/%Y %H:%M")
          times[i] <- as.numeric(difftime( as.POSIXct(t3.arr[i+1]), as.POSIXct(t3.dep[i]), units="days"))
          }
     tagList[[j]] <- rep( tag.ids[j], nrow(t3)-1 )
     reachList[[j]] <- r
     amtTimeList[[j]] <- times
     rkmListA[[j]] <- rkmA
     #rkmListB[[j]] <- rkmB
}
tagList
reachList
amtTimeList
tags <- unlist(tagList)
reaches <- unlist(reachList)
amttimes <- unlist(amtTimeList)
rkmAs <- unlist(rkmListA)
#rkmBs <- unlist(rkmListB)
time_table <- as.data.frame(cbind( tags, reaches, amttimes, rkmAs ))
ordered_time_table <- time_table[order(time_table[,2]), ]
ordered_time_table
## match up reach_table with ordered_time_table
head( ordered_time_table )
head( reach_table )

amtTimeAway1 <- amtTimeAway2 <- rep(0, nrow(reach_table))
for( i in 1:nrow(reach_table)) {
      #i<-1
      ind <- which( reach_table$ReachRkms[i] == ordered_time_table$rkmAs ) # amount of time away from lower monitor
      t4 <- ordered_time_table[ind,]
      amtTimeAway1[i] <- sum( as.numeric( na.exclude( t4$amttimes )))
      ind <- which( reach_table$ReachRkmsB[i] == ordered_time_table$rkmAs ) # amount of time away from upper monitor
      t4 <- ordered_time_table[ind,]
      amtTimeAway2[i] <- sum( as.numeric( na.exclude( t4$amttimes )))
}

amtTimeAway1
amtTimeAway2
head(Prob_Det_Gated )
TAway1 <- c(amtTimeAway1, 0)
TAway2 <- c( 0, amtTimeAway2)
TAway <- TAway1 + TAway2
Prob_Det_Gated_b <- cbind( Prob_Det_Gated, TAway )
Prob_Det_Gated_b
time.in.front.of.monitor <- as.numeric(Prob_Det_Gated_b$g.obs.detections) / (24 * 60)
time.in.front.of.monitor  # estimated number of days actually spent at monitor
#
# Difference of g.time.spent (from first time fish detected to last time fish is
# detected during each gate encounter) and time.in.front.of.monitor should be allocated to
# time away from monitor for each monitor
t5 <- cbind( as.numeric(Prob_Det_Gated_b$g.time.spent), time.in.front.of.monitor)
t5
dim(t5)
TAway.additional <- t5[,1] - t5[,2]
TAway.additional
TAway.combined <- TAway + TAway.additional
Prob_Det_Gated_b <- cbind( Prob_Det_Gated, TAway, round(TAway.additional,2), TAway.combined )
Prob_Det_Gated_b
head(Prob_Det_Gated_b)

## add in roving survey data (rsd)- simulate for now (12/7/2011 4:37:12 PM) # For monthly RUI, will have to break this out by month
rsd.detects <- c(0, 2, 9, 5, 8, 3, 3, 9, 2, 1, 0, 1, 1, 4, 4, 1) # close to reality
rsd.totals  <- c(8, rep(10,8), rep(6, 7))                        # close to reality
RovingSurveyData <- cbind(rsd.detects, rsd.totals)

#estReachTimesA <- estReachTimesB <- rep(NA, nrow(reach_table))
#for( i in 2:(nrow(reach_table)-1)) {
#      # for reach 1 - special case
#      estReachTimesA[1] <- TAway.combined[1] * g.Pdets[2] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
#      estReachTimesB[1] <- TAway.combined[2] * (rsd.detects[1] / ( rsd.detects[1] + rsd.detects[2])) * g.Pdets[1]
#      # for reaches 2 - 15
#      #estReachTimesA[2] <- TAway.combined[2] * (rsd.detects[2] / ( rsd.detects[1] + rsd.detects[2])) * g.Pdets[3] ## calc 3 - then odds from there
#      #estReachTimesB[2] <- TAway.combined[3] * (rsd.detects[3] / ( rsd.detects[2] + rsd.detects[3])) * g.Pdets[2] ## calc 4 - then evens from there
#      estReachTimesA[i] <- TAway.combined[i] * (rsd.detects[i] / ( rsd.detects[i-1] + rsd.detects[i])) * g.Pdets[i+1] ## calc 3 - then odds from there
#      estReachTimesB[i] <- TAway.combined[i+1] * (rsd.detects[i+1] / ( rsd.detects[i] + rsd.detects[i+1])) * g.Pdets[i] ## calc 4 - then evens from there
#      # for uppermost reach - special case (reach 16)
#      estReachTimesA[16] <- TAway.combined[16] * (rsd.detects[16] / ( rsd.detects[15] + rsd.detects[16])) * g.Pdets[17] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
#      estReachTimesB[16] <- TAway.combined[17] *  g.Pdets[16]
#}

estReachTimesA <- estReachTimesB <- rep(NA, nrow(reach_table))
n <- nrow(reach_table)
for( i in 2:(nrow(reach_table)-1)) {
      # for reach 1 - special case
      estReachTimesA[1] <- TAway.combined[1] * g.Pdets[2] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
      estReachTimesB[1] <- TAway.combined[2] *  g.Pdets[1]
      # for reaches 2 - 15
      #estReachTimesA[2] <- TAway.combined[2] * (rsd.detects[2] / ( rsd.detects[1] + rsd.detects[2])) * g.Pdets[3] ## calc 3 - then odds from there
      #estReachTimesB[2] <- TAway.combined[3] * (rsd.detects[3] / ( rsd.detects[2] + rsd.detects[3])) * g.Pdets[2] ## calc 4 - then evens from there
      estReachTimesA[i] <- TAway.combined[i] *  g.Pdets[i+1] ## calc 3 - then odds from there
      estReachTimesB[i] <- TAway.combined[i+1] *  g.Pdets[i] ## calc 4 - then evens from there
      # for uppermost reach - special case (reach 16)
      estReachTimesA[16] <- TAway.combined[n-1] *   g.Pdets[n] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
      estReachTimesB[16] <- TAway.combined[n] *  g.Pdets[n-1]
}



ReachTimes <- estReachTimesA + estReachTimesB
numDays <- 273  # number of days of survey (feb1 2011 to oct20 2011)
rui <- ReachTimes / numDays
new_reach_table <- cbind( reach_table, ReachTimes, rui )
new_reach_table
## end of RUI analysis section for whole season
# Plot RUIs
plot( 1:nrow(reach_table), rui, type="h", lwd=3, las=1, ylab="Reach Usage Index", xlab="Reach Number" )
text( 1:nrow(reach_table)-0.35, rep(0, nrow(reach_table)),
      reach_table[,2], srt=90, cex=0.7, pos=4 )



## setup to cycle through all months of interest
rsd_data <- read.csv("test_roving_survey_data.csv", header=T)
rsd_data # simulated data (monthly)

#Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
fMonth <- 2; lMonth<- 10 # first month 2 = Feb, #lMonth = 10 (Oct)
nReaches <- 27
nMonths <- 8 # starting in Feb1
nDaysMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
nDaysMonth <- nDaysMonth[fMonth:lMonth]
rui_table <- as.data.frame(matrix(rep(NA, nReaches * nMonths), ncol=nMonths, byrow=T) ) # builds empty
rui_table
## BEGIN of RUI analysis section for each month
# subset data to pull out only detections for 2011 - mdata already subset to be only
# detections above rkm 411
mons <- fMonth:lMonth
for( k in 1:length(mons)) {
k <- 1
t2 <- subset( mdata, Year==2011 & Month == mons[k] & Detected==1 )
head(t2)
if(nrow(t2) <= 1) {
    rui_table[ ,k] <- rep(NA, nReaches)
    } else
if(nrow(t2) > 1) {
# sum up all the time away for all fish over the season
tag.ids <- as.numeric(unique(t2$TagID))
tag.ids
reachList <- amtTimeList <- tagList <- rkmListA <- rkmListB <- vector( 'list', length(tag.ids) )
for( j in 1:length(tag.ids) ) {
     #j<-1
     t3 <- t2[t2$TagID==tag.ids[j],]
     r <- times <- rkmA <- rkmB <- rep(NA, nrow(t3)-1)
     for( i in 1:(nrow(t3)-1)) {
          rkmA[i] <- paste(t3$RKM[i],"to", t3$RKM[i+1], collapse=" ")
          #rkmB[i] <- paste(t3$RKM[i+1],"to", t3$RKM[i], collapse=" ")
          #i<-1
          r[i] <- paste(t3$Location[i],"to", t3$Location[i+1], collapse=" ")
          t3.arr <- strptime(t3$TimeOfArrival, "%m/%d/%Y %H:%M:%S")
          t3.dep <- strptime(t3$TimeOfDeparture,"%m/%d/%Y %H:%M")
          times[i] <- as.numeric(difftime( as.POSIXct(t3.arr[i+1]), as.POSIXct(t3.dep[i]), units="days"))
          }
     tagList[[j]] <- rep( tag.ids[j], nrow(t3)-1 )
     reachList[[j]] <- r
     amtTimeList[[j]] <- times
     rkmListA[[j]] <- rkmA
     #rkmListB[[j]] <- rkmB
}
tagList
reachList
amtTimeList
tags <- unlist(tagList)
reaches <- unlist(reachList)
amttimes <- unlist(amtTimeList)
rkmAs <- unlist(rkmListA)
#rkmBs <- unlist(rkmListB)
time_table <- as.data.frame(cbind( tags, reaches, amttimes, rkmAs ))
ordered_time_table <- time_table[order(time_table[,2]), ]
ordered_time_table
## match up reach_table with ordered_time_table
head( ordered_time_table )
head( reach_table )

amtTimeAway1 <- amtTimeAway2 <- rep(0, nrow(reach_table))
for( i in 1:nrow(reach_table)) {
      #i<-1
      ind <- which( reach_table$ReachRkms[i] == ordered_time_table$rkmAs ) # amount of time away from lower monitor
      t4 <- ordered_time_table[ind,]
      amtTimeAway1[i] <- sum( as.numeric( na.exclude( t4$amttimes )))
      ind <- which( reach_table$ReachRkmsB[i] == ordered_time_table$rkmAs ) # amount of time away from upper monitor
      t4 <- ordered_time_table[ind,]
      amtTimeAway2[i] <- sum( as.numeric( na.exclude( t4$amttimes )))
}

amtTimeAway1
amtTimeAway2
head(Prob_Det_Gated )
TAway1 <- c(amtTimeAway1, 0)
TAway2 <- c( 0, amtTimeAway2)
TAway <- TAway1 + TAway2
Prob_Det_Gated_b <- cbind( Prob_Det_Gated, TAway )
Prob_Det_Gated_b
time.in.front.of.monitor <- as.numeric(Prob_Det_Gated_b$g.obs.detections) / (24 * 60)
time.in.front.of.monitor  # estimated number of days actually spent at monitor
#
# Difference of g.time.spent (from first time fish detected to last time fish is
# detected during each gate encounter) and time.in.front.of.monitor should be allocated to
# time away from monitor for each monitor
t5 <- cbind( as.numeric(Prob_Det_Gated_b$g.time.spent), time.in.front.of.monitor)
t5
dim(t5)
TAway.additional <- t5[,1] - t5[,2]
TAway.additional
TAway.combined <- TAway + TAway.additional
Prob_Det_Gated_b <- cbind( Prob_Det_Gated, TAway, round(TAway.additional,2), TAway.combined )
Prob_Det_Gated_b
head(Prob_Det_Gated_b)

## add in roving survey data (rsd)- simulate for now (12/7/2011 4:37:12 PM) # For monthly RUI, will have to break this out by month
rsd.detects <- c(0, 2, 9, 5, 8, 3, 3, 9, 2, 1, 0, 1, 1, 4, 4, 1) # close to reality
rsd.totals  <- c(8, rep(10,8), rep(6, 7))                        # close to reality
RovingSurveyData <- cbind(rsd.detects, rsd.totals)
#rsd_tab <-

#estReachTimesA <- estReachTimesB <- rep(NA, nrow(reach_table))
#for( i in 2:(nrow(reach_table)-1)) {
#      # for reach 1 - special case
#      estReachTimesA[1] <- TAway.combined[1] * g.Pdets[2] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
#      estReachTimesB[1] <- TAway.combined[2] * (rsd_data[1,k] / ( rsd_data[1,k] + rsd_data[2,k])) * g.Pdets[1]
#      # for reaches 2 - 15
#      #estReachTimesA[2] <- TAway.combined[2] * (rsd_data[2] / ( rsd_data[1] + rsd_data[2])) * g.Pdets[3] ## calc 3 - then odds from there
#      #estReachTimesB[2] <- TAway.combined[3] * (rsd_data[3] / ( rsd_data[2] + rsd_data[3])) * g.Pdets[2] ## calc 4 - then evens from there
#      estReachTimesA[i] <- TAway.combined[i] * (rsd_data[i,k] / ( rsd_data[i-1,k] + rsd_data[i,k])) * g.Pdets[i+1] ## calc 3 - then odds from there
#      estReachTimesB[i] <- TAway.combined[i+1] * (rsd_data[i+1,k] / ( rsd_data[i,k] + rsd_data[i+1,k])) * g.Pdets[i] ## calc 4 - then evens from there
#      # for uppermost reach - special case (reach 16)
#      estReachTimesA[16] <- TAway.combined[n-1] * (rsd_data[n-1,k] / ( rsd_data[n-2,k] + rsd_data[n-1,k])) * g.Pdets[n] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
#      estReachTimesB[16] <- TAway.combined[n] *  g.Pdets[n-1]
#}
estReachTimesA <- estReachTimesB <- rep(NA, nrow(reach_table))
for( i in 2:(nrow(reach_table)-1)) {
      # for reach 1 - special case
      estReachTimesA[1] <- TAway.combined[1] * g.Pdets[2] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
      estReachTimesB[1] <- TAway.combined[2] *  g.Pdets[1]
      # for reaches 2 - 15
      #estReachTimesA[2] <- TAway.combined[2] * (rsd_data[2] / ( rsd_data[1] + rsd_data[2])) * g.Pdets[3] ## calc 3 - then odds from there
      #estReachTimesB[2] <- TAway.combined[3] * (rsd_data[3] / ( rsd_data[2] + rsd_data[3])) * g.Pdets[2] ## calc 4 - then evens from there
      estReachTimesA[i] <- TAway.combined[i] *  g.Pdets[i+1] ## calc 3 - then odds from there
      estReachTimesB[i] <- TAway.combined[i+1] *  g.Pdets[i] ## calc 4 - then evens from there
      # for uppermost reach - special case (reach 16)
      estReachTimesA[16] <- TAway.combined[n-1] *  g.Pdets[n] # all time towards "reach1" since no detects from rsd scaled by g.Pdets[2]
      estReachTimesB[16] <- TAway.combined[n] *  g.Pdets[n-1]
}



ReachTimes <- estReachTimesA + estReachTimesB
numDays <- nDaysMonth[k]  # number of days of survey (feb1 2011 to oct20 2011)
rui <- ReachTimes / numDays

rui_table[ ,k] <- rui

    }
}
rui_table

monthly_rui_table <- cbind( reach_table[,-c(4)], rui_table )
monthly_rui_table
# end of RUI analysis section for each month

# Plot Monthly RUIs
mids <- (as.numeric(g.rkm[2:n]) - as.numeric(g.rkm[1:(n-1)])) / 2
midpts <- as.numeric(g.rkm[1:(n-1)]) + mids
monthNames <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
par(mfrow=c(5,2))
par(mar= c(1,4,1,1 ))
xlim <- c(250, 550); ylim<-c(0, 20)
for(i in 1:ncol(rui_table)) {
plot( midpts, rep(0, length(midpts)),
            type="h", xlim=xlim, ylim=ylim,
            las=1, cex=2, ann=F)
            ind <- which( rui_table[,i] > 0 )
            points(  midpts[ind], rui_table[ind,i], type="h", lwd=2)

            text(500, 15, paste(monthNames[mons[i]],2011), cex=2)
            if( i == 5) { mtext( side=2, line=2, "Reach Usage Index", cex=1.8) }
            if( i == 8) { mtext( side=1, line=3, "River Kilometer (from GG)", cex=1.8) }
abline( v = 411.8, lty=3, lwd=1.5)
}


head(reach_table)
head(new_reach_table)
head(ordered_time_table)
head(Prob_Det)
head(Prob_Det_Gated)
head(Prob_Det_Gated_b)
head(RovingSurveyData)
head(rsd_data)

#output dataset - loop works - d/n modify
project <- "Detection_Probs_2011_GS_"
mytime <- format(Sys.time(), "%b_%d_%Y_%H_%M_%S")
myfile <- gsub("( )", "", paste(data.dir, project, mytime, ".csv"))
write.table( Prob_Det, file=myfile,  quote=F, sep=",", row.names=F, append=F)

# clean up workspace #
objects()
rm(myfile, mytime, other.ids, project, tag.ids,
    detects, dts, newmdata, mdata, mon_data, myfile, mytime, unique.monitors, Year,
    rkm, lat, lon, num1s, totals, Pdets, ind, loc, Pdet.tab, Prob_Det,
    obs.detections, obs.time.spent, exp.detections, exp.time.spent, prop.time.spent)
objects()
##




##======================================== END
# EXAMPLE CODE FOR OUTPUTTING OBJECTS / FILES / FIGURES
#write.table(mydf, file=paste(i , ".infile", sep=""), quote=F, row.names=F, sep="\t")
#write.table(x, paste(files[i], c(".out"), sep=""), quote=FALSE, sep="\t", col.names = NA)

tag.ids <- unique(TagID)
tag.ids
length(tag.ids) # 234
index <- seq(0, length(tag.ids), 6)
a1 <- seq(1, length(tag.ids), 2)
b1 <- seq(0, length(tag.ids), 2)
c1 <- sort( c(a1, b1))[-c(1)]
c1
length(c1)
tag.ids
for( j in seq(1,length(tag.ids),2))  {
#j<-1
tags <- unique(tag.ids)[c1[j]:c1[j+1]] 
length(tags)
windows()
outputdir <- 'E:/WORK/LONG-TERM MOVEMENT/OUTPUT/'
bmp(filename = paste( unique(tag.ids)[c1[j]],"-",unique(tag.ids)[c1[j+1]],"_RKM_plots.bmp", sep=""), width=1024, height=768,
    units = "px", pointsize = 5, bg = "white", res = 300,
    restoreConsole = TRUE)
par(mfrow=c(1,2))
for( k in 1:length(tags)) {
    RkmPlot(tags[k], Lrkm=-100, Urkm=550, time1 = "2011-03-01 00:00:00", 
    time2 = "2011-12-01 00:00:00", specific.locations=c(225, 285, 411.8))
  }
dev.off()
}


## PlotIt function
a1 <- seq(1, length(tag.ids), 3)
b1 <- seq(0, length(tag.ids), 3)
c1 <- sort( c(a1, b1))[-c(1)]
c1
length(c1)

#j <- 155
for( j in seq(1,5,2))  {
tags <- unique(TagID)[c1[j]:c1[j+1]] 
length(tags)
outputdir <- 'E:/WORK/LONG-TERM MOVEMENT/OUTPUT/'
bmp(filename = paste(unique(TagID)[c1[j]],"-",unique(TagID)[c1[j+1]],"_spatial_plots_v2.bmp", sep=""), width=1024, height=768,
    units = "px", pointsize = 5, bg = "white", res = 300,
    restoreConsole = TRUE)
par(mfrow=c(1,3))
for( k in 1:length(tags)) {
    PlotIt(tags[k])
  }
dev.off()
}  


##======================================== BEGIN
# start section of code to link monitor data (specific location(s)) to water quality / quantity
# data from CDEC stations or UCD maintained sites
x <- "SJ_MedfordChannel"
aa <- test[ test$Location==x , ]
head(aa)
new.date <- paste(aa$DetectDate)
new.date.time <- strptime(new.date, "%m/%d/%Y %H:%M:%S")
head(new.date)
head(new.date.time)

##==============================================================================
## wq data
wq_pri <- read.csv("PRI_wqdata.csv", stringsAsFactors=FALSE, header=T )    # water quality at CDEC station PRI (Prisoners Point on SJ river)
#attach(flows)
head(wq_pri)
dim(wq_pri)
n.date.time <- paste(wq_pri$date,wq_pri$time)
date.time <- strptime(n.date.time, "%m/%d/%Y %H:%M")
head(date.time)



##==============================================================================
# convert dates/times to seconds after fixed date/time
origin <- "2009-01-01 00:00:00"
dettime.secs  <- as.numeric( difftime(new.date.time, origin, units="secs"))     ## detections
flowtime.secs   <- as.numeric( difftime(date.time, origin, units="secs"))       ## flows / wq parameters


syncd.flows <- as.data.frame(matrix(rep(NA, nrow(aa)*(ncol(wq_pri)-2)), byrow=T,ncol=(ncol(wq_pri)-2)))
head(syncd.flows)
for( i in 1:length(new.date.time))  {
#i <- 1  # index of detections
  #for( j in 1:(length(Deg.C)-1)) {
      ## narrow "search" by matching Reach (flows) and River.Reach (detections)
      ## and by only searching a given interval around specific detection date
      detection.range <- c( dettime.secs[i] - 1800, dettime.secs[i] + 1800)
      detection.range
      eff1 <- which( flowtime.secs >= detection.range[1] & flowtime.secs <= detection.range[2] )
      eff1  # index of flows

      min.abs <- which.min( abs( flowtime.secs[eff1] - dettime.secs[i] ))

      syncd.flows[i,]  <- wq_pri[ eff1[min.abs] , c(3:ncol(wq_pri)) ]

      }


##============= CONVERT CONDUCTIVITY TO SALINITY ===============================
## Author Alan D. Jassby and James E. Cloern
#Maintainer Alan Jassby <adjassby@ucdavis.edu>
#Description Functions to assist in the processing and exploration of
#data from monitoring programs for aquatic ecosystems. The focus
#is on time series data for physical and chemical properties of
#water, as well as the plankton. The package is intended for
#programs that sample approximately monthly at discrete stations.

#ec2pss converts electrical conductivity data to salinity using the Practical Salinity Scale 1978 in
#the range of 2-42 (Fofonoff and Millard 1983). Salinities below 2 are calculated using the extension
#of the Practical Salinity Scale (Hill et al. 1986).
#R2pss is the same function, except that conductivity ratios rather than conductivities are used as
#input.
##============================
## load package "wq"
library(wq)
temp_c <- (as.numeric(syncd.flows$temp_f) - 32) *(5/9)
salinity_ppt <- ec2pss( as.numeric(syncd.flows$cond_uscm)/1000, temp_c, rep(0,length(temp_c)))
discharge_m3s <-  (as.numeric(syncd.flows[,2]) / 35.31467)
swq <- data.frame( syncd.flows, discharge_m3s, temp_c, salinity_ppt)
      colnames(swq) <- c(names(wq_pri)[3:ncol(wq_pri)] , "discharge_m3s", "temp_c", "sal_ppt" )

new_aa <- data.frame( aa, swq )
head( new_aa )

pri_temp_c <- (as.numeric(wq_pri$temp_f) - 32)*(5/9)
pri_sal <- ec2pss( as.numeric(wq_pri$cond_uscm)/1000, pri_temp_c, rep(0,length(pri_temp_c)))

#class( new_aa$DetectDate )
new_dt <- strptime(as.character(new_aa$DetectDate), "%m/%d/%Y %H:%M:%S")
head(new_dt)
# plot linked wq parameters
par(mfrow=c(2,2))
plot( new_dt, new_aa$temp_c, pch=19, cex=0.8)
plot( new_dt, new_aa$discharge_m3s, pch=19, cex=0.8)
plot( new_dt, new_aa$sal_ppt, pch=19, cex=0.8)
plot( new_dt, new_aa$turb_ntu, pch=19, cex=0.8)

# plot wq_pri data with detections overlayed
par(mfrow=c(2,2))
head(wq_pri)
plot( date.time, pri_temp_c , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$temp_c, pch=19, cex=0.8)
plot( date.time, (as.numeric(wq_pri$Qcfs) / 35.31467) , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$discharge_m3s, pch=19, cex=0.8)
plot( date.time, pri_sal , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$sal_ppt, pch=19, cex=0.8)
plot( date.time, wq_pri$turb_ntu , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_aa$turb_ntu, pch=19, cex=0.8)



##====================================================
## wq data
wq_mrz <- read.csv("MRZ_wqdata.csv", stringsAsFactors=FALSE, header=T )    # water quality at CDEC station MRZ (Martinez Marina)
#attach(flows)
head(wq_mrz)
dim(wq_mrz)
n.date.time <- paste(wq_mrz$date,wq_mrz$time)
date.time <- strptime(n.date.time, "%m/%d/%Y %H:%M")
head(date.time)

head(test)
x <- 51.69
bb <- test[ test$RiverKm==x , ]
head(bb)
new.date <- paste(bb$DetectDate)
new.date.time <- strptime(new.date, "%m/%d/%Y %H:%M:%S")
head(new.date)
head(new.date.time)

##==============================================================================
# convert dates/times to seconds after fixed date/time
origin <- "2009-01-01 00:00:00"
dettime.secs  <- as.numeric( difftime(new.date.time, origin, units="secs"))     ## detections
flowtime.secs   <- as.numeric( difftime(date.time, origin, units="secs"))       ## flows / wq parameters


syncd.flows <- as.data.frame(matrix(rep(NA, nrow(bb)*(ncol(wq_mrz)-2)), byrow=T,ncol=(ncol(wq_mrz)-2)))
head(syncd.flows)
for( i in 1:length(new.date.time))  {
#i <- 1  # index of detections
  #for( j in 1:(length(Deg.C)-1)) {
      ## narrow "search" by matching Reach (flows) and River.Reach (detections)
      ## and by only searching a given interval around specific detection date
      detection.range <- c( dettime.secs[i] - 1800, dettime.secs[i] + 1800)
      detection.range
      eff1 <- which( flowtime.secs >= detection.range[1] & flowtime.secs <= detection.range[2] )
      eff1  # index of flows

      min.abs <- which.min( abs( flowtime.secs[eff1] - dettime.secs[i] ))

      syncd.flows[i,]  <- wq_mrz[ eff1[min.abs] , c(3:ncol(wq_mrz)) ]
colnames(syncd.flows) <- c(names(wq_mrz)[3:ncol(wq_mrz)])
      }

head(syncd.flows)

##============= CONVERT CONDUCTIVITY TO SALINITY ===============================
## Author Alan D. Jassby and James E. Cloern
#Maintainer Alan Jassby <adjassby@ucdavis.edu>
#Description Functions to assist in the processing and exploration of
#data from monitoring programs for aquatic ecosystems. The focus
#is on time series data for physical and chemical properties of
#water, as well as the plankton. The package is intended for
#programs that sample approximately monthly at discrete stations.

#ec2pss converts electrical conductivity data to salinity using the Practical Salinity Scale 1978 in
#the range of 2-42 (Fofonoff and Millard 1983). Salinities below 2 are calculated using the extension
#of the Practical Salinity Scale (Hill et al. 1986).
#R2pss is the same function, except that conductivity ratios rather than conductivities are used as
#input.
##============================
## load package "wq"
library(wq)
temp_c <- (as.numeric(syncd.flows$temp_f) - 32) *(5/9)
salinity_ppt <- ec2pss( as.numeric(syncd.flows$cond_uscm)/1000, temp_c, rep(0,length(temp_c)))
#discharge_m3s <-  (as.numeric(syncd.flows[,2]) / 35.31467)
swq <- data.frame( syncd.flows, temp_c, salinity_ppt)
      colnames(swq) <- c(names(wq_mrz)[3:ncol(wq_mrz)] , "temp_c", "sal_ppt" )

new_bb <- data.frame( bb, swq )
head( new_bb )

mrz_temp_c <- (as.numeric(wq_mrz$temp_f) - 32)*(5/9)
mrz_sal <- ec2pss( as.numeric(wq_mrz$cond_uscm)/1000, mrz_temp_c, rep(0,length(mrz_temp_c)))

#class( new_bb$DetectDate )
new_dt <- strptime(as.character(new_bb$DetectDate), "%m/%d/%Y %H:%M:%S")
head(new_dt)
# plot linked wq parameters
par(mfrow=c(2,2))
plot( new_dt, new_bb$temp_c, pch=19, cex=0.8)
plot( new_dt, new_bb$do_mgl, pch=19, cex=0.8)
plot( new_dt, new_bb$sal_ppt, pch=19, cex=0.8)
plot( new_dt, new_bb$turb_ntu, pch=19, cex=0.8)

# plot wq_mrz data with detections overlayed
par(mfrow=c(2,3))
head(wq_mrz)
plot( date.time, mrz_temp_c , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_bb$temp_c, pch=19, cex=0.8)
plot( date.time, wq_mrz$do_mgl , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_bb$do_mgl, pch=19, cex=0.8)
plot( date.time, mrz_sal , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_bb$sal_ppt, pch=19, cex=0.8)
plot( date.time, wq_mrz$turb_ntu , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_bb$turb_ntu, pch=19, cex=0.8)
plot( date.time, wq_mrz$pH , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_bb$pH, pch=19, cex=0.8)
plot( date.time, wq_mrz$chlorophyll_ugl , pch=19, col="grey50", cex=0.8 )
points( new_dt, new_bb$chlorophyll_ugl, pch=19, cex=0.8)


##======================================== END
##
##
##
##
## GETTING IFFY DOWN HERE ##
## GETTING IFFY DOWN HERE ##
## GETTING IFFY DOWN HERE ##
## GETTING IFFY DOWN HERE ##

## possible ideas # return Y or N if each fish met some criteria
highest.rm <- det.abv.IF <- rep(NA, length(tag.ids))
for( i in 1:length(tag.ids)) {
highest.rm[i] <- max( PrevKm[TagNumber==tag.ids[i]] )
det.abv.IF[i] <- ifelse( max( PrevKm[TagNumber==tag.ids[i]] )>= 411.8, "Y","N" ) # PrevKm from output of GetMovement function (may need to reorder)
}
highest.rm
det.abv.IF
summary.move.table <- cbind( tag.ids, highest.rm, det.abv.IF )
summary.move.table
days.abv.IF <- rep(NA, length(tag.ids))
#for( i in 1:length(tag.ids)) {}

## possible ideas #

## LOOK IN "RESULTS" DATAFRAME FOR SINGLE DETECTIONS
## so, for each fish (each row of "RESULTS")
num.single.dets <- rep(NA, nrow(results))
for( i in 1:nrow(results)) { num.single.dets[i] <- length(which( results[i,] == 1)) }
num.single.dets
cbind( x, num.single.dets ) ## quick table showing number of single detections @ receivers / fish
## end
## add to this by outputting which receivers that single detections occurred at


## possible ideas #
## DRAFT DISTRIBUTION PLOT W/ CALENDAR OVERLAP CAPABILITY - IN PROGRESS - BEGIN
x <-  c( 46611,46613,46616,46617)#[1]
i<-1
myList <- myList2<- tagList <- monList <- vector( 'list', length(x))
myList
YEAR <- 2011
for( i in 1:length(x)) {
aa <- strptime( test[ TagID==x[i], ][,6] ,"%m/%d/%Y %H:%M:%S")
locs <- test[ TagID==x[i], ][,8]
yind <- which( aa$year +1900 == YEAR )
nextyind <- which( aa$year +1900 == YEAR+1 )
nextyind
nyaa <- aa[nextyind]
pyaa <- aa[yind]
pyweek <- as.numeric(format(pyaa, "%j"))  #     "%j" = julian day ,  "%W" = week of year
nyweek <- as.numeric(format(nyaa, "%j")) + 365  #     "%j" = julian day ,  "%W" = week of year
nyweek
cweek <- as.numeric(c(pyweek, nyweek))
rkms <- test[ TagID==x[i], ][,9]
nyrkms <- rkms[nextyind]
pyrkms <- rkms[yind]
crkms <- c(pyrkms, nyrkms)
group <- c(0, cumsum(diff(cweek) != 0))
#group <- c(0, cumsum(diff(aa) != 0))
bb <- tapply( cweek, group,)
cc <- unique(bb)
weekly.mean.rkm <- sds <- nums <- num.mons<-  rep(NA, length(cc))
for( j in 1:length(cc) ) {
    weekly.mean.rkm[j]<- mean( crkms[which( bb==cc[j] )] )
    sds[j] <- sd( crkms[which( bb==cc[j] )] )
    nums[j] <- length( crkms[which( bb==cc[j] )] )
    #num.mons[j] <- unique( locs[which( bb==cc[j] )] )
     }
nums
myList[[i]] <- weekly.mean.rkm
myList2[[i]] <- unique( cweek )
tagList[[i]] <- rep( unique(x[i]), length(weekly.mean.rkm))
#monList[[i]] <- num.mons
  }
myList
myList2 
tagList
str(tagList)  # dimensions of list
mean.rkms <- unlist( myList)
weeks <- unlist( myList2)
taglist   <- unlist( tagList)
tab <- as.data.frame(cbind(taglist, weeks, mean.rkms ))
tab$weeks
agg1 <- aggregate(tab$mean.rkms, by=list(tab$weeks), FUN=mean )
agg2 <- aggregate(tab$mean.rkms, by=list(tab$weeks), FUN=sd )[2] 
agg2a <- aggregate(tab$mean.rkms, by=list(tab$weeks), FUN=min )
agg2b <- aggregate(tab$mean.rkms, by=list(tab$weeks), FUN=max )
agg3 <- aggregate(tab$taglist, by=list(tab$weeks), FUN=unique)[2]
agg4 <- aggregate(tab$taglist, by=list(tab$weeks), FUN=length)[2]  
tab2 <- cbind( agg1, agg2,agg2a, agg3, agg4 )
nf <- layout(matrix(c(1,1,1,1,1,1, 2,2,2), 3, 3, byrow = TRUE), 
   widths=c(3,1), heights=c(1,2))
#layout.show(nf)
xlim <- c(0,max(agg1[,1])); ylim<- c(-100,550)
x.ats <- xlim[1]:xlim[2]
x.ats
x.labs <- c(x.ats[1:max(x.ats)])#, x.ats[1: (length( x.ats)  - length(1:365))] )
x.labs <- x.ats
cbind(x.ats, x.labs)
plot( agg1[,1], agg1[,2], xlim=xlim, ylim=ylim, type="n", las=1, axes=F, ann=F )
segments( agg1[,1], agg2a[,2],   agg1[,1],      agg2b[,2], lwd=1, col="grey20" )
points(agg1[,1], agg1[,2], cex=1.2, pch=15,col="red" )
points(agg1[,1], agg2a[,2], cex=1.2, pch=15 )
points(agg1[,1], agg2b[,2], cex=1.2, pch=15 )
axis(1, at=x.ats, labels=x.labs)
ylim<- c(0,max(agg4[,1]))
plot( agg1[,1], agg4[,1], xlim=xlim, ylim=ylim, type="h", lwd=2, las=1, ann=F, axes=F)
axis(3, at=x.ats, labels=F)
## DRAFT DISTRIBUTION PLOT - IN PROGRESS - END

## possible ideas #
## start table - simple table summarizing number of detections for each monitor
x<- unique(TagID)
x
i <- 1
y <- unique(date.time$yday[TagID==x[i]]) ## this gives you the julian day of detection (could be useful)
y


## possible ideas #