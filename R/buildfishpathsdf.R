
# source after sourcing "fishpaths.R"

library(ybp)
library(beepr)
f <- all69khz

# --- Run this chunk first if you want to group stations for plots --- #
library(stringr)
# Abv_rstr & rstr (121.04 & 121.01) -> rstr_joint (121.0)
# BCE/W <- BC_joint
# BCE/W2 <- BC2_joint
f$Station <- str_replace(f$Station, 'RSTR', 'Rstr_joint')
f$Station <- str_replace(f$Station, 'Abv_rstr', 'Rstr_joint')
f$Station <- str_replace(f$Station, 'BCE' , 'BC_joint')
f$Station <- str_replace(f$Station, 'BCE2', 'BC2_joint')
f$Station <- str_replace(f$Station, 'BCW' , 'BC_joint')
f$Station <- str_replace(f$Station, 'BCW2', 'BC2_joint')
f$Rkm[f$Rkm == 121.04] <- 121
f$Rkm[f$Rkm == 121.01] <- 121
unique(f$Station)
sort(unique(f$Rkm))
str(f)

##  Building fishpaths dataframe ##
f1 = split(f, list(f$TagID, f$Station)) # splits the dataframe into a list, where each element of the list is every combination of TagID and station, and the detections within that.
# glimpse <- f1[sapply(f1, nrow) == 34] # see how it looks: these are all the fish/station combos with 34 rows

f1 = f1[ sapply(f1, nrow) > 0 ] # filter out the combos that don't actually occur

tmp = lapply(f1, splitFishStationVisits) # apply this function, which adds the grouping variable, then calls on the other function, which acts on the grouping variable

fishpaths = do.call(rbind, tmp) # get the whole thing into a df
head(fishpaths)
beep()

#tmp2 = lapply(f1, splitFishStationVisits, rowFunc = myRowFun, TimeThreshold = 60*20) # change the threshold if you like
# tmp3 = do.call(rbind, tmp2)

