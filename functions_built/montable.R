#### Monitor Table Function ##

# Dependencies: Detection object "test" (or any other name).  Requires that detection object has column headings "DateTimeUTC", "Station", "Receiver", and "TagID".  For the example used here, see file with columns labeled as in "2012_LFCJ_dets.csv"

library(readr)
test <- read_csv("C:/Users/Myfanwy/Dropbox/GitHubRepos/JuvSalmon/2012_FishTracker_InputFiles/data_clean/2012_LFCJ_dets.csv", col_types = list(DateTimeUTC = col_datetime()))
test$`[EMPTY]` <- NULL
str(test) # make sure dates are in POSIXct
test


montable <- function(x) {
  options(digits = 2)
  
  montable <- 
    x %>% # where x is is the name of the detection object
    select(DateTimeUTC, Station, Receiver, TagID) %>% # discards any additional columns that might cause trouble
    arrange(DateTimeUTC) %>%
    group_by(Station) %>% #groups the dataset by Station before it goes further
    summarise(n = length(unique(TagID)), min.date = min(DateTimeUTC), max.date = max(DateTimeUTC)) %>% # creates column n for the number of fish detected at that monitor, identifies the minimum and maximum detection date at each monitor and creates a summary column
    arrange(min.date) #orders the table by the minimum detection date (so that the table will proceed in rough geographical order by fish path)
  # Note: when putting this in to a function, include checks to make sure that the user doesn't have a global variable defined as "TagID", etc.
 
    montable <- montable %>%
      mutate(range = difftime(a$max.date, a$min.date, units="days"))
  
  montable
}

montable(test)