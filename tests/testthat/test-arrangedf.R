# test that fishpaths orders a dataframe by code and detection time

# test: have a df that's arranged by something else, plus one that's arrange correctly
# call fishpaths() on both, test that the output is identical

d = readRDS("data/onetag.rds")
d2 = fishpaths(d, d$TagID, d$Station, Datetime_col = "DateTimePST", Threshold = 60*10)
d3 = readRDS("data/alltags.rds")
d3 = fishpaths(d3, d3$TagID, d3$Station, "DateTimePST", Threshold = 60*60*4) # four hours
