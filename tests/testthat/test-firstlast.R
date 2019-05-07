context("firstlast")

test_that("getFirstLast returns a single value per TagID for all tagIDs", {
  data("alljuvtt")
  myfl <- getFirstLast(alljuvtt)
  expect_true(all(table(myfl$TagID) == 1))
  expect_true(all(unique(alljuvtt$TagID) %in% myfl$TagID))
})

allfish = readRDS("data/alltags.rds")
test1 = subset(allfish, TagID == 2590)
str(test1)

fishtrackr:::firstlastOneFish(test1, "TagID", "DateTimeUTC", "Station")

dd = getFirstLast(allfish)
str(dd)
length(unique(allfish$TagID))
