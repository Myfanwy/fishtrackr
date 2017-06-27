context("firstlast")

test_that("getFirstLast returns a single value per TagID for all tagIDs", {
  data("alljuvtt")
  myfl <- getFirstLast(alljuvtt)
  expect_true(all(table(myfl$TagID) == 1))
  expect_true(all(unique(alljuvtt$TagID) %in% myfl$TagID))
})
