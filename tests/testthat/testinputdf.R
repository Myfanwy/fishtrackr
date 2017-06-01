library(tidyverse)
library(assertr)
library(fishtrackr)
library(testthat)

context("inputdf")

# original data example
d <- alljuvtt
d <- dplyr::filter(d, TagID == 2051)

test_that("check df structure", {
  expect_is(d$TagID, "numeric")
  expect_is(d$DateTimeUTC, "POSIXct")
  expect_is(d$Station, "character")
  expect_is(d$Rkm, "numeric")
})


test_that("no detections take place in the future", {
  time = Sys.time()
  expect_true(max(d$DateTimeUTC) < time)
})

