library(tidyverse)
library(assertr)
library(ybp)

context("columns")

# original data example
d <- all69khz_grouped
d <- dplyr::filter(d, TagID == 2841)

test_that("original columns carried over test", {
  expect_equal(ncol(fishpaths(d, d$TagID, d$Station)), 11)
})

