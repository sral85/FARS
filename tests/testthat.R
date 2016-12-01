library(testthat)
library(FARS)



test_that("make_filename for different years", expect_equal(make_filename(2015), "accident_2015.csv.bz2"))
