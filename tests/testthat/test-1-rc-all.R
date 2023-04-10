context("search by all rc_all fetch all countries information without any filter")

test_that("standard rc_all()",{
  fetch_all <- rc_all()

  expect_equal(nrow(fetch_all), 250)
} )
