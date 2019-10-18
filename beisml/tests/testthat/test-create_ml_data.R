context("test-create_ml_data")

test_that("expected behaviour for create_ml_data", {
  df <- mtcars
  proportion <- c(0.8, 0, 0.2)
  validation <- TRUE

  #check adds to 1
  expect_equal(sum(proportion), 1)
  #check class
  expect_is(validation, "logical")
})
