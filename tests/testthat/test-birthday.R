test_that("the birthday function works", {
  expect_equal(Birthday(23), 0.507297, tolerance = 1e-6)
})
