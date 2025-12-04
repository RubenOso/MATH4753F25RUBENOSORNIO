test_that("mu echoed",    { res <- myncurve(10, 5, 6); expect_equal(res$mu,    10) })
test_that("sigma echoed", { res <- myncurve(10, 5, 6); expect_equal(res$sigma,  5) })
test_that("prob matches pnorm", {
  res <- myncurve(10, 5, 6)
  expect_equal(res$prob, pnorm(6, 10, 5), tolerance = 1e-12)
})
