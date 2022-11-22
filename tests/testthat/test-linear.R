usethis::use_testthat()
testthat::test_that("linear works", {
  testthat::expect_equal(linear(c(1,2,3), c(1,2,3))$beta[2], 1)
  testthat::expect_equal(linear(c(1,2,3,4,5), c(-1,-2,-3,-4,-5))$beta[2], -1)
  testthat::expect_error(linear(c(1,2,3,4,5), c(1,2,3,4,"5")))
  testthat::expect_error(linear(c(1,2,3,4,5), c("y","n","y","n","y"), cat = FALSE))
})
