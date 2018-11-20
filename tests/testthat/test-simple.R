context("test-simple")

setup(clear_cache())

test_that("simple examples", {
  f <- function(x, ...) cache({
    c(x, rnorm(1))
  }, ...)
  expect_equal(f(1), f(1))
  expect_false(identical(f(1, .cache = FALSE), f(1, .cache = FALSE)))
})

teardown(clear_cache())
