context("test-cache_ignore")

setup(clear_cache())

test_that("cache_ignore", {
  f <- function(x, .y, ...) cache({
    c(x, rnorm(1))
  }, ...)
  expect_equal(f(1, 1), f(1, 2))

  f <- function(x, y, ...) cache({
    c(x, rnorm(1))
  }, ...)
  expect_false(identical(f(1, 1), f(1, 2)))

  f <- function(x, .y, ...) cache({
    c(x, rnorm(1))
  },
  .cache_ignore = "y",
  ...)
  expect_equal(f(1, 1), f(1, 2))
})

teardown(clear_cache())
