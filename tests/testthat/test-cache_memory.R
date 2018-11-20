context("test-cache_memory")

setup(clear_cache())

test_that("examples", {
  clear_cache()
  x <- cache_memory()
  f <- function(x, ...) cache(c(x, rnorm(1)), ...)
  f1a <- f(1, .cache_memory = x)
  f1b <- f(1)
  expect_false(identical(f1a, f1b))
  expect_identical(f1a, f(1, .cache_memory = x))
})

teardown(clear_cache())
