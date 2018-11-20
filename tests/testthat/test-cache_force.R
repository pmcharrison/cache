context("test-cache_force")

setup(clear_cache())

test_that("examples", {
  x <- cache_memory()
  f <- function(x, ...) cache(c(x, rnorm(1)), ...)

  expect_error(f(1, .cache_force = TRUE),
               "Cached result not found.")

  expect_error(f(1, .cache_memory = x, .cache_force = TRUE),
               "Cached result not found.")
})

teardown(clear_cache())
