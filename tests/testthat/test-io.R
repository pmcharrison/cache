context("test-io")

test_that("examples", {
  x <- cache_memory()
  f <- function(x, ...) cache(c(x, rnorm(1)), ...)

  f1 <- f(1, .cache_memory = x)
  f2 <- f(2, .cache_memory = x)
  f3 <- f(3, .cache_memory = x)

  save_cache(x, "temp.rds")
  y <- load_cache("temp.rds")

  expect_equal(f1, f(1, .cache_memory = y))
  expect_equal(f2, f(2, .cache_memory = y))
  expect_equal(f3, f(3, .cache_memory = y))
  expect_error(f(4, .cache_memory = y, .cache_force = TRUE),
               "Cached result not found.")

  file.remove("temp.rds")
})
