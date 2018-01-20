context("cache")

cache <- new.env()

# Make a cached function
add_one <- function(x, cache_env, stop_on_missing) {
  cache("addition",
        cache_env = cache_env,
        stop_on_missing = stop_on_missing,
        expr = expression({
          x + 1
        }))
}

test_that("correct_initial_output", {
  expect_equal(add_one(1, cache, stop_on_missing = FALSE), 2)
})

test_that("correct_cache_size", {
  expect_equal(length(as.list(cache)), 1)
})

test_that("correct_second_output", {
  expect_equal(add_one(1, cache, stop_on_missing = FALSE), 2)
})

test_that("stop_on_missing", {
  expect_error(add_one(2, cache, stop_on_missing = TRUE))
})
