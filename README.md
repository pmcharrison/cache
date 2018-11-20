
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cache - Caching function outputs

`cache` is an R package for function output caching, or memoisation.

The
[memoise](https://cran.r-project.org/web/packages/memoise/index.html)
package is an effective tool for caching repeated computations in data
analysis projects. However, the memoise package cannot (easily) memoise
internal functions within R packages, especially when the demand is for
file-based memoisation, or when memoisation options need to be exposed
to the end user.

This package addresses these problems. It is designed for memoising
functions when creating R packages, while allowing subsequent package
users to control cache options (e.g. disabling cache, clearing the
cache, changing the cache directory).

## Installation

You can install the package directly from Github.

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/cache")
```

## Example usage

Define a simple cached function:

``` r
f <- function(x, ...) cache({
  message("Computing...")
  c(x, rnorm(1))
}, ...)
```

The first time it’s called, the main body of the function is run.

``` r
f(1) 
#> Computing...
#> [1] 1.0000000 0.7575416
```

The second time it’s called, the result is loaded from the file-based
cache.

``` r
f(1)
#> [1] 1.0000000 0.7575416
```

Our function `f` can be called with various caching options - see
`?cache::cache` for details. These options include:

  - `.cache` - Whether or not to use cache on the current call.
  - `.cache_memory` - Enables in-memory caching.
  - `.cache_dir` - Directory in which to save cached files.
  - `.cache_force` - If TRUE, an error will be thrown if the current
    call cannot be found in the cache. Useful for debugging.

If we call `f` with cache disabled, we should get a different result.

``` r
f(1, .cache = FALSE) # produces a different result
#> Computing...
#> [1]  1.0000000 -0.3510109
```

We can also define an in-memory cache. In-memory caches are faster than
the default file-based caches.

``` r
x <- cache_memory()
f(1, .cache_memory = x)
#> Computing...
#> [1] 1.0000000 0.5139942

f(1, .cache_memory = x)
#> [1] 1.0000000 0.5139942

as.list(x)[[1]]
#> $fun_name
#> [1] "f"
#> 
#> $args
#> $args$x
#> [1] 1
#> 
#> 
#> $result
#> [1] 1.0000000 0.5139942
```

When we’re done, we can clear the cache.

``` r
clear_cache()
```
