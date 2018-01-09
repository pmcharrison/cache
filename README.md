## Functions

`cacheR::get_cache_info` - Extracts the name of the function as well as the argument list, and uses this to define a reproducible name for a cache file. T

`cacheR::save_cache` - 

## Parameters

`cache` - If `TRUE` then results will be cached to avoid repeated computation.

`cache_root` - Path to the top-level directory used to store cache results; typically caches from other packages/functions will also be stored in this directory.

`cache_dir` - Path to the low-level directory used to store cache results for this particular function, relative to `cache_root`.

`ignore_args` - Arguments in the function call that will be ignored when defining cache equality.

````{r}
fun <- function(
  n,
  msg_start = TRUE,
  msg_end = TRUE,
  cache = TRUE,
  cache_root = "./cache",
  cache_dir = "fun"
) {
  cacheR::cache(
    cache_root = cache_root,
    cache_dir = cache_dir,
    ignore_args = c("msg_start", "msg_end", "cache", "cache_root", "cache_dir"),
    expr = expression({
      if (msg_start) message("Generating some random numbers")
      res <- rnorm(n)
      if (msg_end) message("Finished generating some random numbers")
    })
  )
}
````
