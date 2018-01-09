#' @importFrom magrittr "%>%"
NULL

#' @param fun_name Name identifying the function to be cached, in the form of a character string. This will be used as one of the tags in the function's cached files.
#' @param cache Whether or not to use cache on the current call to the function (includes both loading and saving).
#' @param cache_root Path to the top-level directory used to store cache results; typically caches from other packages/functions will also be stored in this directory.
#' @param cache_dir Path to the low-level directory used to store cache results for this particular function, relative to \code{cache_root}.
#' @param ignore_args Character vector; arguments in the function call that should be ignored when defining cache equality.
#' @export
cache <- function(
  fun_name,
  cache = TRUE,
  cache_root = "cache",
  cache_dir = fun_name,
  ignore_args = c("cache", "cache_root", "cache_dir"),
  expr
) {
  parent_env <- parent.frame()
  cache_info <- if (cache) get_cache_info(
    fun_name = fun_name,
    parent_env = parent_env,
    cache_root = cache_root,
    cache_dir = cache_dir,
    ignore_args = ignore_args
  )
  if (cache && cache_info$result_found) {
    res <- cache_info$result
  } else {
    # We evaluate the expression in the parent environment,
    # i.e. the body of the function that called cache().
    # This prevents namespace clashes with temporary
    # variables created within the cache() function.
    res <- eval(expr, envir = parent_env)
    if (cache) save_cache(result = res, cache_info = cache_info)
  }
  res
}

get_cache_info <- function(fun_name, parent_env, cache_root, cache_dir, ignore_args) {
  args <- as.list(parent_env) %>%
    (function(x) x[setdiff(names(x), ignore_args)])
  cache_file_path <- file.path(
    cache_root,
    cache_dir,
    list(fun_name = fun_name,
         args = args) %>%
      (digest::digest) %>%
      paste(".RDS", sep = "")
  )
  result_found <- FALSE
  result <- NULL
  if (file.exists(cache_file_path)) {
    dat_from_file <- readRDS(cache_file_path)
    # Double-check that the arguments match
    # (catches rare case of hash collision)
    if (identical(dat_from_file$args, args) &&
        identical(dat_from_file$fun_name, fun_name)) {
      result_found <- TRUE
      result <- dat_from_file$result
    }
  }
  list(
    fun_name = fun_name,
    args = args,
    cache_file_path = cache_file_path,
    result_found = result_found,
    result = result
  )
}

save_cache <- function(result, cache_info) {
  R.utils::mkdirs(dirname(cache_info$cache_file_path))
  saveRDS(list(fun_name = cache_info$fun_name,
               args = cache_info$args,
               result = result),
          cache_info$cache_file_path)
}

#' @export
clear_cache <- function(cache_root = "cache", cache_dir = NULL) {
  path <- if(is.null(cache_dir)) {
    cache_root
  } else {
    file.path(cache_root, cache_dir)
  }
  unlink(path, recursive = TRUE)
}

