#' @importFrom magrittr "%>%"
NULL

#' @param fun_name Name identifying the function to be cached, in the form of a character string. This will be used as one of the tags in the function's cached files.
#' @param cache Whether or not to use cache on the current call to the function (includes both loading and saving).
#' @param cache_root Path to the top-level directory used to store cache results; typically caches from other packages/functions will also be stored in this directory.
#' @param cache_dir Path to the low-level directory used to store cache results for this particular function, relative to \code{cache_root}.
#' @param cache_env Optional environment containing cache to use instead of file-based system
#' @param ignore_args Character vector; arguments in the function call that should be ignored when defining cache equality.
#' @param stop_on_missing Logical; if TRUE, then the function will throw an error if it can't find any cache for its results. Useful if you want to ensure that the function is using cache instead of recomputing.
#' @export
cache <- function(
  fun_name,
  cache = TRUE,
  cache_root = "cache",
  cache_dir = fun_name,
  cache_env = NULL,
  ignore_args = c("cache", "cache_root", "cache_dir", "cache_env"),
  stop_on_missing = FALSE,
  expr
) {
  parent_env <- parent.frame()
  cache_info <- if (cache) get_cache_info(
    fun_name = fun_name,
    parent_env = parent_env,
    cache_root = cache_root,
    cache_dir = cache_dir,
    ignore_args = ignore_args,
    cache_env = cache_env
  )
  if (!is.null(cache_info) && cache_info$result_found) {
    res <- cache_info$result
  } else {
    # We evaluate the expression in the parent environment,
    # i.e. the body of the function that called cache().
    # This prevents namespace clashes with temporary
    # variables created within the cache() function.
    if (stop_on_missing) stop("Cached result not found.")
    res <- eval(expr, envir = parent_env)
    if (cache) save_cache(result = res, cache_info = cache_info,
                          cache_env = cache_env)
  }
  res
}

get_cache_info <- function(fun_name, parent_env, cache_root,
                           cache_dir, ignore_args, cache_env = NULL) {
  args <- as.list(parent_env) %>%
    (function(x) x[setdiff(names(x), ignore_args)])
  hash <- list(fun_name = fun_name,
               args = args) %>% (digest::digest)

  cache_file_path <- NULL

  dat_loaded <- if (!is.null(cache_env)) {
    cache_env[[hash]]
  } else {
    cache_file_path <- file.path(
      cache_root,
      cache_dir,
      hash %>% paste(".RDS", sep = "")
    )
    if (file.exists(cache_file_path)) {
      dat_loaded <- readRDS(cache_file_path)
    } else NULL
  }
  # Double-check that the arguments match
  # (catches rare case of hash collision)
  result_found <-
    !is.null(dat_loaded) &&
    all.equal(dat_loaded$args, args) &&
    all.equal(dat_loaded$fun_name, fun_name)

  list(
    fun_name = fun_name,
    args = args,
    hash = hash,
    cache_file_path = cache_file_path,
    result_found = result_found,
    result = if (result_found) dat_loaded$result
  )
}

save_cache <- function(result, cache_info, cache_env) {
  out <- list(fun_name = cache_info$fun_name,
              args = cache_info$args,
              result = result)
  if (is.null(cache_env)) {
    R.utils::mkdirs(dirname(cache_info$cache_file_path))
    saveRDS(out, cache_info$cache_file_path)
  } else {
    cache_env[[cache_info$hash]] <- out
  }
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

#' @param path Folder containing RDS files to be loaded
#' @param fun Optional function to be applied to the cached result, should return an updated version of the result
#' @export
load_cache <- function(path, fun = NULL) {
  files <- list.files(path, pattern = "\\.RDS$")
  hash <- gsub("\\.RDS", "", files)
  env <- new.env()
  if (interactive()) pb <- txtProgressBar(max = length(files), style = 3)
  for (i in seq_along(files)) {
    dat <- readRDS(file.path(path, files[i]))
    if (!is.null(fun)) {
      dat$result <- fun(dat$result)
    }
    env[[hash[i]]] <- dat
    if (interactive()) setTxtProgressBar(pb, i)
  }
  env
}
