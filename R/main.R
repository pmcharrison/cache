#' Cache a function's outputs
#'
#' Caches a function's outputs to avoid repeated computation.
#' By default, the cache is saved to the working directory,
#' within the sub-directory \code{/.cache}.
#' In-memory caches are also supported,
#' by passing an memory-cache object (as created by \code{\link{cache_memory}()})
#' to the argument \code{.cache_memory}.
#'
#' Unlike most existing memoisation packages,
#' \code{cache} is well-suited for memoising functions within R packages.
#' Its default arguments are chosen so that, in most cases,
#' a function can be cached simply by wrapping its body in a call to
#' \code{cache()}.
#'
#' \code{cache()} should be used as a wrapper for the function's body.
#' The code within the body will be memoised.
#' It is possible to add additional code before the call to \code{cache()};
#' such code is typically used to preprocess the function arguments
#' to make memoisation more efficient.
#' For example, if one of the inputs is a vector representing an unordered set,
#' it might be sensible to sort this vector before making the call to \code{cache()}.
#'
#' @param body (R code) The body of the function to memoise,
#' typically wrapped in curly braces (\code{{}}).
#' @param .cache (Logical scalar)
#' Whether or not to use cache on the current call.
#' If \code{FALSE}, no results are loaded or saved to the cache.
#' @param .cache_ignore (Character scalar)
#' By default, all function arguments beginning with '.cache' are ignored for
#' caching purposes; these variables are assumed not to affect function outputs.
#' Additional arguments to ignore can be specified with this parameter.
#' @param .cache_memory (Logical scalar)
#' Either \code{NULL} (default),
#' in which case a file-based cache will be used,
#' or an in-memory cache as created by \code{\link{cache_memory}()}.
#' @param .cache_dir (Character scalar)
#' Directory in which to save cached files.
#' If \code{NULL},
#' a directory is constructed within the working directory,
#' representing the name of the calling function
#' and the package within which the function was defined (if applicable).
#' @param .cache_force (Logical scalar)
#' If \code{TRUE}, an error will be thrown if the current call cannot be found
#' in the cache. Useful for debugging.
#' @examples
#' # Define a simple cached function
#' f <- function(x, ...) cache({
#'   message("Computing...")
#'   c(x, rnorm(1))
#' }, ...)
#' f(1)
#' f(1) # produces the same result
#' f(1, .cache = FALSE) # produces a different result
#' @note Caching in the \code{cache} package does not check for changes
#' in the code of the memoised function.
#' It is the user's responsibility to clear the cache after
#' updating the source code.
#' @export
cache <- function(
  body,
  .cache = TRUE,
  .cache_ignore = character(),
  .cache_memory = NULL,
  .cache_dir = NULL,
  .cache_force = FALSE
) {
  parent_env <- parent.frame()
  pkg_name <- utils::packageName(parent_env)
  fun_name <- gsub("^.*::", "",
                   deparse(sys.call(-1)[[1]]))
  if (is.null(.cache_dir)) {
    .cache_dir <- if (is.null(pkg_name))
      file.path(".cache", "global", fun_name) else
        file.path(".cache", "packages", pkg_name, fun_name)
  }
  cache_info <- if (.cache) get_cache_info(
    fun_name = fun_name,
    parent_env = parent_env,
    cache_dir = .cache_dir,
    ignore_args = .cache_ignore,
    cache_mem = .cache_memory
  )
  if (!is.null(cache_info) && cache_info$result_found) {
    res <- cache_info$result
  } else {
    # We evaluate the expression in the parent environment,
    # i.e. the body of the function that called cache().
    # This prevents namespace clashes with temporary
    # variables created within the cache() function.
    if (.cache_force) stop("Cached result not found.")
    res <- eval(body, envir = parent_env)
    if (.cache) .save_cache(result = res,
                            cache_info = cache_info,
                            cache_mem = .cache_memory)
  }
  res
}

#' Memory cache
#'
#' Creates an empty in-memory cache that can be filled by subsequent calls
#' to \code{\link[cache]{cache}()}.
#' @export
cache_memory <- function() {
  x <- new.env()
  class(x) <- c("cache_memory", class(x))
  x
}

#' Save memory cache
#'
#' Saves a memory cache from file.
#' @param x Object of class \code{cache_memory}.
#' @param file (Character scalar) Output file path.
save_cache <- function(x, file) {
  stopifnot(is(x, "cache_memory"))
  saveRDS(x, file)
}

#' Load memory cache
#'
#' Loads a memory cache from file,
#' producing an object of class \code{cache_memory}.
#' @param file (Character scalar) File path.
load_cache <- function(file) {
  x <- readRDS(file)
  if (!is(x, "cache_memory"))
    stop(file, " was not a valid memory-cache object")
  x
}

#' @export
print.cache_memory <- function(x, ...) {
  n <- length(x)
  cat("An in-memory cache containing",
      n, ngettext(n, "result", "results"),
      "(see ?cacher::cache_memory)\n",
      sep = " ")
}

#' Clear cache
#'
#' Clears a file-based memory cache.
#' @param path (Character scalar)
#' Directory to clear; defaults to the cache package's default directory.
#' @export
clear_cache <- function(path = ".cache") {
  unlink(path, recursive = TRUE)
}

# #' @param path Folder containing RDS files to be loaded
# #' @param fun Optional function to be applied to the cached result, should return an updated version of the result
# #' @export
# load_cache <- function(path, fun = NULL) {
#   files <- list.files(path, pattern = "\\.rds$")
#   hash <- gsub("\\.rds", "", files)
#   env <- cache_memory()
#   if (interactive()) pb <- utils::txtProgressBar(max = length(files), style = 3)
#   for (i in seq_along(files)) {
#     dat <- readRDS(file.path(path, files[i]))
#     if (!is.null(fun)) dat$result <- fun(dat$result)
#     env[[hash[i]]] <- dat
#     if (interactive()) utils::setTxtProgressBar(pb, i)
#   }
#   env
# }

get_cache_info <- function(fun_name,
                           parent_env,
                           cache_dir,
                           ignore_args,
                           cache_mem = NULL) {
  args <- as.list(parent_env)
  ignore <- union(ignore_args,
                  grep(pattern = "^\\.cache", x = names(args), value = TRUE))
  keep <- setdiff(names(args), ignore)
  args <- args[sort(keep)]
  hash <- digest::digest(list(fun_name = fun_name, args = args))

  cache_file_path <- NULL

  dat_loaded <- if (!is.null(cache_mem)) {
    cache_mem[[hash]]
  } else {
    cache_file_path <- file.path(
      cache_dir,
      paste0(hash, ".rds")
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

.save_cache <- function(result, cache_info, cache_mem) {
  out <- list(fun_name = cache_info$fun_name,
              args = cache_info$args,
              result = result)
  if (is.null(cache_mem)) {
    R.utils::mkdirs(dirname(cache_info$cache_file_path))
    saveRDS(out, cache_info$cache_file_path)
  } else {
    cache_mem[[cache_info$hash]] <- out
  }
}
