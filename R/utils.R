
#' Wrap all functions in the given namespace
#'
#' Wrap all functions in the given namespace with another function.
#' This is especially useful in a package zzz.R file; for example
#' as used in the \code{\link{market_all}} function.
#'
#' @param f Function to wrap every other function with
#' @param env Environment to look for functions to wrap
#' @param ... Extra arguments to pass to the wrapping function
#'
#' @export
wrap_all_functions <- function(f, env = parent.frame(), ...) {
  funcs <- mget(ls(envir = env), envir = env, mode = "function",
                ifnotfound = NA)

  for (fname in names(funcs)) {
    func <- funcs[[fname]]
    if (!is.function(func)) {
      next
    }

    new_func <- f(func, ...)
    assign(fname, new_func, envir = env)
  }
}
