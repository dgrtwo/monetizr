#' Monetize a function by displaying an ad before the function runs
#'
#' Monetize a function by displaying an ad before it runs, optionally
#' with a pause. Can also display HTML, which will appear either in the
#' RStudio "Viewer" window or (if the viewer window is not available)
#' in the.
#'
#' @param func Function to add advertising to
#' @param ads A character vector from which to pick a random ad
#' @param pause In seconds, an amount to wait after showing the
#' ad before running the function.
#' @param impression_url A URL to send a GET request to every time
#' a function is called.
#' @param html Whether the ad should be rendered as HTML
#'
#' @details
#'
#' If an \code{impression_url} is given, a GET request is sent along
#' with the parameters \code{function}, with the function name,
#' and \code{adId}, with the index of the ad that was selected.
#'
#' @examples
#'
#' mean(1:10)
#'
#' ad_mean <- marketably(mean, "Check out my blog, Variance Explained!")
#'
#' ad_mean(1:10)
#'
#' # We can insert a pause first
#' ad_mean <- marketably(mean, "Check out my blog, Variance Explained!",
#'                     pause = 3)
#' ad_mean(1:10)
#'
#' # or we can include HTML
#' ad <- paste0("Check out my blog, <a href = 'http://varianceexplained.org'>",
#'              "Variance Explained</a>!")
#'
#' ad_mean <- marketably(mean, ad, html = TRUE)
#' ad_mean(1:10)
#'
#' @export
marketably <- function(func, ads, html = FALSE, pause = NULL,
                       impression_url = NULL) {
  function_name <- as.character(substitute(func))

  func <- purrr::as_function(func)

  ret <- function(...) {
    # first display ad
    ad_id <- sample(length(ads), 1)
    msg <- ads[ad_id]

    if (html) {
      print(htmltools::browsable(htmltools::HTML(msg)))
    } else {
      message(msg)
    }

    # if requested, call a function that increments an impression counter
    if (!is.null(impression_url)) {
      # add function name
      url <- paste0(impression_url, "?function=",
                    URLencode(function_name),
                    "&adId=", ad_id)

      # perform without errors in case the impression doesn't work
      purrr::safely(httr::GET)(url, timeout = 2)
    }

    # if requested, insert a pause
    if (!is.null(pause)) {
        Sys.sleep(pause)
    }

    # now call the function and return result
    func(...)
  }

  ret
}


#' Make all the functions in a namespace (typically a package)
#' show ads before running.
#'
#' Call this function in the file zzz.R of a package in order to
#' advertise for all functions in the package.
#'
#' @param ... Extra arguments passed on to \code{\link{marketably}}
#'
#' @export
market_all <- function(...) {
  wrap_all_functions(marketably, parent.frame(), ...)
}
