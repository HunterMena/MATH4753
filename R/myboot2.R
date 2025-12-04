#' Bootstrap Confidence Interval Function
#'
#' \code{myboot2()} performs bootstrap resampling on a numeric vector \code{x}
#' and returns a bootstrap confidence interval along with the bootstrap
#' statistics. This version is used for Task 8 of Lab 9.
#'
#' @param x A numeric vector of data values.
#' @param iter Number of bootstrap iterations (default = 10000).
#' @param fun A statistic function to apply to each resample.
#'   Can be "mean", "median", or any function that returns a numeric value.
#' @param alpha Significance level for the confidence interval
#'   (default = 0.05 gives a 95% confidence interval).
#' @param ... Additional arguments passed to the statistic function.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{ci} The bootstrap confidence interval.
#'   \item \code{xstat} Vector of bootstrap sample statistics.
#'   \item \code{x} The original data.
#'   \item \code{fun} The statistic used.
#' }
#'
#' @examples
#' data(ddt)
#' MATH4753::myboot2(x = ddt$DDT)
#'
#' @export
myboot2 <- function(iter = 10000, x, fun = mean, alpha = 0.05, ...) {

  # Check for invalid input
  if (is.logical(x)) {
    stop("x must not be logical")
  }
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  if (any(is.na(x))) {
    stop("x contains NA values")
  }

  # convert fun safely
  fun <- match.fun(fun)

  n <- length(x)

  # bootstrap statistics
  xstat <- replicate(iter, fun(sample(x, n, replace = TRUE)))

  # confidence interval
  ci <- quantile(xstat, c(alpha/2, 1 - alpha/2))

  list(
    ci = ci,
    xstat = xstat,
    x = x,
    fun = fun
  )
}
