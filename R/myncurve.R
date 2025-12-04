#' Plot Normal Distribution Curve and Shade Area
#'
#' This function plots a normal distribution curve with a given mean (`mu`)
#' and standard deviation (`sigma`), then shades the area under the curve
#' up to a specified value `a`. It also computes the probability (area)
#' corresponding to P(X ≤ a).
#'
#' @importFrom graphics abline barplot curve hist layout mtext par polygon
#' @importFrom stats dnorm integrate pbinom pnorm qbinom qnorm rbinom
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a The upper limit for shading under the curve.
#'
#' @returns a list containing:
#' ' \describe{
#'   \item{mu}{Mean of the distribution used.}
#'   \item{sigma}{Standard deviation of the distribution used.}
#'   \item{area}{The calculated probability (area under the curve) up to `a`.}
#' }
#'
#' @export
#'
#' @examples
#' # Example: Plot N(0,1) and shade the area up to x = 1.5
#' myncurve(mu = 0, sigma = 1, a = 1.5)
#'
myncurve <- function(mu, sigma, a) {
  x <- NULL
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma), col = "blue", lwd = 2)
  area <- integrate(function(x) dnorm(x, mean = mu, sd = sigma), lower = -Inf, upper = a)$value
  polygon(c(mu - 3 * sigma, seq(mu - 3 * sigma, a, length.out = 100), a),
          c(0, dnorm(seq(mu - 3 * sigma, a, length.out = 100), mean = mu, sd = sigma), 0),
          col = "lightblue", border = NA)
  list(mu = mu, sigma = sigma, area = area)
}
