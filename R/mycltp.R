#' Simulate Sample Means from a Poisson Distribution
#'
#' This function demonstrates the Central Limit Theorem (CLT) using random samples
#' from a Poisson distribution. It repeatedly generates samples, computes their
#' means, and displays a histogram of the sample means with an overlaid theoretical
#' normal curve for comparison.
#'
#' @param n Integer. The sample size (number of observations in each iteration).
#' @param iter Integer. The number of iterations or samples to generate.
#' @param lambda Numeric. The rate parameter (λ) of the Poisson distribution. Defaults to 4.
#'
#' @return A histogram is produced in the active graphics device.
#' The function also (invisibly) returns a numeric vector of sample means for further analysis.
#'
#' @export
#'
#' @examples
#' # Example: Generate 10,000 samples of size 5 from Poisson(4)
#' mycltp(n = 5, iter = 10000, lambda = 4)
#'
#' # Larger sample size for smoother CLT behavior
#' mycltp(n = 20, iter = 10000, lambda = 10)
mycltp <- function(n, iter, lambda = 4) {
  y <- rpois(n * iter, lambda = lambda)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  w <- apply(data, 2, mean)
  param <- hist(w, plot = FALSE)
  ymax <- max(param$density) * 1.1
  hist(w, freq = FALSE, ylim = c(0, ymax),
       main = paste("Histogram of Sample Mean\nn =", n, ", lambda =", lambda),
       xlab = "Sample Mean")
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)),
        add = TRUE, col = "red", lty = 2, lwd = 3)
}
