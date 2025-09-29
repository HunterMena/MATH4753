#' Simulate Binomial Distribution and Plot
#'  This function simulates a binomial distribution using rbinom and produces a barplot of relative frequencies.
#' @param iter Number of iterations
#' @param n Number of trials
#' @param Probability of success
#'
#' @returns Relative frequencies as a table
#' @export
#'
mybin <- function(iter, n, p) {
  y <- rbinom(iter, n, p)
  rel.freq <- table(factor(y, levels=0:n)) / iter
  barplot(rel.freq, col="lightblue",
          main=paste("Binomial Simulation with", iter, "iterations"),
          xlab="Number of Successes", ylab="Relative Frequency")
  return(rel.freq)
}
