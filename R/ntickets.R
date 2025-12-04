#' Calculate number of tickets to sell for a flight given overbooking risk
#'
#' This function computes the number of tickets to be sold for a flight with
#' N seats, given the probability `p` that a passenger shows up and
#' gamma, the probability that the flight will be overbooked.
#'
#' The function calculates two ticket sale limits:
#' \itemize{
#'   \item \code{nd}: based on the discrete (binomial) distribution
#'   \item \code{nc}: based on the normal approximation
#' }
#' It also plots the objective functions for both methods.
#'
#' @param N Integer: Number of seats on the flight.
#' @param gamma Numeric: Probability that the flight is truly overbooked.
#' @param p Numeric: Probability that a passenger shows up.
#'
#' @return A named list containing:
#' \describe{
#'   \item{nd}{Number of tickets using discrete distribution.}
#'   \item{nc}{Number of tickets using normal approximation.}
#'   \item{N}{Number of seats.}
#'   \item{p}{Show up probability.}
#'   \item{gamma}{Overbooking probability.}
#' }
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#'
#' @export
ntickets <- function(N, gamma, p) {
  nd <- qbinom(1 - gamma, size = N, prob = p)
  nc <- qnorm(1 - gamma, mean = N * p, sd = sqrt(N * p * (1 - p)))
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
  obj_function_discrete <- function(n) {
    return(1 - gamma - pbinom(n, size = N, prob = p))
  }
  obj_function_continuous <- function(n) {
    return(1 - gamma - pnorm(n, mean = N * p, sd = sqrt(N * p * (1 - p))))
  }
  n_values <- seq(0, N * 2, by = 1)
  par(mfrow = c(2, 1))
  plot(n_values, obj_function_discrete(n_values), type = "l", col = "blue",
       xlab = "n", ylab = "Objective function", main = "Objective function vs n (Discrete)")

  abline(h = 0, col = "red")
  par(mfrow = c(2, 1))
  plot(n_values, obj_function_continuous(n_values), type = "l", col = "green",
       xlab = "n", ylab = "Objective function", main = "Objective function vs n (Continuous)")

  abline(h = 0, col = "red")
}


