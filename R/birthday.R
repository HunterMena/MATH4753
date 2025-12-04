#' Birthday Problem
#'
#' Computes the probability that at least two people in a group of size `k`
#' share the same birthday, assuming 365 equally likely birthdays and ignoring leap years.
#'
#' @param k Integer. The number of people in the group.
#'
#' @returns A numeric value giving the probability that at least two people
#' in the group have the same birthday.
#' @export
#'
#' @examples
#' # Probability that two or more people among 20 share a birthday
#' birthday(20)
#'
#' # Probabilities for group sizes 20 through 24
#' birthday(20:24)
birthday <- function(k){
  1 - exp(lchoose(365, k) + lfactorial(k) - k*log(365))
}
