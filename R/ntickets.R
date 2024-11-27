#' ntickets
#'
#' @param N The number of seats on the plane
#' @param gamma Probability that more passengers show up than there are seats
#' @param p Probability that a ticketed passenger will show up for the flight
#'
#' @importFrom graphics abline
#'
#' @return blah
#' @export
#'
#' @examples
#' ntickets(N = 100, gamma = 0.05, p = 0.9)


ntickets <- function(N, gamma, p) {
  # Discrete case (binomial distribution)
  objective_discrete <- function(n) {
    1 - gamma - pbinom(N, n, p)
  }

  # Find nd (optimal number of tickets using the discrete distribution)
  nd <- NA
  for (n in N:(N+50)) {
    if (objective_discrete(n) >= 0) {
      nd <- n
      break
    }
  }

  # Normal approximation case
  objective_continuous <- function(n) {
    mean <- n * p
    sd <- sqrt(n * p * (1 - p))
    1 - gamma - pnorm(N, mean, sd)
  }

  # Find nc (optimal number of tickets using the normal approximation)
  nc <- NA
  for (n in N:(N+50)) {
    if (objective_continuous(n) >= 0) {
      nc <- n
      break
    }
  }

  # Create a named list for the results
  result <- list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  )

  # Plot objective function vs n for discrete case
  n_values <- seq(N-5, N+50, by = 1)
  obj_discrete_values <- sapply(n_values, objective_discrete)
  obj_continuous_values <- sapply(n_values, objective_continuous)

  # Plot the discrete objective function
  plot(n_values, obj_discrete_values, type = 'b', col = 'blue', ylim = c(0, 1),
       xlab = 'n', ylab = 'Objective', main = 'Objective Vs n (Discrete)')
  abline(h = 0, col = 'red', lwd = 2)
  abline(v = nd, col = 'red', lwd = 2)

  # Plot the continuous objective function
  plot(n_values, obj_continuous_values, type = 'l', col = 'black', ylim = c(0, 1),
       xlab = 'n', ylab = 'Objective', main = 'Objective Vs n (Continuous)')
  abline(h = 0, col = 'blue', lwd = 2)
  abline(v = nc, col = 'blue', lwd = 2)

  # Print the results list
  print(result)
}
