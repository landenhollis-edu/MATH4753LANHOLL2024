#' The myci Function
#'
#' @param x a vector of the data
#'
#' @importFrom stats qt
#'
#' @return
#' @export
#'
#' @examples
#'x <- rnorm(30, mean = 10, sd = 12); myci(x)
myci <- function(x) {
  # Sample size
  n <- length(x)

  # Sample mean
  x_bar <- mean(x)

  # Sample standard deviation
  s <- sd(x)

  # Critical t-value for 95% confidence interval
  t_critical <- qt(0.975, df = n - 1)  # 0.975 is the upper tail for a 95% CI

  # Margin of error
  margin_error <- t_critical * s / sqrt(n)

  # Confidence interval
  lower_bound <- x_bar - margin_error
  upper_bound <- x_bar + margin_error

  # Return the confidence interval as a vector
  c(lower_bound, upper_bound)
}



