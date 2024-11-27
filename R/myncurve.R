#' My N Curve
#'
#' @param mu the mean
#' @param sigma the standard deviation
#' @param a the top bound for the shading
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @return a list containing mu, sigma, and the calculated probability
#' @export
#'
#' @examples myncurve(mu=10, sigma=2, a=12)

myncurve <- function(mu, sigma, a) {
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 1000)  # Define x explicitly

  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  x_fill <- seq(mu - 3 * sigma, a, length = 1000)
  y_fill <- dnorm(x_fill, mean = mu, sd = sigma)
  polygon(c(x_fill, a), c(y_fill, 0), col = "skyblue")

  prob <- pnorm(a, mean = mu, sd = sigma)

  return(list(mu = mu, sigma = sigma, P_X_less_than_a = prob))
}
