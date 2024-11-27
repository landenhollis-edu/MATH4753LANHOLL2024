#' Bootstrap Function
#'
#' This function performs bootstrap resampling on the provided numeric vector `x`
#' and calculates the specified statistic using the specified function (`fun`).
#' It also generates a histogram of the bootstrap statistics along with confidence intervals.
#'
#' @param iter Number of bootstrap iterations. Default is 10000.
#' @param x A numeric vector of data values to be bootstrapped.
#' @param fun A function to compute the statistic of interest. Default is "mean".
#' @param alpha The significance level for the confidence interval. Default is 0.05.
#' @param cx Scaling factor for text size in plots. Default is 1.5.
#' @param ... Additional parameters passed to the histogram plotting function.
#'
#' @importFrom graphics segments text
#' @importFrom stats quantile
#'
#' @return A list containing the confidence interval, the function used, the original data, and the bootstrap statistics.
#' @export
#'
#' @examples
#' set.seed(123)
#' results <- myboot2(iter = 10000, x = rnorm(100), fun = mean, alpha = 0.05, cx = 1.5)
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n = length(x)  # sample size

  y = sample(x, n * iter, replace = TRUE)
  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun)  # xstat is a vector and will have iter values in it
  ci = quantile(xstat, c(alpha / 2, 1 - alpha / 2))  # Nice way to form a confidence interval

  # A histogram follows
  para = hist(xstat, freq = FALSE, las = 1,
              main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha=", alpha, " iter=", iter, sep = ""),
              ...)

  # mat will be a matrix that contains the data, this is done so that I can use apply()
  mat = matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)

  # pte is the point estimate
  # This uses whatever fun is
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")  # Vertical line
  segments(ci[1], 0, ci[2], 0, lwd = 4)  # Make the segment for the CI
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)

  # plot the point estimate 1/2 way up the density
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x, xstat = xstat))  # Some output to use if necessary
}
