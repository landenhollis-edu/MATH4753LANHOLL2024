#' The in between probability function
#'
#' @param x a number, which represents the upper bound of the probability to be found.
#'
#' @param y a number, which represents the lower bound of the probability to be found.
#'
#' @param n a number, the number of independent trials or experiments to be represented.
#'
#' @param p a decimal number, represents the probability of success on a single trial.
#'
#' @importFrom stats pbinom
#'
#' @return a number
#' @export
#'
#' @examples
#' betweenprob(x = 2,y = 8,n = 10,p = 0.6)

betweenprob <- function(x,y,n,p){
  pbinom(y, size = n, prob = p) - pbinom(x,size = n, prob = p)
}

