#' The MyClt function
#'
#' @param n number of population sampled
#' @param iter how many iterations you want
#' @param a lower bound
#' @param b upper bound
#'
#' @importFrom graphics hist
#' @importFrom grDevices rainbow
#' @importFrom stats runif
#'
#' @return blah
#' @export
#'
#' @examples w=myclt(n=50,iter=10000,a=5,b=10)
#'
globalVariables("x")
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nrow = n,ncol = iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(function(x) dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}

