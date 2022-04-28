#' @title My CLT
#' @description central limit theorem function
#' @param n sample size
#' @param iter iterations
#' @param a lower bound
#' @param b upper bound
#'
#' @return a histogram showing the sum of uniform distribution
#'
#' @export
#' @importFrom stats runif
#' @examples \dontrun{myclt(n=10,iter=10000,a=5,b=10)}
myclt=function(n,iter,a=0,b=5){
  x=NULL
  y=runif(n*iter,a,b) # A: this line creates a sample of size n*iter, a lower limit of 0, and an upper limit of 5. It stores this as the variable y
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE) #B: this line makes a matrix of information about the sample and stores it as the variable data
  sm=apply(data,2,sum) #C Adding up the margins of the matrix and storing it as the variable sm

  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}

