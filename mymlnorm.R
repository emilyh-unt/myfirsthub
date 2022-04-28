#' @title normal maximum likelihood function
#'
#' @param x vectors for data values
#' @param mu mean
#' @param sig variance
#' @param ... other parameters
#'
#' @importFrom graphics contour text
#' @importFrom grDevices rainbow
#' @importFrom stats pnorm
#' @importFrom utils write.csv
#' @return contour plot showing the mean and variance for the normal distribution max likelihood
#' @export
#'
#' @examples \dontrun{mymlnorm(x=c(10,12,13,15,12,11,10),mu=seq(10,15,length=1000),
#' sig=seq(0.1,4,length=1000),lwd=2,labcex=1)}
mymlnorm=function(x,mu,sig,...){

  # length of mu and sigma
  nmu=length(mu)
  nsig=length(sig)

  # sample size
  n=length(x)
  zz=c()
  # log likelihood for normal dist.
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))
  for(j in 1:nsig){
    # matrix
    z=outer(x,mu,lfun,p=sig[j])
    y=apply(z,2,sum)

    zz=cbind(zz,y)
    ## zz is the matrix with each column containing log L values, rows difft mu, cols difft sigmas
  }
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),...)
  mlx=round(mean(x),2)
  mly=round(sqrt((n-1)/n)*sd(x),2)

  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  # Now find the estimates from the co-ords
  muest=mu[coord[1]]
  sigest=sig[coord[2]]

  abline(v=muest, h=sigest)
  return(list(x=x,coord=coord,maxl=maxl))
}

