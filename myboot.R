#' @title Myboot function
#'
#' @description
#' @param iter iterations
#' @param x desired sample
#' @param fun desired parameter
#' @param alpha confidence level
#' @param cx size of text
#' @param ... continues
#'
#' @importFrom stats quantile
#' @importFrom graphics abline segments
#' @return a histogram and mean of the sample
#' @export
#'
#' @examples \dontrun{myboot2(iter=1000,x, fun="mean",alpha=0.05,xlab="mean",cx=1.5,...)}
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE) #This will create samples of size n with replacement, meaning once the sample is chosen, it will be "put back" in the population and will be able to be randomly selected again.

  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2)) #forming a confidence interval

  #Parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""))


  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)


  return(list(ci=ci,fun=fun,x=x))
}
