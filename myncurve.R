#' @title Curve Function
#'
#' @param mu mean of data
#' @param sigma standard deviation
#' @param a upper bound of probability
#'
#'
#'
#'
#' @return A plot that shows the probability that data is between certain parameters
#' @export myncurve
#' @importFrom graphics barplot curve hist layout mtext par polygon
#' @importFrom stats dnorm sd
#'
#'
#' @examples \dontrun{myncurve(mu, sigma,a)}
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x, mu, sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))
  xcurve <- seq(-3*sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(c(-3*sigma, xcurve, a), c(0, ycurve, 0), col = "light blue")
  prob <- round((pnorm(a, mu, sigma)), 4)
  text(x = a, y = 0.03,(paste0("Area = ", prob)))

  }
