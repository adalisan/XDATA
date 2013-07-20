#' Given a decreasingly sorted vector, return the given number of elbows
#' @param dat the decreasingly sorted vector (e.g. a vector of standard deviations)
#' @param n the number of returned elbows.
#' @param threshold either \code{FALSE} or a number. If threshold is a number, then all the elements in d that are not larger than the threshold will be ignored.
#' @param plot a flag to draw a screeplot or not.
#' @return q a vector of length n.
#’ @references
#’  Zhu, Mu and Ghodsi, Ali (2006), "Automatic dimensionality selection from
#'   the scree plot via the use of profile likelihood", Computational
#'   Statistics & Data Analysis
#' @export
getElbows <- function(dat, n = 3, threshold = FALSE, plot = FALSE) {
  if (is.matrix(dat))
    d <- sort(apply(dat,2,sd), decreasing=T)
  
  else
    d <- sort(dat,decreasing=T)

  if (!is.logical(threshold))
    d <- d[d > threshold]
  
  p <- length(d)
  if (p == 0)
    stop(paste("d must have elements that are larger than the threshold ",
               threshold), "!", sep="")

  lq <- rep(0.0, p)                     # log likelihood, function of q
  for (q in 1:p) {
    mu1 <- mean(d[1:q])
    mu2 <- mean(d[-(1:q)])              # = NaN when q = p
    sigma2 <- (sum((d[1:q] - mu1)^2) + sum((d[-(1:q)] - mu2)^2)) /
      (p - 1 - (q < p))
    lq[q] <- sum( dnorm(  d[1:q ], mu1, sqrt(sigma2), log=TRUE) ) +
      sum( dnorm(d[-(1:q)], mu2, sqrt(sigma2), log=TRUE) )
  }

  q <- which.max(lq)
  if (n > 1 && q < p) {
    q <- c(q, q + getElbows(d[(q+1):p], n-1, plot=FALSE))
  }

  if (plot==TRUE) {
      if (is.matrix(dat)) {
          sdv <- d # apply(dat,2,sd)
          plot(sdv,type="b",xlab="dim",ylab="stdev")
          points(q,sdv[q],col=2,pch=19)
      } else {
          plot(dat, type="b")
          points(q,dat[q],col=2,pch=19)
      }
  }

  return(q)
}
