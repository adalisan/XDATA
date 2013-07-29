
## Z: n x (#T x #F), where #T is the total time step, #F is the number of features,
## ndirs: #F, the number of features
DissMultVarTSSpline <- function(Z, ndirs)
{
  require(gtools)
  sDelta <- 0
  d <- ncol(Z)
  df <- d/ndirs
  tstep <- running(1:d,by=df,width=df,fun=function(x) x,simplify=F) # 1~481
  for (d in 1:ndirs) {
    cat("Working on d = ", d, "\n")
    ss <- t(apply(Z[,tstep[[d]]],1,function(x) smooth.spline(x)$y)) # spar=0.8 for "a lot"
    
    out <- dist(ss)^2
    sDelta <- sDelta + out/max(out)
  }
  sDelta <- sqrt(sDelta)
  return(sDelta)
}