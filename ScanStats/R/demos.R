#' Demo for XDATA inference in CharityNet via graph embedding.
#'
#' @param method 1: methd1, 2: method2
#' @seealso \code{\link{demo2}} for XDATA Time Series of Graphs anomaly detection in CharityNet via scan statistics.
#' @export
demo1 <- function(method=1)
{
  ## load data: charity, donor, transaction
  data <- getSamples()
  #    attach(data)
  
  nd <- nrow(data$donor)
  nc <- nrow(data$charity)
  nt <- nrow(data$subtran)
  n <- nd+nc
  
  ## build graph
  gout <- buildGtable(data) 
  #    attach(gout)
  
  ## rebuild donor and charity
  udonor <- unique(gout$relations$from)
  donor <- donor[which(data$donor$name %in% udonor),] 
  ucharity <- unique(gout$relations$to)
  charity <- charity[which(data$charity$name %in% ucharity),]
  
  nd <- nrow(donor)
  nc <- nrow(charity)
  nt <- nrow(gout$relations)
  n <- nd+nc
  
  truelab <- as.character(charity$state)
  nuc <- length(unique(truelab))
  
  A <- gout$g[]
  Atable <- A[1:nd,(nd+1):n]
  #    detach(gout)
  
  if (method==2) {
    ## build Gj (donor)
    out <- buildGj(donor)
    Aj <- out$A
  }
  
  ## embed g using STFP
  cat("embdding a graph...\n")
  maxd <- 20
  if (method==1) {
    require(irlba)
    L <- irlba(Atable,maxd,maxd)
    stfp <- L$v[,1:maxd] %*% diag(sqrt(L$d[1:maxd]))
  } else {
    stfp <- inverse.rdpg.oos(Aj,maxd,Atable)
    stfp <- as.matrix(stfp$X.oos)
  }
  
  pcx <- prcomp(stfp)$x
  elbow <- getElbows(pcx,n=3,plot=TRUE)
  dat <- pcx[,1:elbow[3]]
  
  ## clustering
  cat("clustering charities...\n")
  require(mclust,quietly=TRUE)
  kmax <- length(unique(truelab))
  mc <- Mclust(dat,G=1:kmax)
  print(summary(mc))
  plot(mc,"BIC")
  
  ## calcl ARI
  cat("ARI(mc,truth) = ", adjustedRandIndex(mc$class,truelab), "\n")
}

#' Demo for XDATA Time Series of Graphs anomaly detection in CharityNet via
#' scan statistics.
#'
#' @param plotmap flag to draw map or not.
#' @seealso \code{\link{demo1}} for XDATA inference in CharityNet via graph embedding.
#' @export
demo2 <- function(plotmap=FALSE)
{
  data("X-15059x6")
  data("donor-8052")
  data("charity-756")
  data("heng")
  attach(heng)
  
  us.vn <- vertex.norm.stat[1,,] 
  them.vn <- vertex.norm.stat[2,,]
  us.tn <- temp.stat[1,]
  them.tn  <- temp.stat[2,]
  
  nbins <- ncol(them.vn)
  idx <- sapply(1:nbins,function(x) which(X$window==x)[1])
  datex <- sapply(idx,function(x)
    format(as.POSIXct(as.character(X[x,4]),format="%Y-%m-%d"),
           "%m/%d/%y"))
  
  require(reshape2)
  stat <- data.frame(date=1:nbins,us=us.tn,them=them.tn)
  stat <- melt(stat,id="date")
  colnames(stat) <- c("date","stats","value")
  
  require(ggplot2)
  require(reshape2)
  p1 <- ggplot(stat, aes(x=date,y=value,group=stats,linetype=stats))
  p1 <- p1 + geom_line() 
  p1 <- p1 + theme_bw()
  p1 <- p1 + xlab("time") + ylab("value")
  p1 <- p1 + scale_x_continuous(breaks=1:nbins,labels=datex)
  p1 <- p1 + theme(legend.position="top",
                   axis.text.x=element_text(angle=90))
  print(p1)
  ggsave(p1,filename="scanstats.pdf")
  
  if (plotmap) {
    require(igraph)
    require(animation)
    require(gridExtra)
    ani.options(outdir=paste0(getwd(),"/Anim"))
    rng <- c(1,10000)
    vname <- charity$name
    
    saveHTML({
      for (i in 1:nbins) {
        g <- glist[[i]]
        argv <- which.max(them.vn[,i])
        Nk <- unlist(neighborhood(g,1,argv)) # first one is the argv
        rel <- X[X$window==i,]
        rel <- rel[,c("from","to","date","amount")]
        cl <- rep("rest",nrow(rel))
        cl[which(rel$to %in% vname[Nk])] <- "N1(argv)"
        p2 <- plotmap(rel,donor,charity,mcout=cl,rng=rng,main=paste("Donated Amount for Each Charity, |N| =",length(Nk)))
        pbind = grid.arrange(p1+geom_vline(aes(xintercept=i,color='red')),p2)
        ani.pause()
      }
    }, img.name = "graphstat-b", verbose=FALSE,
             htmlfile = "index.html",
             single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0",
             description = c("tsg", "charity"),width=500,height=500)
  }
  
  detach(heng)
}

#' Quick demo for XDATA with preloaded object
#'
#' @seealso \code{\link{demo1}} and \code{\link{demo2}} for more detailed demos.
#' @export
helloworld <- function()
{
  data("relations-15059")
  data("charity-756")
  data("donor-8052")
  data("g-8808-15059")
  data("Aj-8052-c01-with-daisy")
  data("stfpout-A8052-D20-c01-scale-with-daisy")
  data("mc2-756-Khat17")
  
  nd <- nrow(donor)
  nc <- nrow(charity) # 756, 85
  n <- nd+nc # 8808, 67283
  truelab <- as.character(charity$state)
  
  A <- g[]
  Atable <- A[1:nd,(nd+1):n]
  
  stfpXc <- as.matrix(stfp$X.oos)
  pc <- prcomp(stfpXc)
  pairs(pc$x[,1:4],main="Gdonor + Gtable with PCA")
  elbow <- getElbows(pc$x,n=3,plot=TRUE)
  dat <- pc$x[,1:elbow[3]]
  print(summary(mc))    
  par(ask=TRUE)    
  plot(mc,"BIC")
  ari <- adjustedRandIndex(mc$class,truelab)
  cat("ARI(mc,truth) = ", ari, "\n")
  
  pmap <- plotmap(relations,donor,charity,mc$class)
  print(pmap)
}

