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
