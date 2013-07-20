#' Calculate times series of graph from the CharityNet's transaction data
#'
#' @param relations data frame of transaction data
#' @param nbins integer for the number of time bins
#' @return a list of times series graph in an igraph format
#' @export
buildTSG <- function(relations,nbins=25)
{
    require(igraph)
    require(chron)
    require(plyr)
    require(Matrix)
    
    udonor <- unique(relations$from)
    ucharity <- unique(relations$to)
    nd <- length(udonor)
    nc <- length(ucharity)
    
    dtparts <- t(as.data.frame(strsplit(as.character(relations$time),split=' ')))
    rownames(dtparts) <- NULL

    dts <- dtparts[,1]
    tms <- dtparts[,2]

    chronobj <- chron(dates.=dts, times.=tms,format=c('y-m-d','h:m:s'))
    numObj <- as.numeric(chronobj)
    numObj <- (numObj - min(numObj))/(max(numObj)-min(numObj))

    X <- data.frame(from=relations$from,
                    to=relations$to,
                    when=numObj,
                    date=dts,
                    amount=relations$amount)
    X <- X[order(X[,3]),]

    steps <- floor(nrow(relations)/nbins)
    idx <- rep(1:(nbins+1),each=steps,len=nrow(relations))
    X <- cbind(X,window=idx)

    ggg <- NULL
    for (i in 1:nbins) {
        rel <- X[X$window==i,]
        A <- spMatrix(nrow=nd,ncol=nc,
                      i=as.numeric(rel$from),
                      j=as.numeric(rel$to),
                      x=rep(1,length(as.numeric(rel$from))))
        row.names(A) <- levels(rel$from)
        colnames(A) <- levels(rel$to)
        ggg[[i]] <- graph.incidence(A,directed=TRUE)
    }

    return(ggg)
}
