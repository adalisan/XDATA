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
