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
