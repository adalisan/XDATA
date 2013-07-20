#' Function to build $Gj$ table
#'
#' @param subdonor data frame of Donors
#' @param samp flag to use a synthetic graph. if \code{FALSE}, use a weighted graph.
#' @seealso \code{\link{buildGtable}} 
#' @export
#' @return a list of dissimilarity matrix, probability matrix, and adjacency matrix.
buildGj <- function(subdonor,samp=TRUE)
{
    cat("building Gj...\n")
    
    require(cluster)
    
    nd <- nrow(subdonor)

    ## use daisy twice
    dmat1 <- daisy(as.data.frame(subdonor[,3]))
    dmat2 <- daisy(as.data.frame(subdonor[,4]))
    dmat <- dmat1 + 2*dmat2
    dmat[dmat>2] <- 2

    ## build dpg or similar, e.g., p_{uv} = e^{-1/2d^2_{uv}/c}
    set.seed(1234)
    puv <- exp(-1/4*dmat^2)
    Puv <- matrix(0,nd,nd)
    Puv[lower.tri(Puv)] <- puv
    Puv <- Puv + t(Puv)

    if (samp) {
        Aj <- rg.sample(Puv)
    } else {
        Aj <- Puv
    }
    rownames(Aj) <- colnames(Aj) <- as.character(subdonor[,1])

    return(list(D=dmat,P=Puv,A=Aj))
}
