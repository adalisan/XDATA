#' extract cosine similarity between rows
#'
#' @param x numeric matrix or data frame
#' @export
cossim <- function(x) {
    ## fix rows with all 0s
    zrow <- which(rowSums(x)==0)
    if (length(zrow)!=0) x[zrow,] <- 1e-10
    y <- abs(x %*% t(x))
    res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
    res[res<1e-10] <- 0
    diag(res) <- 0
    return(res/2)
}
