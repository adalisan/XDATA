#' Sample an undirected graph on n vertices
#'
#' @param P \code{n times n} matrix giving the parameters of the Bernoulli r.v.
#' @return an Adjacency matrix
#' @export
rg.sample <- function(P){
  n <-  nrow(P)
  U <- Matrix(0, nrow = n, ncol = n)
  U[col(U) > row(U)] <- runif(n*(n-1)/2)
  U <- (U + t(U))
  A <- (U < P) + 0 ;
  diag(A) <- 0
  return(A)
}

#' an utility function
#' @param A n times n matrix
#' @return  the Laplacian
#' @export
nonpsd.laplacian <- function(A){
    n = nrow(A)
    s<-NULL
    #print(A)
   # print(str(A))
    if (attr(class(A),"package")=="Matrix") {
      s <- Matrix::rowSums(A)
    } else  {
      s <- rowSums(A)
    }
    L <- diag(s)/(n-1) + A

    return(L)
}

#' an utility function : Extract the spectral embedding coordinates from SVD decomposition of adjacency matrix
#' @export
svd.extract <- function(A, dim = NULL, scaling = FALSE){

    require(irlba)

    L <- nonpsd.laplacian(A)
    L.svd <- irlba(L, nu = dim, nv = dim)
#    L.svd <- svd(L)

    if(is.null(dim))
      dim <- scree.thresh(L.svd$d)

    L.svd.values <- L.svd$d[1:dim]
    L.svd.vectors <- L.svd$v[,1:dim]

    if(scaling == TRUE){
      if(dim == 1)
        L.coords <- sqrt(L.svd.values) * L.svd.vectors
      else
        L.coords <- L.svd.vectors %*% diag(sqrt(L.svd.values))
    }
    else{
      L.coords <- L.svd.vectors
    }

    return(L.coords)
}

#' scree.thresh is the elbow finding code of Zhu and Ghodsil
#' 
#' @param x
#' @param trace
#' @return a list containing top, the dimension that is 
#'          the maximum of the likelihood function 
#'          and lik, the likelihood value 
#' @export
scree.thresh <- function(x, trace=F, ...)
{
##      myvar <- function(x) {
##          if (length(x) == 1)
##              return (0)
##          else
##              return(var(x))
##      }

     x <- sort(x, decreasing=T);
     n <- length(x);
     lik <- rep(0, n);
     for (i in 1:(n-1)) {
#         u <- mean(x[1:i]); s1 <- myvar(x[1:i]);
#         v <- mean(x[(i+1):n]); s2 <- myvar(x[(i+1):n]);
         u <- mean(x[1:i]); s1 <- ifelse(length(x[1:i])==1,0,var(x[1:i]))
         v <- mean(x[(i+1):n]); s2 <- ifelse(length(x[(i+1):n])==1,0,var(x[(i+1):n]))
         s <- sqrt(((i-1)*s1+(n-i-1)*s2)/(n-2))
         lik[i] <- sum(dnorm(x[1:i], u, s, log=T)) +
                   sum(dnorm(x[(i+1):n], v, s, log=T))
     }

     u <- mean(x); s <- sqrt(var(x));
     lik[n] <- sum(dnorm(x, u, s, log=T))

     top<-which(lik==max(lik));

     if (!trace) return(top)
     else return(list(top=top, lik=lik))
}

#' Perform stfp embedding of graph from its adjacency matrix.
#' @param A n times n adjacency matrix
#' @param dim  the dimension to embed to
#' @param scaling
#' @export
inverse.rdpg <- function(A, dim, scaling = FALSE){
  if(class(A)=="igraph") {
      require(igraph)
      A <- lapply(A,get.adjacency)
  }
  
  if(is.list(A)){
    for(i in 1:length(A)){
      if(i == 1){
        X <- svd.extract(A[[i]], dim, scaling)
      }
      else{
        X <- cbind(X, svd.extract(A[[i]], dim, scaling))
      }
    }
  }
  else{
    X <- svd.extract(A, dim, scaling)
  }


  return(X)
}

#' Perform stfp embedding of graph from its adjacency matrix.
#' @param A n times n adjacency matrix
#' @param dim  the dimension to embed to
#' @param scaling
#' @export
inverse.rdpg.reduce.dim <- function(A, scaling = FALSE){
  maxdim <- 30
  init.embedding<- inverse.rdpg(A,dim=NULL,scaling)
}

#' Perform stfp embedding using out of sampling. First estimate X from A=XX^T using SVD, then estimate Y from Anew=XY^T  using Moore-Penrose  psuedoinverse
#'
#' @param A an n x n adjacency matrix
#' @param dim the dimension to embed into
#' @param Anew an n x m adjacency matrix of the bipartite graph between n  vertices and  m vertices of interest (the m vertices we wish to embed.)
#' @return X an m x dim embedded matrix
#' @export
inverse.rdpg.oos <- function(A, dim=2, Anew)
{
  require(Matrix)
  if (is.matrix(A))
    A<- Matrix(A)
  if (is.matrix(Anew))
    Anew<- Matrix(Anew)
  if (nrow(A) != nrow(Anew)) {
      Anew <- t(Anew)
  }

  if (nrow(A) != ncol(A)) { # already embedded
      X.is <- A
  } else {
      X.sample <- inverse.rdpg(A, dim, scaling = TRUE)
      X.is <- X.sample #$X
  }

  X.oos <- Matrix::t(solve(Matrix::t(X.is) %*% X.is) %*% Matrix::t(X.is) %*% Anew)

  return(list(X.is=Matrix(X.is),X.oos=X.oos))
}

#' dual oos
#'
#' @param Atable Atable
#' @param Xd stfp-ed embedding
#' @return X oos-ed X
dualoos <- function(Atable,Xd)
{

}
