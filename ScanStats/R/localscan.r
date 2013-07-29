#' Calculate local scan statistics for each vertex in a graph
#'
#' @param g igraph object or matrix.
#' @param gp igraph object or matrix. It's typically at t=t-1, for "them" statistics.
#' @param k radii of the neighborhood.
#' @param mode one of "in", "out", "total".
#' @param FUN scan function. It's one of "ecount" or "vcount".
#' @return a vector of scan statistics, one for each vertex in the graph.
#' @export
local.scan.igraph <- function(g,gp=NULL,k=1,mode="out",FUN=ecount)
{
    if(k<0) stop("Error: k should be a non-negative integer!\n")
    if(k==0) FUN <- degree

    require(igraph)

    if (is.matrix(g) | is.matrix(gp)) {
        gmode <- ifelse((mode=="out" | mode=="in"),"directed","undirected")
        g <- simplify(graph.adjacency(g,mode=gmode))
        if (!is.null(gp)) 
            gp <- simplify(graph.adjacency(gp,mode=gmode))
    }
    
    n <- vcount(g)
    print("vcount")
    print(n)
    if (is.null(gp)) {
        if (k==0) out <- FUN(g,mode=mode)
        else out <- sapply(graph.neighborhood(g,k,V(g),mode),FUN)
    }
    else { # them
   
      verts.in.gp<-V(gp)
        if (k==0) 
            out <- unlist(sapply(1:(n),function(x) {
                FUN(induced.subgraph(gp, intersect(unlist(neighborhood(g,k+1,x,mode)),verts.in.gp) ),
                    mode=mode)  }))
        else out <- sapply(1:(n),function(x) {
                FUN(induced.subgraph(gp, intersect(unlist(neighborhood(g,k,x,mode)), verts.in.gp )))})
    }
    return(out)
}

local.scan.igraph.weighted <- function(g,gp=NULL,k=1,mode="out") {
  if(k<0) stop("Error: k should be a non-negative integer!\n")
  
  
  require(igraph)
  
  if (is.matrix(g) | is.matrix(gp)) {
    gmode <- ifelse((mode=="out" | mode=="in"),"directed","undirected")
    g <- simplify(graph.adjacency(g,mode=gmode))
    if (!is.null(gp)) 
      gp <- simplify(graph.adjacency(gp,mode=gmode))
  }
  fun.0 <- function (v,gr){
    e.list<-incident(graph=gr,v,mode)
  }
  fun.k <- function (v.nhood,gr){
    E(v.nhood)
  }
  fun.sum <- function (v.nhood){
    sum(v.nhood$weight)
  }
  
  n <- vcount(g)
  
  #print("vcount")
  #print(n)
  sc.stat <- list()
  if (is.null(gp)) {
    if(k==0) {
      sc.stat <- sapply(V(g), function(v,gr) {
                                         fun.sum(fun.0(v,gr))}
                                       ,g)
    } else {

      sc.stat <- sapply(graph.neighborhood(g,k,V(g),mode),
                        function(v.nhood,gr) {
                          fun.sum(fun.k(v.nhood,gr))
                        },
                        g)
      
    }
  } else { # them
    
    verts.in.gp<-V(gp)$name
    if (k==0) 
      sc.stat <- sapply(V(g),function(x) {
        sub.g <- induced.subgraph(gp, intersect(unlist(neighborhood(g,k+1,x,mode)),verts.in.gp),mode=mode)
        
                    fun.sum(fun.0(x,sub.g))   } )
    else sc.stat <- sapply(V(g),function(x) {
      sub.g <- induced.subgraph(gp, intersect(unlist(neighborhood(g,k,x,mode)), verts.in.gp ))
      fun.sum(fun.0(x,sub.g))
    })
  }
return(sc.stat)
}


#' Perform vertex normalization on time series of local scan statistics
#'
#' @param stat #vertex x #time matrix or #vertex x (tau+1) x #time array of local statistics
#' @param tau time window
#' @return a #vertex x #time normilized matrix of scan statistics
#' @export
vertex.norm <- function(stat,tau=1)
{
    if (is.matrix(stat)) {
        n <- nrow(stat)
        nbins <- ncol(stat)
        nstat <- matrix(0,n,nbins)
        for (i in 1:nbins) {
            if (i>tau) {
                muv <- apply(stat[,(i-tau):(i-1)],1,mean)
                sdv <- apply(stat[,(i-tau):(i-1)],1,sd)
                sdv[is.na(sdv)] <- 1
                nstat[,i] <- (stat[,i]-muv)/pmax(sdv,1)
            }
        }
    } else {
        dd <- dim(stat)
        n <- dd[1]
        nbins <- dd[3]
        nstat <- matrix(0,n,nbins)
        for (i in 1:nbins) {
            if (i>tau) {
                muv <- apply(stat[,(1:tau),i],1,mean)
                sdv <- apply(stat[,(1:tau),i],1,sd)
                sdv[is.na(sdv)] <- 1
                nstat[,i] <- (stat[,(tau+1),i]-muv)/pmax(sdv,1)
            }
        }
    }

    return(nstat)
}

#' Perform temporal normalization on time series of (normalized) local scan statistics
#'
#' @param stat #vertex x #time matrix of local statistics
#' @param ell time window
#' @return a vector of #time temporally normilized scan statistics
#' @export
temp.norm <- function(stat,tau=1,ell=0)
{
    nbins <- ncol(stat)
    Mtilde <- apply(stat,2,max)
    
    if (ell==0) {
        return(Mtilde)
    } else {
        muMtilde <- rep(0,nbins)
        sdMtilde <- rep(1,nbins)
        for (i in (ell+1):nbins) {
            muMtilde[i] <- mean(Mtilde[(i-ell):(i-1)])
            sdMtilde[i] <- sd(Mtilde[(i-ell):(i-1)])
        }
        
        sstat <- (Mtilde-muMtilde) / pmax(sdMtilde,1)
        sstat[1:(tau+ell)] <- 0

        return(sstat)
    }
}

