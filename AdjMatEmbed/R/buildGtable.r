#' Function to build $G_{table}$ table
#'
#' @param data list of data frames from \code{\link{getSamples}}
#' @seealso \code{\link{buildGj}}, \code{\link{getSamples}}
#' @export
#' @return a list of largest connected component (igraph object), and a new data frame of of transactions
buildGtable <- function(data)
{
    cat("building Gtable...\n")
    
#    attach(data)
    donor <- data$donor
    charity <- data$charity
    subtran <- data$subtran
    
    actors <- rbind(donor,charity) 
    relations <- with(subtran, data.frame(from=person_id,
                                          to=charity_id,
                                          time=date,
                                          amount=amount))

#    detach(data)
    
    require(igraph,quietly=TRUE)
    g <- graph.data.frame(relations,directed=TRUE,vertices=actors)
    lcc <- giant.component(g)
    lccedge <- get.edgelist(lcc)

    subrel <- with(relations, which((from %in% lccedge[,1]) &
                                    (to %in% lccedge[,2]), arr.ind=TRUE))
    newrel <- relations[subrel,]
    newrel <- data.frame(from=as.character(newrel$from),
                         to=as.character(newrel$to),
                         time=as.character(newrel$time),
                         amount=newrel$amount)

    return(list(g=lcc,relations=newrel))
}

giant.component <- function(graph, ...) {
  cl <- clusters(graph, ...)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
