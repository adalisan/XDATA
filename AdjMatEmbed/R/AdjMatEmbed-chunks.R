
#'Orders the vertices according to the number of edges
#'in the edgelist
#'@param complete_edgelist the list of edges as an ffdf object that has two columns, v1 and v2
#'@return a ffdf object that include the vertex list in `key` column and the count of incident edges for each vertex in `freq` column
#'@export
OrderVByEcount <- function (complete_edgelist) {
  v_1<- BinColumn(complete_edgelist$v1)
  v_2<- BinColumn(complete_edgelist$v2)
  
  v_all_rt <- merge(v_2,v_1,by="key",all.x=TRUE)
  keyCounts<- v_all_rt
  ncols <- ncol(keyCounts)
  nrows <- nrow(keyCounts)
  
  freq <- ff(vmode="integer", length=nrows)
  for (i in 2:ncols) {
    column <- as.ff(keyCounts[,i],vmode="integer")
    #replace all NA with 0
    indices.NA <- ffwhich(column,is.na(column))
    if(length(indices.NA)>0){
      column[indices.NA] <- as.ff(ffrep.int(0, length(indices.NA))
                                  , vmode="integer")
    }
    freq <- freq+column
  }
  max.ff(freq)
  V.list.by.Ecount.ffdf <- ffdf(key=keyCounts$key,
                      freq=freq)
  ord <- fforder(freq, decreasing=TRUE)
  V.list.by.Ecount.ffdf <- V.list.by.Ecount.ffdf[ord,]
  max.freq <- max(freq)
  print(paste("maximum freq", max.freq))
  return(V.list.by.Ecount.ffdf)
}


#' Embed the in-sample vertices of the graph by Adj Spectral Embedding
#' @param  V.list.by.Ecount.ffdf ffdf object that contains the list of all vertices sorted (in descending order) by the count of incident edges
#' @param complete_edgelist the list of edges as an ffdf object that has two columns, v1 and v2
#' @param core.v.chunk names of vertices in the in-sample group
#' @param v.chunk.size count of vertices to be embedded
#' @param embed.dim the embedding dimension
#' @param unique.edge.list if TRUE, Consider the unique entries in complete_edgelist, so that there exist only one edge per pair of vertices. In this case, the matrix to be embedded is an adjacency matrix.
#' @return a Matrix object (v.chunk.size x embed.dim) that contains the embedding coordinates.
#' @export
EmbedGraphCore <-function(V.list.by.Ecount.ffdf
                         ,complete_edgelist, core.v.chunk
                         ,v.chunk.size=2000, embed.dim =10, unique.edge.list=TRUE,scaling=TRUE){
  #Embed v.chunk.size vertices with highest number of edges (the ``core`` vertices)
  
  
  #v.chunk <- ffindexget ( V.list.by.Ecount.ffdf$key,as.ff(1:v.chunk.size ))
  edge.count<-dim(complete_edgelist)[1]

  b.1<- ffwhich(complete_edgelist,(v1 %in% core.v.chunk) & v2 %in% core.v.chunk )
  
  
  v1.sublist<- complete_edgelist[b.1,]
  
  if (unique.edge.list) 
    v1.sublist<- unique(v1.sublist)
  
  #v1.sublist<- ffdfindexget(,lender_lender_sublist_log)
  #kiva.lender_lender.edge.sublist.df<- data.frame(v1.sublist)
  vert_1 <- v1.sublist[,1]
  vert_2 <- v1.sublist[,2]
  bigMat<- as.matrix(cbind(as.character(vert_1),as.character(vert_2)))
  
  
  #get the adjacency matrix of the ``core`` graph
  kiva_Graph_lender_lender <- graph.edgelist((bigMat)
                                             , directed=FALSE)
  adj.mat<- get.adjacency(kiva_Graph_lender_lender)
  adj.mat.norm<-adj.mat
  
#   epsilon <-0.05
#   if (!unique.edge.list){
#    adj.mat.norm <-      adj.mat.norm/max(adj.mat.norm)
#  
#   #normalize rows and columns to get doubly stochastic  matrix
#   # This really needs to be repeated many times.
#   #adj.mat <- matrix(as.logical(adj.mat),v.chunk.size)
#     iter.count<- 0
#     while (max(adj.mat.nrom)<1+epsilon){
#       adj.mat.norm <- adj.mat.norm/rowSums(adj.mat.norm)
#       adj.mat.norm <- t(t(adj.mat.norm)/colSums(adj.mat.norm))
#       iter.count <- iter.count +1
#     }
#     print("iter.count  for adj.mat.norm", iter.count)
#     adj.mat.norm <- (t(adj.mat.norm)+adj.mat.norm)/2
#   }
   
  
  Embedded.coords <-  inverse.rdpg(A=adj.mat.norm,dim=embed.dim,scaling=scaling)
  rownames(Embedded.coords)<- as.character(core.v.chunk)
  return(Embedded.coords)
}




Embed.OOS.chunk <- function (chunk.i, complete_edgelist,
                             core.v.chunk, Embedded.coords,scaling=TRUE) {
  
  
  v.chunk.size <- length(chunk.i)
  core.v.size <- length(core.v.chunk)
  
  
  edge_sublist_log <- ffwhich(complete_edgelist,
                              v1 %in% chunk.i &
                              v2 %in% chunk.i)
  
  v1.sublist<- complete_edgelist[edge_sublist_log,]
  
  kiva.lender_lender.edge.sublist.df<- data.frame(v1.sublist)
  vert_1 <- kiva.lender_lender.edge.sublist.df[,1]
  vert_2 <- kiva.lender_lender.edge.sublist.df[,2]
  bigMat<- as.matrix(cbind(as.character(vert_1),as.character(vert_2)))
  #all_verts <- union(vert_1,vert_2)
  #kiva_Graph_periph_periph <- graph.edgelist((bigMat) , directed=FALSE)
  #adj.mat<- get.adjacency(kiva_Graph_periph_periph)
  
 
  edge_sublist_log <- ffwhich(complete_edgelist,
            (v1 %in% chunk.i)  & (v2 %in% core.v.chunk))
  edge_sublist_log_2 <- ffwhich(complete_edgelist,
                              (v2 %in% chunk.i)& (v1 %in% core.v.chunk))
  
  edge_sublist_log <- c(edge_sublist_log,edge_sublist_log_2)
  #v1.sublist<- ffdfindexget(complete_edgelist,edge_sublist_log)
  v1.sublist<- complete_edgelist[edge_sublist_log,]
  
  uniq.edgelist <- cbind(as.character(v1.sublist[,1]),
                         as.character(v1.sublist[,2]))
  
  #add self-loop edges to make sure all vertices appear in the edgelist
  # and are added to igraph
  uniq.edgelist<- rbind( uniq.edgelist,
                         cbind(as.character (core.v.chunk)
                               , as.character (core.v.chunk)),
                         cbind(as.character (chunk.i)
                               ,as.character (chunk.i))
  )
  
  uniq.edgelist<- unique(uniq.edgelist)
  
  
  
  bip.graph <- graph.edgelist(matrix(uniq.edgelist,ncol=2),directed=FALSE)
  
  
  A.test <- get.adjacency(bip.graph)
  A.table<- A.test[as.character(core.v.chunk)
                   ,as.character(chunk.i)]
  print(str(A.table))
  
  
  Embedded.coords.oos <-inverse.rdpg.oos(A=Embedded.coords,
                                    Anew=A.table,dim=dim)
  
  return(Embedded.coords.oos$X.oos) 
}





EmbedOOS <- function (X.is, Atable.edgelist ){
  
  
  uniq.edgelist<- unique( Atable.edgelist)
  v.is.names <- rownames(X.is)
  v.oos.names <- as.character( unique( uniq.edgelist[,2]))
  
  dim <- dim(X.is)[2]
  
  bip.graph <- graph.edgelist(matrix(uniq.edgelist,ncol=2),directed=FALSE)
  
  
  
  A.test  <- get.adjacency(bip.graph)
  A.table <- A.test [v.is.names ,v.oos.names]
  
  
  Embedded.coords.oos <-inverse.rdpg.oos(A=X.is,
                                         Anew=A.table,
                                         dim=dim)
  
}

