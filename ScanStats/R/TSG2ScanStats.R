
#' Compute the (us and/or them) scan statistics for a time series of graphs
#'
#' @param TheBig.TS.Graph The Time series of graphs as a list of igraph objects 
#' @param k the local neighborhood radius to consider for the scan statistic. 
#' @param us.stats Compute "us" statistics 
#' @param them.stats Compute "them" statistics
#' @param tau Consider last 3 timesteps for vertex-dependent normalization of local scan statistics
#' @param el Consider last 3 timesteps for temporal normalization of maximum scan statistics
#' @return subset of the list of transactions
#' @export 
computeScanstats<- function(TheBig.TS.Graph,k=1,us.stats=TRUE,them.stats=TRUE,tau=3, el=3,
is.weighted.graph=FALSE){
  if (!(us.stats | them.stats))
    stop("Neither us or them statistic is chosen for computation")
  raw.stats<-compute.raw.Scanstats(TheBig.TS.Graph,k,is.weighted.graph)
  time.norm.stats.us<-NULL
  time.norm.stats.them<-NULL
  v.max.stats<- NULL
  if (us.stats){
  v.norm.stats <- vertex.depend.normalize.stats(raw.stats,tau=tau,us.T.them.F=us.stats,TSG=TheBig.TS.Graph,k=k)
  v.max.stats<- unlist(lapply(v.norm.stats,function (x) { v.norm.vec<- unlist(x); a  <- max(v.norm.vec)
                                                          names(a)<- names(which.max(v.norm.vec));
                                                          return(a)}))
  #print(names(v.max.stats))
  v.max.stats[1:tau]<-0
  time.norm.stats.us<- temporal.normalize.stats(max.stats.list=v.max.stats,el=el)
  #time.norm.stats.us<-v.max.stats
  time.norm.stats.us[1:(tau+el)] <- NA
  names(time.norm.stats.us)[1:(tau+el)] <- ""
  }
  if (them.stats){
    v.norm.stats <- vertex.depend.normalize.stats(raw.stats,tau=tau,us.T.them.F=(!them.stats),TSG=TheBig.TS.Graph,k=k)
    v.max.stats<- unlist(lapply(v.norm.stats,function (x) { v.norm.vec<- unlist(x); a  <- max(v.norm.vec)
                                                            names(a)<- names(which.max(v.norm.vec));
                                                            return(a)}))
    v.max.stats[1:tau]<-0
    print(names(v.max.stats))
    time.norm.stats.them <- temporal.normalize.stats(max.stats.list=v.max.stats,el=el)
    #time.norm.stats.us<-v.max.stats  
    time.norm.stats.them[1:(tau+el)] <- NA
    names(time.norm.stats.them)[1:(tau+el)] <- ""
  }
  return (list(us = time.norm.stats.us,them =time.norm.stats.them))
}


#' Get subset of the list of transactions that takes place from begin to end
#'
#' @param trans.list list of all transactions 
#' @param begin beginning of the time interval
#' @param end end of the time interval
#' @return subset of the list of transactions
#' @export 
getTrans.at.T<- function (trans.list, begin, end){
	subselect<- trans.list$time>=begin & trans.list$time < end
	return(trans.list[subselect,])
	
}


#' Build a Time series of graphs of length numt
#' from a list of interactions
#' 
#' @param trans.list List of all transactions, with as a data frame with "from","to","time" and "amount"  
#' @param numt : length of the time series
#' @param div.by.equaltime.T.by.equalcount.F  divide the transactions into timesteps
#'        by equal intervals of  time or by equal number of transactions
#'        @param build.weighted.graph
#' @return A time series of graphs where each graph at a time interval is
#'         the largest connected component of the unweighted graph that is composed of the transactions at that time interval
#' @export         
#'          
buildGraph_at.T <- function(trans.list,numt, div.by.equaltime.T.by.equalcount.F,build.weighted.graph) {
  if (is.data.frame(trans.list)){
    if (!all(c("from","to","amount","time") %in% names(trans.list)))
    {
      stop("at least one of the required columns   do not exist in the data-frame trans.list")
    } else {
      
      names(trans.list) <- c("trans_id","from","to","time","weight")
      
      
    }
  } else if (is.matrix(trans.list) | is.array(trans.list) )
  {
    if (dim(trans.list)[2]!=5) 
    {
      stop("trans.list does not have 4 columns. They should be correspond to 'from','to','amount','time'
respectively " )
    } else {
      colnames(trans.list) <- c("trans_id","from","to","time","weight")
      trans.list<- as.data.frame(trans.list)
    }
  }
  num.trans <- nrow(trans.list)
  ord<-NULL
  if ( !div.by.equaltime.T.by.equalcount.F)
  {
    num.trans.per.timestep <- ceiling(num.trans/numt)
    ord <-order (trans.list$time)
  }
  
	tsg<-list()
	begin.time <- min(trans.list$time)
	end.time <- max(trans.list$time)
	Whole.Time.Duration <- end.time  - begin.time
	TimeInt.length<- Whole.Time.Duration/numt
	trans.Time.List<-list()
  
  TS.breaks <- array(0, dim=c(0,2))
  for (time.i in 1:numt)
  {
    subtrans.list <-NULL
		#get the induced subgraph at T
    if ( div.by.equaltime.T.by.equalcount.F)
    {
		subtrans.list <- getTrans.at.T(trans.list, 
				begin.time + (time.i-1)*TimeInt.length,
				begin.time + (time.i)*TimeInt.length)  
      print("num of transactions")
      print(nrow(subtrans.list))
      subtrans.list <-  aggregate(x=list(weight=subtrans.list$weight)
                                  , by=list(from=subtrans.list$from,
                                            to=subtrans.list$to) 
                                  , FUN=sum)
      print("num of from-to pairs (edges)")
      print(nrow(subtrans.list))
      
		TS.breaks <- rbind(TS.breaks, 
                    cbind(begin.time + (time.i-1)*TimeInt.length,   
                   begin.time + (time.i)*TimeInt.length))
    } else {
      
      subselect<- ord[((time.i-1)*num.trans.per.timestep+1):
                        min((time.i)*num.trans.per.timestep,
                            num.trans)]
      subtrans.list<- trans.list[subselect,]
      TS.breaks <- rbind(TS.breaks, 
                         cbind(subtrans.list[1,]$time,   
                               subtrans.list[max(subselect),]$time))
      subtrans.list <-  aggregate(x=list(weight=subtrans.list[,c("weight")])
                                  , by=list(from=subtrans.list$from,
                                            to=subtrans.list$to) 
                                  , FUN=sum)
      
      
    }
    
		actors_at.T <- union(subtrans.list$from,subtrans.list$to)
		num_v_at.T  <- length(actors_at.T )
		
		print(str(actors_at.T))
		#adj_Mat    <- transList2adj_Mat(actors_at.T,subtrans.list)
		require(igraph,quietly=TRUE)
		#create the graph from adjacency matrix and 
		#find the largest connected component
    g<- NULL
    if (build.weighted.graph){
      
      g <- graph.data.frame(subtrans.list[,c("from","to","weight")],directed=TRUE)  
    } else {
      g <- graph.data.frame(subtrans.list[,c("from","to")],directed=TRUE)  
    }
		
		#lcc <- giant.component(g)
		
		# Find the edges in the LCC  
		# and prune the  graph and the sub-transactions list (at T)
		# to include those edges (transactions) only
		#lccedge <- get.edgelist(lcc)
		
		#subrel <- with(trans.list, which((from %in% lccedge[,1]) &
		#						(to %in% lccedge[,2]), arr.ind=TRUE))
		#newrel <- trans.list[subrel,]
		#newrel <- data.frame(from=as.character(newrel$from),
		#		to=as.character(newrel$to),
		#		time=as.character(newrel$time),
		#		amount=newrel$amount)
		
		#list(g=lcc,relations=newrel)
    tsg<- c(tsg, list(g))
	}
	return (list(TSG=tsg,TS.breaks=TS.breaks))
}

#' compute the "raw" local scan statistics of all the graphs in the time series
#' @param Graph.List  Time series of graphs as an R list
#' @param k  The radius of the neighborhood of the vertex to be considered for scan statistic 
#' @return The raw statistics as a list of named vectors. The names of the vectors are the vector ids.
#' @export
compute.raw.Scanstats<- function(Graph.List,k=1,is.weighted.graph=FALSE){
	numt <- length(Graph.List)
	us <- them <- list()
	for (i in 1:numt) {    
		print(paste("Computing stats at time ",i))
		na.v.names <- is.na(V(Graph.List[[i]])$name)
		V(Graph.List[[i]])$name[na.v.names]<-"0.0.0.0"
		#Compute scan stats at time i as a vector of numbers
    us.i <-NULL
    if (is.weighted.graph){
      us.i <- local.scan.igraph.weighted(Graph.List[[i]], k = k)
    } else{
		  us.i <- local.scan.igraph(Graph.List[[i]], k = k )
    }
		print(class(Graph.List[[i]]))
		# print(str(Graph.List[[i]]))
		
		# For "them" stats, use  same function with Gp(2nd arg) graph at t-1

		# Assign the names of the vertices to their scan stats and add list
		names(us.i) <- V(Graph.List[[i]])$name
		
		us<-c(us,list(us.i))
		
	}
	return (us)
}

#' Normalize the scan statistics for each vertex separately using the statistics most recent tau-length time window
#'@param stats.list  a list of named vectors where each named vector in the list contain statistics at one time interval 
#'                    and names in the named vectors     correspond to vertex ids. 
#' @param tau length of time window
#' @param us.T.them.F logical variable to choose computation of the `us` statistic (if TRUE) or `them` statistic (if FALSE) 
#' @param TSG the time series of graphs the scan statistics were computed from which is necessary if `them` statistic is being computed
#' @param k  The radius of the neighborhood of the vertex to be considered for scan statistic 
#' @return a list with the same structure (list of named vectors) as stats.list. The first tau elements will be 0, since
#'                    there is no tau-length history to normalize with respect to.
vertex.depend.normalize.stats <- function(stats.list, tau=0, us.T.them.F=TRUE,
		TSG=NULL, k=1) {
	numt <- length(stats.list)
	if (tau <= 0){
		return (stats.list)
	} else if (tau==1) {
		
		# starting from the end of time series
		# lapply'ed for efficiency
		stats.list <- lapply(numt:2, function(t.i,st.list){ 
					if (us.T.them.F){
						st.list[[t.i]] <- normalize_verts.at.T(st.list[[t.i]],
								st.list[[t.i-1]]
						)
					} else {
					  if(is.null(TSG))
					    stop("The original time series of graphs must be supplied with TSG argument")
					  
						#Use original TSG to compute  \phi stats only if necessary  
						
						#phi_1 is |V(G)|-length vector for \phi_{t.i,t.i-1,k}(v) v \in V(G)
						phi_1 <- local.scan.igraph(TSG[[t.i]] ,TSG[[t.i-1]],k)
						
						st.list[[t.i]]   <- normalize_verts.at.T(st.list[[t.i]],
								phi_1)        
					}      
				},
				stats.list
		)
		
	} else if (tau>1) {    
		stats.list <- lapply(numt:(tau+1), function(t.i,st.list){ 
					num.v <- length(st.list[[t.i]])
          
					v.name <- names(st.list[[t.i]])
					mean_phi_tau <- rep (0,num.v)
					sd_phi_tau <- rep (0,num.v)
					
					#phi_tau is |V(G)|x tau matrix for \phi_{t.i,t.i-j,k}(v) j=1:tau    v \in V(G)
					phi_tau= matrix(0,num.v,tau)
					if (!us.T.them.F){
            if(is.null(TSG))
              stop("The original time series of graphs must be supplied with TSG argument")
						for (j in 1:tau){
							# local.scan.igraph should be able to deal with
							# vertices at t.i not existing in t.i-j
							phi_tau[,j] <- local.scan.igraph(g=TSG[[t.i]], gp = TSG[[t.i-j]],k)
							
						}
					} else {
						#we can use the already computed local scan statistics.
						
						num.v<- length(v.name)

						phi_tau <- vapply( (t.i-(1:tau)), function(tau.i,st.list,v.n) {
									num.v <- length(v.n)
									tau_phi_tau.i <- rep(0,num.v)
									v.exists.at.tau <- v.n %in%  names(st.list[[tau.i]])
                  # the statistics corresponding to the vertices at 
                  # t.i, if they also exist at tau.i are
                  # accessed  from the named vector of stats.list[[tau.i]]
                  #
                  # the names of the vertices (v.name) are subselected using 
                  # v.exists.at.tau
									tau_phi_tau.i[v.exists.at.tau] <-  st.list[[tau.i]][v.n[v.exists.at.tau]]
									
									return(tau_phi_tau.i)
									
								}
								,FUN.VALUE = rep(1,num.v) ,  stats.list,v.name
						)
					}
						#print(dim(phi_tau))
						mean_phi_tau <- rowMeans(phi_tau)
						#print(mean_phi_tau)
						
						names(mean_phi_tau)<- v.name
						sd_phi_tau = apply(phi_tau,1,sd)
						#print(sd_phi_tau)
            #use replacement vals for stdev
					  sd_phi_tau[sd_phi_tau<1] <- 1
						names(sd_phi_tau)<- v.name
						
            st.list[[t.i]] <- normalize_verts.at.T.with_tau(st.list[[t.i]],
					                                                   mean_phi_tau, sd_phi_tau)
					
          
				},
				stats.list
		)
	} 
	return (c(rep(0,tau),rev(stats.list)))
}

normalize_verts.at.T <- function (stats_at.T,stats_at.T_minus_1) {
	verts_at.T <- names(stats_at.T )
  if (sum(is.na(verts_at.T))>0) {
    verts_at.T[is.na(verts_at.T)]<-"0.0.0.0"    
  }
	verts_at.Tmin_tau <- names(stats_at.T_minus_1 )
	
	lapply(verts_at.T, function(v) {
				#print(str(v))
				#print(v)
				tau.prev.stats <- 0
				#     if (!is.null(stats_at.Tmin_tau[[v]]))
				#	tau.prev.stats <-   stats_at.Tmin_tau[[v]]
				if (v %in%   verts_at.Tmin_tau)
					tau.prev.stats <-   stats_at.T_minus_1[[v]]
				
				stats_at.T[[v]] <- stats_at.T[[v]] - tau.prev.stats
			})
 
	return(stats_at.T)
}

#'
#'
#' stats_at_tau_window the scan stats
#' as a list in tau length time window (t-tau to t)
#' 
normalize_verts.at.T.with_tau <- function (stats_at.T,
		mean_phi_tau, sd_phi_tau) {
	
	verts_at.T <- names(stats_at.T )

	verts_at.T[is.na(verts_at.T)]<-"NoName"    
	
	stats_at.T <- lapply(verts_at.T, function(v,st_at.T,mean_recent_phi_tau,sd_recent_phi_tau) {
				#print(str(v))
				#print(v)
        
          
        if (sd_recent_phi_tau[v]!=0){
          st_at.T[[v]] <- (st_at.T[[v]] - mean_recent_phi_tau[v])/sd_recent_phi_tau[v]
				
        }
        else {
          st_at.T[[v]] <-0
          
        }
        
			},stats_at.T,mean_phi_tau,sd_phi_tau)
}

#' Normalize the maximum scan statistics at each time interval using the statistics most recent el-length time window
#'@param stats.list  a named vector that  contains the maximum scan statistics in the graph at each time interval 
#'                    and names in the named vector     correspond to vertex ids. 
#' @param el length of time window
#' @return a vector of same length as stats.list. The first (el) values will be 0, since   there is no el-length history to normalize with respect to

temporal.normalize.stats <- function(max.stats.list,el) {  
	numt <- length(max.stats.list)
  names.argmax.v <- names(max.stats.list)
	if (el<=0){
		return(max.stats.list)
	} else if (el==1){
		# starting from the end of time series
		# lapply'ed for efficiency
	  max.stats.list <- lapply(numt:2, function(t.i,stats.list){ 
					stats.list[[t.i]]<- stats.list[[t.i]] - stats.list[[t.i-1]]      
				},
				max.stats.list
		)
		
	} else if (el>1) {    
		t.i<- numt
    
		sd.el.window <- sd(max.stats.list[(t.i-el):(t.i-1)])
		mean.el.window <- mean(max.stats.list[(t.i-el):(t.i-1)])
		max.stats.list <- lapply( numt:(el+1), function(t.i,stats.list){ 
					sd.el.window <- sd(stats.list[(t.i-el):(t.i-1)])
					mean.el.window <- mean(stats.list[(t.i-el):(t.i-1)])
					sd.el.window[sd.el.window<1] <- 1
					stats.list[[t.i]] <- (stats.list[[t.i]]-mean.el.window)/
            sd.el.window
					
				},
				max.stats.list
		)
		
	} 
	names(max.stats.list) <-  names.argmax.v[numt:(el+1)] 
	return(c(rep(0,el),rev(max.stats.list)))
}


#'Writes the us and them statistics to a csv file along with the timesteps
#'@param fname Name of the filename
#'@param stats.us list of  scan statistics whose names correspond
#'@param stats.them
#'@param ts.breaks
#'@export
 write.stats.to.csv<-function(fname,stats.us, stats.them, ts.breaks){
   numt <- length(ts.breaks)-1
   argmax.v.us <- names(stats.us)
   argmax.v.them <- names(stats.them)
   
   write.csv( data.frame(Statistic_1=unlist(stats.us),vertex_with_max_stat_1=argmax.v.us,
              Statistic_2=unlist(stats.them),vertex_with_max_stat_2=argmax.v.them,
              begin_time = ts.breaks[,1],end_time = ts.breaks[,2] )
              ,file=fname,col.names=TRUE)
   
 }




#'Rearrange raw stats in list of named vector format(lnv) to matrix/ multidimensional array format 
#'@param raw.scanstats list of named vectors that stores the statistics
#'@export
lnv.to.mdim_array <- function (raw.scanstats) {
  vlist<-c()
  for (i in 1:length(raw.scanstats)){
    vlist<- c(vlist,names(raw.scanstats[[i]]))  
  }
  vlist <- unique(vlist)
  raw.scanstats.mat <- matrix(0, length(vlist),length(raw.scanstats))
  rownames(raw.scanstats.mat)<- vlist
  
  for (i in 1:length(raw.scanstats)){
    v.at.t <- names(raw.scanstats[[i]])
    raw.scanstats.mat[,i][vlist%in%v.at.t] = raw.scanstats[[i]][v.at.t]	
  }
  rownames(raw.scanstats.mat)<- vlist
  return(raw.scanstats.mat)
}

#' unix epoch time to POSIXct object
#' @param time an integer representing time in seconds unix epoch time
#' @return a POSIXCT object representation of time
unix2POSIXct  <-  function (time)   structure(time, class = 
                                                c("POSIXt", "POSIXct")) 

