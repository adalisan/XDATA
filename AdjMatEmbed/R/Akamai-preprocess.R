#' Reads all the traceroute  files where each row is one hop (in csv.gz) in a directory 
#' and returns  a list of igraph objects (or NULL) based on a time series of graphs view of the data
#' Filters the data to include only  hops  from vantage points in the vector vant.pt.of.interest and the first `n.hops` hops
#' @param ntimebins number of igraph objects (number of timesteps in TSG)
#' @param vant.pt.of.interest  an  integer  which is the   id number for the vantage point of interest
#' @param n.hops use only the first n.hops for each traceroute
#' @param csv.dir if NULL, searches the current directory for csv.gz files, otherwise  calls setwd(csv.dir)
#' @return a list of igraph objects (or NULL) based on a time series of graphs view of the data
#'  @export           
tracerouteToigraphList <- function(ntimebins,
                                   vant.pt.of.interest
                                   ,n.hops ,csv.dir=NULL
                                   , split.timespan=FALSE
                                   , aggreg.by.region=FALSE){
  require(lubridate)
  
  TSG<-list()
  if (!is.null(csv.dir)) setwd(csv.dir)
  edgelist.df.and.times <- tracerouteToEdgelist(
    vant.pt=vant.pt.of.interest,n.first.hops=n.hops
    ,csv.dir=NULL,ret.uniq.times=TRUE
    , aggreg.by.region=aggreg.by.region)
  edgelist.df<- edgelist.df.and.times$el
  time.pts <- edgelist.df.and.times$time.pts
  time.pts.lubridate<- origin+time.pts
  if (!is.null(ntimebins) & (ntimebins< length(time.pts))  ){
    
    deltat<-(max(time.pts)-min(time.pts))/ntimebins
    time.pts.sorted <- sort(time.pts)
    time.pts.groups <- split(time.pts.sorted,
                             cut(time.pts.sorted,breaks=ntimebins))
    
    for (timebin.i in 1:ntimebins){
      edgelist.df.time.int <- NULL
      if (split.timespan){
        start.int<- min(time.pts) + (timebin.i-1) *deltat
        end.int<-  min(time.pts) + (timebin.i) *deltat
        edgelist.df.time.int<- ffwhich(edgelist.df,
                                       time>=start.int &
                                         time<= end.int)
      } else  #if (split.time.pts)
      {  
        times.in.group <-  unlist(time.pts.groups[timebin.i])
        edgelist.df.time.int <- ffwhich(edgelist.df,
                                        time %in% times.in.group)
        
      }
      TSG.igraph <-NULL
      if (length(edgelist.df.time.int)>0) {
        try ( 
          final.edgelist <- 
            edgelist.df[edgelist.df.time.int,-1])
        
        if (aggreg.by.region){
          is.long.delay<- final.edgelist$delay>30 
        }
        final.edgelist <- as.data.frame(final.edgelist)
        final.edgelist <-cbind(final.edgelist,is.long.delay)
        
        
        TSG.igraph<- graph.data.frame(final.edgelist,directed=TRUE))
)
      }
TSG<-c(TSG,list(TSG.igraph))      
    }
  }

return(TSG)
}

#' Reads all the traceroute  files where each row is one hop (in csv.gz) in a directory 
#' and turns  a list of igraph objects (or NULL) based on a time series of graphs view of the data
#' Filters the data to include only  hops  from vantage points in the vector vant.pt.of.interest and the first `n.first.hops` hops
#' @param ntimebins number of igraph objects (number of timesteps in TSG)
#' @param vant.pt an  integer  which is the   id number for the vantage point of interest
#' @param n.first.hops
#' @param csv.dir if NULL, searches the current directory for csv.gz files, otherwise  calls setwd(csv.dir)
#' @param  ret.uniq.times if TRUE, the function returns a vector of integers representing the unique time steps (in unix time format) in the data.frame 
#' @details
#' The function assumes the traceroute files read are in csv.gz format that has
#' the following columns listed  ( name: data_type)
#' traceNum: integer (number of the traceroute)
#' vp: integer   (vantage point- the origin of the traceroute)
#' time: integer   (the time of the traceroute in unix time)
#' hop: integer     (index of the hop to reach this IP )
#' delay: numeric  time the traceroute takes to reach this hop - time the traceroute takes to reach prev hop  (which could be negative since the traceroute might have taken a shorter route to reach this IP)
#' IP: character (the IP address) 
#'               country: character 
#'                AS: character
#'                lat: numeric 
#'                lon: numeric
#' @export
tracerouteToEdgelist<- function(vant.pt=4149, n.first.hops=8
                                ,csv.dir=NULL
                                , ret.uniq.times=TRUE
                                ,return.cols = c("traceNum","IP" ,"time","delay")
                                ,aggreg.by.region=FALSE){
  TSG <- list()
  if (!is.null(csv.dir)) setwd(csv.dir)
  fnames<-Sys.glob("*.csv.gz")
  ffdf.edgelist.read <- NULL
  line.count<-10000
  time.pt.list <- c()
  for (filename in fnames) { # For each csv.gz file 
    
    file.conn <- gzfile(description=filename,open="r")
    print(paste("processing file ",filename))
    # read.chunk <- "a"
    continue.file.read <-TRUE
    line.ptr<-0           
    remains.of.prev.traceroute <-    data.frame(
      traceNum = integer(0)  , vp     = integer(0)
      ,  time   = integer(0)    , hop    = integer(0)
      ,  delay  = numeric(0)    , IP     = character(0)
      ,  country = character(0) , AS     = character(0)
      ,  lat  = numeric(0)      , lon    = numeric(0)
    )
    
    while (continue.file.read) {
      # runtime.chunk<- system.time({
      data.frame.read <- read.csv(file.conn, nrows=line.count
                                  , col.names = c(
                                    "traceNum", "vp" ,  "time" , "hop"
                                    , "delay",  "IP"  , "country"
                                    , "AS" ,    "lat",  "lon" )
                                  
                                  ,colClasses=
                                    c("integer","integer","integer"
                                      ,"integer", "numeric" ,"character"
                                      ,"character" ,"character"
                                      ,"numeric" ,"numeric")
                                  ,fill=TRUE
                                  ,sep=",", stringsAsFactors=FALSE
      )
      if (ret.uniq.times)   {
        time.pt.list<- union(time.pt.list
                             , unique(data.frame.read$time))
      }
      
      if (nrow(data.frame.read)==0) 
        continue.file.read<-FALSE
      data.frame.read <- rbind(  remains.of.prev.traceroute
                                 , data.frame.read)
      last.line <- nrow(data.frame.read)
      #find beginning of last traceroute in read chunk
      line.ptr <- tail( which(data.frame.read$hop==0),1)
      remains.of.prev.traceroute <- data.frame.read[line.ptr: last.line,]
      #remove the possible portion of traceroute
      data.frame.read<- data.frame.read[-(line.ptr: last.line),]
      
      data.frame.read$IP[data.frame.read$IP=="0.0.0.0"] <- NA
      
      df.rows <- nrow(data.frame.read)
      
      region.vec <- data.frame.read$country
      us.r.vec   <- region.vec=="US"
      us.r.vec[is.na(us.r.vec)] <- FALSE
      region.vec[us.r.vec] <- ifelse (data.frame.read$lon[us.r.vec]< -105,"WUS","EUS")
      
      #filter to get only the first n.first.hops and 
      #only those vantage point
      filter.row.var <-   (data.frame.read$hop>0  &
                             data.frame.read$hop <= n.first.hops) &
        (data.frame.read$vp %in% vant.pt)
      
      filter.col.var <- return.cols # traceNum, IP, time, delay
      
      data.frame.read.filt <- data.frame.read[filter.row.var,][,filter.col.var]
      region.vec <- region.vec[filter.row.var]
      
      if (aggreg.by.region) {
        data.frame.read.filt <- cbind(data.frame.read.filt,region.vec)
      }
      
      
      edgelist.with.attribs <- ddply(.data=data.frame.read.filt
                                     ,  .variables="traceNum"
                                     ,  .fun=hopsToEdges
                                     , vant.pt, aggreg.by.region)
      #} ) #end of system.tim
      #ffdf.read <- ffdfappend (ffdf.read, data.frame.read)
      if (nrow(edgelist.with.attribs)>0){
        ffdf.edgelist.read <- ffdfappend (ffdf.edgelist.read 
                                          , edgelist.with.attribs )
      }
    }
    print(paste("processed file ",filename))
  }
  
  if (ret.uniq.times) {
    return (list(el=ffdf.edgelist.read,time.pts=time.pt.list))
  } else {
    return(ffdf.edgelist.read)
  }
}

#' List of hops (each row is an IP) to edgelist (each row is an edge from one IP to another)
#' @param hoplist data.frame of hops
#' @param vant.pt ID number of vantage point hops originated from (also, the from vertex id in the first row of edgelist )
#' @return a data.frame that is an edgelist:has the vertices from and to  and time and delay attributes for each edge
#' @export
hopsToEdges <- function(hoplist,vant.pt ,aggreg.by.region=FALSE){
  num.hops<- nrow(hoplist)
  num.edges <- (num.hops)
  #num.attribs <- 2  #ncol(hoplist) - 2
  edge.list <- data.frame(v1=rep("",num.edges),
                          v2=rep("",num.edges),
                          time=rep(NA,num.edges),
                          delay= rep(NA,num.edges))
  if (!aggreg.by.region){
    edge.list[,2:4] <- hoplist[,2:4]
    if (num.hops==1) edge.list[1,1]<- as.character(vant.pt)
    else{
      edge.list[,1]<- c(as.character(vant.pt),hoplist[1:(num.edges-1),2])
    }
  } else{
    edge.list[,3:4] <- hoplist[,3:4]
    edge.list[,2] <- hoplist[,5]
    if (num.hops==1) { 
      edge.list[1,1]<- "VAPT"
    } else{
      edge.list[,1]<- c("VAPT"
                        ,as.character(hoplist[1:(num.edges-1),5]))
    }
    
  }
  return(edge.list)
}


traceroutesbyCC<- function(vant.pt){
  tracerouteToEdgelist ()
  
}



