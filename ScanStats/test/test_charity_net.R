
# Sanity check
# Uses charitynet data to compare two functions  for  computing scan statistics
# scan statistics stored in list of named vectors (lnv), or large multidimensional matrices/arrays (array)
# The former (lnv) should be more efficient for large graphs.
# Author: Sancar
###############################################################################

require(testthat)

require(igraph)


#loading array's computed scan statistics from charitynet
load("./data/heng.RData")
attach(heng)

source("./R/TSG2ScanStats.R")
source("./R/buildGtable.R")
source("./R/localscan.r")

raw.scanstats <- compute.raw.Scanstats(glist)


tau <- 3
ell <- 4




#us statistic
v.norm.stats.lnv <- vertex.depend.normalize.stats(raw.scanstats,tau=tau,us.T.them.F=TRUE,TSG=glist,k=1)
v.max.stats.lnv  <- unlist(lapply(v.norm.stats.lnv,function (x) {max(unlist(x))}))
#computed in reverse time order compared to array


v.norm.array<- vertex.norm(raw.scanstats.mat,tau)
rownames(v.norm.array)<- rownames(raw.scanstats.mat)
v.max.array <- apply (v.norm.array,2,max)


all.equal(v.max.stats.lnv[(tau+1):length(glist)],
          v.max.array[(tau+1):length(glist)])

expect_that(
  v.max.stats.lnv[(tau+1):length(glist)],
            equals(v.max.array[(tau+1):length(glist)])
)



tmp.norm.stat.lnv<- unlist(temporal.normalize.stats(v.max.stats.lnv, el))
tmp.norm.array<-temp.norm(v.norm.array,tau=tau,ell=el)

all.equal(tmp.norm.stat.lnv[(tau+el+1):length(glist)],
          tmp.norm.array[(tau+el+1):length(glist)])

expect_that(
  tmp.norm.stat.lnv[(tau+el+1):length(glist)],
  equals(tmp.norm.array[(tau+el+1):length(glist)])
)


#them statistic
v.norm.stats.lnv.them<- vertex.depend.normalize.stats(raw.scanstats,tau=tau,us.T.them.F=FALSE,TSG=glist,k=1)
v.max.stats.lnv.them  <- unlist(lapply(v.norm.stats.lnv.them,function (x) {max(unlist(x))}))



num.v<-vcount(glist[[1]])


#vertex x (tau+1) x #time 
raw.scanstats.mat <- array(0, dim=c(num.v,tau+1,length(glist)))

	for (j in (tau+1):length(glist)){
	  for (k in 1:(tau)){
	      	raw.scanstats.mat[,k,j] <- local.scan.igraph(glist[[j]],glist[[j-k]],1)
	   }
	   raw.scanstats.mat[,tau+1,j] <- local.scan.igraph(glist[[j]],gp=NULL,1)
	   
   }

v.norm.array.them<- vertex.norm(raw.scanstats.mat,tau)
v.max.array.them <- apply (v.norm.array.them,2,max)


all.equal(v.max.stats.lnv.them[(tau+1):length(glist)],
          v.max.array.them[(tau+1):length(glist)])


expect_that(v.max.stats.lnv.them[(tau+1):length(glist)],
            equals(v.max.array.them[(tau+1):length(glist)]))




tmp.norm.stat.lnv.them<- unlist(temporal.normalize.stats(v.max.stats.lnv.them, el))
tmp.norm.array.them <-temp.norm(v.norm.array.them,tau=tau,ell=el)

all.equal(tmp.norm.stat.lnv.them[(tau+el+1):length(glist)],
          tmp.norm.array.them[(tau+el+1):length(glist)])

expect_that(tmp.norm.stat.lnv.them[(tau+el+1):length(glist)],
            equals(tmp.norm.array.them[(tau+el+1):length(glist)]))



detach(heng)