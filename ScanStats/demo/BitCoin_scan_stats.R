#rawdata <- read.delim("./data/JHU_sample_ 2013-06-18 .tsv", stringsAsFactors = FALSE )
#rawdata <- read.delim("./data/JHU_sample_11June2013.tsv", stringsAsFactors = FALSE )
library(ScanStats)
rawdata<- data(bitcoin)

if (!exists("rawdata")) {
  
  bitcoinfile <-   system.file("extdata","bitcoin-sample.tsv", package="ScanStats")
  
  if (!file.exists(bitcoinfile))
    stop("First, load data into `rawdata` variable ")
  rawdata <-filterBitcoin (bitcoinfile.tsv=bitcoinfile)
}

if (!exists("numt")) numt <-36
if (!exists("numt")) eq.time.int <- TRUE
#source("./R/preprocess.R")
readyData <- processBitcoin(rawdata)

#source("./R/TSG2ScanStats.R")
#source("./R/buildGtable.R")
#source("./R/localscan.r")

result <- buildGraph_at.T(readyData,numt,
                          div.by.equaltime.T.by.equalcount.F=eq.time.int
                          , build.weighted.graph=FALSE)

TheBig.TS.Graph <- result$TSG
timeseries.breaks <- result$TS.breaks


#Remove small graphs from TSG
small.graph.flag <- vapply  (TheBig.TS.Graph, function(graph.at.t) {
  print(class(graph.at.t))
  print(vcount(graph.at.t))
  return(vcount(graph.at.t)<500)
  

},FUN.VALUE=c(TRUE)
)

filter.small.graphs <- FALSE
if (filter.small.graphs){
TheBig.TS.Graph <- TheBig.TS.Graph[!(small.graph.flag)]
timeseries.breaks <- timeseries.breaks[!(small.graph.flag),]
#32 time intervals remaining in the TSG
}
Scan.Stats.List <- computeScanstats(TheBig.TS.Graph,k=1,us.stats=TRUE,them.stats=TRUE,tau=3, el=3)

stats.1.us<-  (Scan.Stats.List$us) 
stats.1.them <- (Scan.Stats.List$them) 
write.stats.to.csv("test.write.stats.csv",stats.1.us,
                   stats.1.them, timeseries.breaks)








