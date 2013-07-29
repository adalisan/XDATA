#munge code

#preprocess Bitcoin dataset



LoadBitCoinData <- function(csvfile) { 
  if(exists("bc.ffdf")) 
    return(bc.ffdf)
  
  bc.ffdf <- read.delim.ffdf(file=csvfile, header=FALSE, quote="", sep="\t", na.strings=c(""), fill=TRUE,
                             colClasses = c(
                               "integer",
                               "integer",
                               "integer",
                               "POSIXct",
                               "numeric"))
  
  names(bc.ffdf) <- c("id","src","dest","time","amt")
  return(bc.ffdf)
}

BinColumn <- function(col.ff) {
  
  keys <- !duplicated(col.ff)
  keyIndices <- ffwhich(keys, keys == TRUE)
  
  print(paste("Extracting unique keys..."))
  
  keyCounts <-  ffdf(col.ff[keyIndices])
  names(keyCounts) <- c("key")
  
  chunks <- chunk(col.ff)
  
  if(length(chunks)==1){
    keyCounts <- as.ffdf( count(col.ff[1:length(col.ff)]) )
    names(keyCounts) <- c("key", "freq")
    freq <- keyCounts$freq
    print("Completed binning records")
  }
  else {
    for (i in 1:length(chunks)) {
      
      print(paste("Binning records, processing chunk",i,"of",length(chunks)))
      print(chunks[[i]])
      
      binnedChunk <- as.ffdf( count(col.ff[chunks[[i]]]) )
      names(binnedChunk) <- c("key", paste("freq",i,sep=""))
      #left join
      keyCounts <- merge(keyCounts, binnedChunk, by="key", all.x=TRUE)   
      
    }
    
    #consolidate the freq columns into a single column
    ncols <- ncol(keyCounts)
    nrows <- nrow(keyCounts)
    freq <- ff(vmode="integer", length=nrows)
    for (i in 2:ncols) {
      column <- as.ff(keyCounts[,i])
      
      #replace all NA with 0
      indices.NA <- ffwhich(column,is.na(column))
      
      if(length(indices.NA)>0){
        column[indices.NA] <- as.ff(ffrep.int(0, length(indices.NA)), vmode="integer")
      }
      freq <- freq+column
    }
  }
  
  binned.ffdf <- ffdf(key=keyCounts$key, 
                      freq=freq)
  
  
  ord <- fforder(freq, decreasing=TRUE)
  #apply the index
  binned.ffdf <- binned.ffdf[ord,]
  
  max.freq <- max(freq)
  print(paste("maximum freq", max.freq))
  
  return(binned.ffdf)
  
}




processBitcoin<- function(rawdata){
  require(lubridate)
  names(rawdata)<-c("trans_id", "from","to","time","amount")
  rawdata$time <- ymd_hms(rawdata$time)
  return(rawdata)
  
}
  