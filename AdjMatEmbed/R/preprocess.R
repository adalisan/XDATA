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




processBitcoin<- function(rawdata){
  require(lubridate)
  names(rawdata)<-c("trans_id", "from","to","time","amount")
  rawdata$time <- ymd_hms(rawdata$time)
  return(rawdata)
  
}


load.kiva.lenders <- function(sample.data=FALSE){
  
  require(ff)
  require(ffbase)
  
  try(setwd(file.path(Sys.getenv("PROJECT_DIR"),"XDATA/jhuxdata/")))
  if (sample.data){
    kiva.lenders.samp <- 
      read.delim.ffdf(file="./data/kiva//lenders.csv",header=FALSE, nrows=10
                      , quote="", sep="\t", na.strings=c(""), fill=TRUE,
                      colClasses = c("factor" ,"factor" ,"factor"
                                     ,"integer","integer"
                                     ,"POSIXct"
                                     ,"factor","factor","factor","factor"
                                     ,"factor","factor"
                                     ,"integer","integer" ,"factor"
                      )
      )
    return (kiva.lenders.samp)
  }
  kiva.lenders.raw <- read.delim.ffdf(file="./data/kiva//lenders.csv"
                                      ,header=FALSE, quote="", sep="\t", na.strings=c(""), fill=TRUE,
                                      colClasses = c("factor"
                                                     ,"factor"
                                                     ,"factor"
                                                     ,"integer"
                                                     ,"integer"
                                                     ,"POSIXct"
                                                     ,"factor"
                                                     ,"factor"
                                                     ,"factor"
                                                     ,"factor"
                                                     ,"factor"
                                                     ,"factor"
                                                     ,"integer"
                                                     ,"integer"
                                                     ,"factor"
                                      )
  )
  
  names.ff(kiva.lenders.raw) <- c( "lenders_uid"
                                   , "lenders_lender_id", "lenders_name"
                                   , "lenders_image_id", "lenders_image_template_id"
                                   , "lenders_member_since"
                                   , "lenders_whereabouts", "lenders_country_code"
                                   , "lenders_personal_url", "lenders_occupation"
                                   , "lenders_occupational_info"
                                   , "lenders_inviter_id", "lenders_invitee_count"
                                   , "lenders_loan_count", "lenders_loan_because")
  
  real.loaners<-ffwhich(kiva.lenders.raw,ifelse(kiva.lenders.raw$lenders_loan_count>0,1,0))                                     
  num.of.real.loaners<- sum.ff(real.loaners)
}

load.kiva.bbn <- function(kiva.file.loc){
  
  kiva.lender_lender.file <- file.path(kiva.file.loc,"lender_lender_loan.tsv")
  kiva.lender_lender.edgelist<-read.delim.ffdf(file=kiva.lender_lender.file
                                               ,header=FALSE, quote="", sep="\t", na.strings=c(""), fill=TRUE,
                                               colClasses = c("integer"
                                                              ,"integer")
  )
  return(kiva.lender_lender.edgelist)                                    
}

load.kiva.bbn.lender.partner <- function(kiva.file.loc){
  kiva.lender_partner.file <- file.path(kiva.file.loc
                                        ,"lender_partner_by_loan.tsv")
  
  kiva.lender_partner.edgelist<-read.delim.ffdf(file=kiva.lender_partner.file
                                                , header=FALSE, quote="", sep="\t"
                                                , na.strings=c(""), fill=TRUE,
                                                colClasses = c("integer"
                                                               ,"integer")
  )
  return(kiva.lender_partner.edgelist)                                    
  
}

load.kiva.bbn.partner.loan <- function(kiva.file.loc){
  kiva.partner_loan.file <- file.path(kiva.file.loc
                                      ,"partner_loan_by_loan.tsv")
  
  kiva.partner_loan.edgelist<-read.delim.ffdf(file=kiva.partner_loan.file
                                              , header=FALSE, quote="", sep="\t"
                                              , na.strings=c(""), fill=TRUE,
                                              colClasses = c("integer"
                                                             ,"integer")
  )
  return(kiva.partner_loan.edgelist)                                    
  
}


load.kiva.bbn.all.table <- function(kiva.file.loc) {
  
  kiva.all_table.file <- file.path(kiva.file.loc
                                   ,"loan_lender_borrower_partner.tsv")
  
  kiva.all_table <- read.delim.ffdf(file = kiva.all_table.file
                                    , header=FALSE, quote="", sep="\t"
                                    , na.strings=c(""), fill=TRUE,
                                    colClasses = c("integer"
                                                   ,"integer"
                                                   ,"integer"
                                                   ,"integer")
  )
  return (kiva.all_table)                                    
  
  
}





BinColumn <- function(col.ff) {
  
  keys <- !duplicated.ff(col.ff)
  keyIndices <- ffwhich(keys, keys == TRUE)
  
  print(paste("Extracting unique keys..."))
  
  keyCounts <-  ffdf(col.ff[keyIndices])
  names(keyCounts) <- c("key")
  
  chunks <- chunk(col.ff)
  
  if(length(chunks)==1){
    keyCounts <- as.ffdf( count (col.ff[1:length(col.ff)])
                          ,vmode=c('integer','integer'))
    names(keyCounts) <- c("key", "freq")
    freq <- keyCounts$freq
    print("Completed binning records")
  }
  else {
    for (i in 1:length(chunks)) {
      
      print(paste("Binning records, processing chunk",i,"of",length(chunks)))
      print(chunks[[i]])
      
      binnedChunk <- as.ffdf( count(col.ff[chunks[[i]]])
                              ,vmode=c('integer','integer'))
      names(binnedChunk) <- c("key", paste("freq",i,sep=""))
      #left join
      keyCounts <- merge(keyCounts, binnedChunk, by="key", all.x=TRUE)   
      
    }
    
    #consolidate the freq columns into a single column
    ncols <- ncol(keyCounts)
    nrows <- nrow(keyCounts)
    freq <- ff(vmode="integer", length=nrows)
    for (i in 2:ncols) {
      column <- as.ff(keyCounts[,i],vmode="integer")
      
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
