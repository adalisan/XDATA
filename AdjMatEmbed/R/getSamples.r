#' Subsample the CharityNet Data
#'
#' @param samp number of samples of Donors
#' @return a list of subsampled Donors and associated Charities and Transactions.
#' @export
getSamples <- function(samp=10000)
{
    cat("getting samples...\n")
    
    require(plyr)

    ## first, clean up the data
    donorname <- url("http://www.cis.jhu.edu/~parky/XDATA/CharityNet/donors.csv")
    transname <- url("http://www.cis.jhu.edu/~parky/XDATA/CharityNet/transactions.csv")
    charityname <- url("http://www.cis.jhu.edu/~parky/XDATA/CharityNet/charities_location.csv")

    ## remove time=null from transaction
    transaction <- read.csv(transname)
    transaction$date <- revalue(as.character(transaction$date),c(null=NA))
    transaction <- transaction[complete.cases(transaction$date),]

    ucharity <- unique(transaction$charity_id)
    udonor <- unique(transaction$person_id)

    ## remove charities in transaction, but not in "charity" data
    charity <- read.csv(charityname)
    nocharity <- which(!(ucharity %in% charity$CharityAccountNumber))
    nocharityid <- ucharity[nocharity]
    nocharityidx <- which(transaction$charity_id %in% nocharityid)
    transaction <- transaction[-nocharityidx,]

    ## recalc unique donor & charity
    ucharity <- unique(transaction$charity_id)
    udonor <- unique(transaction$person_id)

    ## same for donor
    donor <- read.csv(donorname) 
    nodonor <- which(!(udonor %in% donor$accountNumber)) 
    nodonorid <- udonor[nodonor]
    nodonoridx <- which(transaction$person_id %in% nodonorid)
    transaction <- transaction[-nodonoridx,]

    ## donor,
    ndonor <- nrow(donor)
    set.seed(1234)
    sdonor <- sample(1:ndonor,samp,replace=FALSE)
    subdonor <- donor[sdonor,]

    tran_subdonor <- which(transaction$person_id %in% subdonor$accountNumber)
    subtran <- transaction[tran_subdonor,]
    usubdonor <- unique(subtran$person_id)
    subdonor <- subdonor[which(subdonor$accountNumber %in% usubdonor),]

    ## charity
    usubcharity <- unique(subtran$charity_id)
    subcharity <- charity[which(charity$CharityAccountNumber %in% usubcharity),]
    subcharity <- ddply(subcharity,.(CharityAccountNumber),head,1)
    ncharity <- length(subcharity$CharityAccountNumber)

    subtran$charity_id <- paste0("C",subtran$charity_id)
    subtran$person_id <- paste0("D",subtran$person_id)
    str(subtran)

    charity <- with(subcharity, data.frame(name=paste0("C",CharityAccountNumber),
                                           id=Name,
                                           city=City,
                                           state=State))
    str(charity)
    
    subdonor$name <- paste(subdonor$firstname,subdonor$lastname,sep=".")
    donor <- with(subdonor, data.frame(name=paste0("D",accountNumber),
                                       id=name,
                                       city=city,
                                       state=state))
    str(donor)

    return(list(donor=donor,charity=charity,subtran=subtran))
}
