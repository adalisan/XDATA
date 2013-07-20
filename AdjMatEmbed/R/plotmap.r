#' Function to draw US maps with CharityNet transactions
#'
#' @param dat data frame of transactions
#' @param donor data frame of donors
#' @param charity data frame of charities
#' @param mcout vector of labels for coloring the mapping object
#' @param rng vector of min and max range of transaction amount
#' @param main character for the plot title
#' @export
plotmap <- function(dat,donor,charity,mcout=NULL,rng=NULL,main=NULL)
{
    require(maps,quietly=TRUE)
    require(R.utils,quietly=TRUE)

    data(us.cities)
    matchloc <- match(dat$to,charity$name)
    newdat <- cbind(dat,charity[matchloc,-1])

    ## decapitalize city names, then capitalize the 1st letter
    cityname <- tolower(newdat$city)
    cityname <- sapply(cityname,.simpleCap)
    newdat$city <- cityname
    
    cityidx <- apply(newdat, 1, function(x) which(us.cities$name %in% paste(x[6],x[7])))
    cityidx <- sapply(cityidx, function(x) ifelse(length(x)==0, NA, x))

    newdat <- cbind(newdat, us.cities[cityidx,4:5])
    mcidx <- matchloc

    newdat[newdat$state %in% c("AK","HI"),c("lat","long")] <- NA
    if (is.null(rng)) {
        rngamount <- range(newdat[!is.na(newdat$amount),]$amount)
    } else {
        rngamount <- rng
    }

    if (!is.null(mcout)) {
        if (length(mcout)==nrow(dat))
            newdat <- cbind(newdat,charity=factor(mcout))
        else
            newdat <- cbind(newdat,charity=factor(mcout[mcidx]))
    } else {
        newdat <- cbind(newdat,charity=factor(rep(1,length(mcidx))))
    }
    
    ## let's use gMap info
    matchid <- match(newdat$id, charity$id)
    newdat[,c("lat","long")] <- charity[matchid,c("lat","long")]

    newdat[newdat$state %in% c("AK","HI","PR"),c("lat","long")] <- NA
    
    library(ggplot2)
    all_states <- map_data("state")

    set.seed(12345)
    p <- ggplot()
    p <- p + geom_polygon(data=all_states,aes(x=long,y=lat,group=group),colour="white")
    p <- p + geom_jitter(data=newdat, position=position_jitter(width=0.5, height=0.5), aes(x=long, y=lat, size = amount, colour=charity), alpha=0.7)
    p <- p + scale_size_area(limit=rngamount,name="Donation")
    P <- p + theme(legend.position="top")
    p <- p + coord_map() #coord_fixed()
    p <- p + theme_clean()
    p <- p + labs(title=main)

    return(p)
}

.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}
