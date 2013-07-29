byCidrCatWeek <- read.csv("agg_result.csv",header=FALSE)
names(byCidrCatWeek) <-c("cidr","week","hits","bytes","cat","SubCat","Country","Continent")

load("~/projects/XDATA/Datasets/akamai/CIDR/cidrGeo.Rdata")
load("~/projects/XDATA/Datasets/akamai/CIDR/byCidrCatweek.Rdata")
require(reshape2)

molten.data<- melt(byCidrCatWeek, measure.vars=c("hits","bytes"),id.vars=c("cidr","cat","week"))
molten.data <- molten.data[!(molten.data$cidr=="cidr=0.0.0.0/27"),]
#cast.data.tensor <- acast(molten.data,week~cat~variable~cidr)
cast.data <- acast(molten.data,variable+cat+week~cidr)

cast.data[is.na(cast.data)]<-0

## Z: n x (#T x #F), where #T is the total time step, #F is the number of features,
## ndirs: #F, the number of features
doSmoothing <- function(Z, ndirs)
{
  require(gtools)
  sDelta <- 0
  d <- ncol(Z)
  df <- d/ndirs
  tstep <- running(1:d,by=df,width=df,fun=function(x) x,simplify=F) # 1~481
  for (d in 1:ndirs) {
    cat("Working on d = ", d, "\n")
    ss <- t(apply(Z[,tstep[[d]]],1,function(x) smooth.spline(x)$y)) # spar=0.8 for "a lot"
    
    out <- dist(ss)^2
    sDelta <- sDelta + out/max(out)
  }
  sDelta <- sqrt(sDelta)
  return(sDelta)
}

DissMat<- doSmoothing(t(cast.data),36)




CIDR.embed<- cmdscale(DissMat,k=10)





country.vec<-unique(cidrGeo$country)
color.vec<- sample (brewer.pal(10,"RdYlGn"))
color.pch.pairs<- expand.grid(color.vec,1:4)
cidr.vec <- substring((rownames(CIDR.embed)),first=6)
countries.of.cidrs <- cidrGeo[match(cidr.vec ,cidrGeo$cidr),][,"country"]
subset.of.countries<- table(countries.of.cidrs)>10
country.vec.subset <- country.vec[subset.of.countries]

subset.of.cidrs<- countries.of.cidrs %in% country.vec.subset

CIDR.embed.subset<- CIDR.embed[subset.of.cidrs,]
countries.of.cidrs.subset <- cidrGeo[match(cidr.vec[subset.of.cidrs] ,cidrGeo$cidr),][,"country"]


colors.of.cidrs <- color.pch.pairs[match(countries.of.cidrs.subset
                                   , country.vec.subset),1]
pch.of.cidrs <- color.pch.pairs[match(countries.of.cidrs.subset
                                   , country.vec.subset),2]

points(CIDR.embed.subset[,1:2],  col = colors.of.cidrs,pch=pch.of.cidrs)
pairs(CIDR.embed.subset[,1:3],  col=colors.of.cidrs,pch=pch.of.cidrs)

