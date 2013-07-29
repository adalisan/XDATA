#byCidrCatWeek <- read.csv("agg_result.csv",header=FALSE)
#names(byCidrCatWeek) <-c("cidr","week","hits","bytes","cat","SubCat","Country","Continent")

load("~/projects/XDATA/Datasets/akamai/CIDR/cidrGeo.Rdata")
load("~/projects/XDATA/Datasets/akamai/CIDR/byCidrCatweek.Rdata")
library(reshape2)
library(arrayhelpers)
library(RColorBrewer)


# molten.data.norm <- aggregate( data = byCidrCatWeek.sub,
#                                cbind(hits,bytes,cat) ~ cidr+week ,
#                                FUN=function(data.split){
#                                 sum.for.all.cat<- sum(data.split)
#                                 data.split<- data.split/sum.for.all.cat
#                                 return ((data.split))
#                               },simplify=TRUE)
# 

molten.data<- melt(byCidrCatWeek, measure.vars=c("hits","bytes"),id.vars=c("cidr","cat","week"))
molten.data <- molten.data[!(molten.data$cidr=="cidr=0.0.0.0/27"),]
# molten.data.plyr<- ddply(  .data=molten.data
#                          , .variables=c("cidr","variable","week")
#                          , .fun=function(data.split){
#                            data.split$value <- 
#                              data.split$value/sum(data.split$value) 
#                            return(data.split)
#                            })
# 

cast.data.tensor <- acast(molten.data,week~cat~variable~cidr)

cast.data.tensor[is.na(cast.data.tensor)]<- 0

hits.tensor<- cast.data.tensor[,,1,]
bytes.tensor<-cast.data.tensor[,,2,]
save(hits.tensor,file="Hits_tensor.RData")
save(bytes.tensor,file="Bytes_tensor.RData")

cast.data.tensor <- apply(cast.data.tensor,c(1,3,4),function(x){
                                      smm <- sum(x)
                                     if (smm!=0) {return(x/smm)} else return(x) } )
hits.tensor<- cast.data.tensor[,,1,]
bytes.tensor<-cast.data.tensor[,,2,]
save(hits.tensor,file="Hits_tensor_normalized.RData")
save(bytes.tensor,file="Bytes_tensor_normalized.RData")

molten.data.2<- array2df(cast.data.tensor
                         ,levels=list(cat=TRUE,week=TRUE,variable=TRUE,cidr=TRUE),label.x="value")
cast.data <- acast(molten.data.2,variable+cat+week~cidr)



#long wait
DissMat<- DissMultVarTSSpline(t(cast.data),36)



#longer wait
embed.dim <- 10
CIDR.embed<- cmdscale(DissMat,k=embed.dim)





display.brewer.pal(11,"RdYlBu")
color.vec<-  brewer.pal(9,"Set1")

color.pch.pairs<- expand.grid(color.vec,1:12)


country.vec<-unique(cidrGeo$country)
cidr.vec <- substring((rownames(CIDR.embed)),first=6)
countries.of.cidrs <- cidrGeo[match(cidr.vec ,cidrGeo$cidr),][,"country"]
countries.of.cidrs

CIDR.embed<- data.frame(CIDR.embed,countries.of.cidrs)





subset.of.countries<- table(countries.of.cidrs)>100

#display US or don't display US (crowded) 
subset.of.countries["US"]<-FALSE

country.vec.sub <- names(subset.of.countries)[subset.of.countries]

color.pch.pairs<- cbind(color.pch.pairs[1:length(country.vec.sub),],country.vec.sub)


subset.of.cidrs<- countries.of.cidrs %in% country.vec.sub

CIDR.embed.subset<- CIDR.embed[subset.of.cidrs,]

countries.of.cidrs.subset <- CIDR.embed.subset[,embed.dim+1]


colors.of.cidrs <- color.pch.pairs[match(countries.of.cidrs.subset
                                   , country.vec.sub),1]
pch.of.cidrs <- color.pch.pairs[match(countries.of.cidrs.subset
                                   , country.vec.sub),2]

plot((CIDR.embed.subset[,1:2]),  col = as.character(colors.of.cidrs) , pch=pch.of.cidrs)
pairs((CIDR.embed.subset[,1:3]),  col=as.character( colors.of.cidrs),pch=pch.of.cidrs)

save.image("CIDR.embed.vis.RData")

#VIS Demo

load("CIDR.embed.vis.RData")
open3d()
points3d(CIDR.embed.subset[,1:3], col=as.character( colors.of.cidrs),pch=pch.of.cidrs)
text3d(CIDR.embed.subset[,1:3],texts=countries.of.cidrs.subset,cex=0.6,col=as.character( colors.of.cidrs))

