
csv.dir <- paste(Sys.getenv("PROJECT_DIR"),"/XDATA/Datasets/akamai/akamai_merged",sep="")
#csv.dir <- "~/my_documents/XDATA/data/Akamai/traceroute/"
tr.igraph.list <- tracerouteToigraphList(csv.dir=csv.dir,n.hops=7,ntimebins=36,split.timespan=FALSE, vant.pt.of.interest=4149,aggreg.by.region=FALSE)

library(ScanStats)
non.null.elem<-which(!unlist(lapply(tr.igraph.list$TSG,is.null)))
akamai.tr.scanstats <-computeScanstats(
  TheBig.TS.Graph=tr.igraph.list$TSG[non.null.elem]
  ,k=1 ,us.stats=TRUE,them.stats=TRUE, tau=5,el=5,
                                       is.weighted.graph=FALSE)


#Nodes aggregated by region


tr.igraph.list.country <- tracerouteToigraphList(csv.dir=csv.dir,n.hops=6,
                                                 ntimebins=36,split.timespan=FALSE, vant.pt.of.interest=4149, aggreg.by.region=TRUE)



non.null.elem<-which(!unlist(lapply(tr.igraph.list.country$TSG,is.null)))

scanstats.ak.tr.country<- computeScanstats(
  TheBig.TS.Graph=tr.igraph.list.country$TSG[non.null.elem]
  ,k=1 ,us.stats=TRUE,them.stats=TRUE, tau=5,el=5,
  is.weighted.graph=TRUE)






#Nodes aggregated by region and count long delays


tr.igraph.list.country.long.delay <- tracerouteToigraphList(csv.dir=csv.dir,n.hops=6,
                                                 ntimebins=36,split.timespan=FALSE, vant.pt.of.interest=4149, aggreg.by.region=TRUE,count.long.delays=TRUE)


#tr.igraph.list.country.avg.delay<-sapply(tr.igraph.list.country$TSG,function(x){if(is.null(x)) return(x);E(x)$weight   <-E(x)$avg_delay; return(x)}

non.null.elem<-which(!unlist(lapply(tr.igraph.list.country.long.delay$TSG,is.null)))

scanstats.ak.tr.country.long.delay <- computeScanstats(
  TheBig.TS.Graph=tr.igraph.list.country.long.delay$TSG[non.null.elem]
  ,k=1 ,us.stats=TRUE,them.stats=TRUE, tau=5,el=5,
  is.weighted.graph=TRUE)



#Multiple vantage points
tr.igraph.list.mult.vp <- tracerouteToigraphList(csv.dir=csv.dir,n.hops=7,ntimebins=36,split.timespan=FALSE, vant.pt.of.interest=c(4149,6432),aggreg.by.region=FALSE)






#Test runs


csv.dir.test<-paste(csv.dir,"/test_dir2",sep="")

tr.igraph.list.mult.vant.pt <- tracerouteToigraphList(csv.dir=csv.dir.test,n.hops=5,ntimebins=36,split.timespan=FALSE, vant.pt.of.interest = c(4149,3795,6432),aggreg.by.region=FALSE)



csv.dir.test<-paste(csv.dir,"/test_dir2",sep="")
tr.edgelist.country <- tracerouteToEdgelist(
  vant.pt=9857, n.first.hops=5
  ,csv.dir=csv.dir.test,ret.uniq.times=TRUE
  , aggreg.by.region=TRUE)


tr.igraph.list.country <- tracerouteToigraphList(csv.dir=csv.dir.test,n.hops=5,ntimebins=3,split.timespan=FALSE, vant.pt.of.interest=9857,aggreg.by.region=TRUE)





csv.dir.test<-paste(csv.dir,"/test_dir2",sep="")
tr.edgelist.country <- tracerouteToEdgelist(
  vant.pt=9857, n.first.hops=5
  ,csv.dir=csv.dir.test,ret.uniq.times=TRUE
  , aggreg.by.region=TRUE)


tr.igraph.list.country <- tracerouteToigraphList(csv.dir=csv.dir.test,n.hops=5,ntimebins=3,split.timespan=FALSE, vant.pt.of.interest=9857,aggreg.by.region=TRUE)

#rename Akamai edge attributes to weight 






csv.dir<-"F:/Users/Sancar/Documents/projects/XDATA/akamai_merged/test_dir"
tr.igraph.list <- tracerouteToigraphList(csv.dir=csv.dir,n.hops=5,ntimebins=36,split.timespan=FALSE, vant.pt.of.interest=4149,aggreg.by.region=FALSE)




csv.dir<-"F:/Users/Sancar/Documents/projects/XDATA/akamai_merged/test_dir2"
tr.edgelist.country <- tracerouteToEdgelist(
  vant.pt=9857,n.first.hops=6
  ,csv.dir=csv.dir,ret.uniq.times=TRUE
  , aggreg.by.region=TRUE)

tr.igraph.list <- tracerouteToigraphList(csv.dir=csv.dir,n.hops=5,ntimebins=3,split.timespan=FALSE, vant.pt.of.interest=9857,aggreg.by.region=TRUE)





######
# Plotting
load("ss.tr.RData")

akamai.tr.scan<-  data.frame(us=unlist(akamai.tr.scanstats$us)
                       ,them=unlist(akamai.tr.scanstats$them),
                             ,us.ctry=as.character(names(akamai.tr.scanstats$us))
                             ,them.ctry=as.character(names(kamai.tr.scanstats$them))                                              
)

molt = melt(akamai.tr.scan,measure.vars=c("us","them"))
molt[is.na(molt)] <- 0

tstep=rep(1:36,2)
gg.0<- ggplot(data=molt,aes(x=tstep,y=value,col=variable))+geom_line(size=1.3)+ggtitle("Avg Delay")
gg.0
ggsave(gg.0,file="IP.scanstats.pdf")
ggsave(gg.0,file="IP.scanstats.png")




akamai.tr.scan.country.avg.delay<- data.frame(us=unlist(scanstats.ak.tr.country$us)  
    ,them=unlist(scanstats.ak.tr.country$them)
,us.ctry=as.character(names(scanstats.ak.tr.country$us))
,them.ctry=as.character(names(scanstats.ak.tr.country$them))                                              
                                              )

molt.avg.delay = melt(akamai.tr.scan.country.avg.delay,measure.vars=c("us","them"))
molt.avg.delay[is.na(molt.avg.delay)] <- 0

gg.1<- ggplot(data=molt.avg.delay,aes(x=(tstep=rep(1:36,2)),y=value,col=variable))+geom_line(size=1.3)+ggtitle("Avg Delay")
gg.1
ggsave(gg.1,file="country.avg.delay.pdf")
ggsave(gg.1,file="country.avg.delay.png")

akamai.tr.scan.country.long.delay<- data.frame(
  us=unlist(scanstats.ak.tr.country.long.delay$us)
  , them=unlist(scanstats.ak.tr.country.long.delay$them)
  ,us.ctry=names(scanstats.ak.tr.country.long.delay$us),
  ,them.ctry=names(scanstats.ak.tr.country.long.delay$them)                                              )



molt.long.delay = melt(akamai.tr.scan.country.long.delay,measure.vars=c("us","them"))
molt.long.delay[is.na(molt.long.delay)] <- 0

gg.2<- ggplot(data=molt.long.delay,aes(x=(tstep),y=value,col=variable))+geom_line(size=1.2)+ggtitle("Count Long Delay") 
ylim1 = boxplot.stats(molt.long.delay$value)$stats[c(1, 5)]
#+scale_y_continuous(limits=c(-3,4))
gg.2<-gg.2+  coord_cartesian(ylim = c(-2,5))
ggsave(gg.2,file="country.long.delay.pdf")
ggsave(gg.2,file="country.long.delay.png")

