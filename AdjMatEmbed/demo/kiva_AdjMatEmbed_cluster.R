
lender.embed.core<- cbind(data.frame(v.name=rownames(lender.embed.dim.10$core)),as.data.frame(lender.embed.dim.10$core))




lender.embed.all<- ffdfappend(lender.embed.dim.10$periph,lender.embed.core)

ord <- fforder(lender.embed.all$v.name, decreasing=FALSE)
lender.embed.all <- lender.embed.all[ord,]
rownames(lender.embed.all ) <- lender.embed.all$v.name


lender.cluster.clara <- clara(as.data.frame(lender.embed.all[,-1])
                              , k=3,samples=100)


pamk(as.data.frame(lender.embed.all[,-1])
     ,krange=1:10,criterion="asw", usepam=FALSE,
     scaling=FALSE, alpha=0.001, diss=FALSE,
     critout=TRUE, ns=10, seed=NULL)


lender.cluster.clara.2 <- clara(as.data.frame(lender.embed.all[,-1])
                                , k=2,samples=100)

lender.cluster.clara.3 <- clara(as.data.frame(lender.embed.all[,-1])
                              , k=3,samples=100)


lender.embed.pca<- princomp(complender.cluster.clara.2$data)

lender.embed.pca <- princomp(as.data.frame(lender.embed.all[,-1])
                             
                             
lender.cluster.clara.k.2.dim.4 <- clara(lender.embed.pca[,1:4] , k=2,samples=10)
lender.cluster.clara.k.3.dim.4 <- clara(lender.embed.pca[,1:4] , k=3,samples=10)
                             
                             p2 <- ggplot() + geom_point(data=DataF.2,aes(x=PC1,y=PC2,colour=Cluster))
                             ggsave(p2,filename="lender.embed.cluster.last.pca.k.2.final.pdf")
                             