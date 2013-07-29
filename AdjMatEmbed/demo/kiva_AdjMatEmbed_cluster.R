

chunk.embeddings <- chunk(embed.coords.periph.ffdf)


for (chunk.index in chunk.embeddings)
lender.cluster.clara <- clara(as.data.frame(embed.coords.periph.ffdf[chunk.index,])
                              , k=6,samples=100)



pamk(as.data.frame(embed.coords.periph.ffdf),krange=seq(1,60,2),criterion="asw", usepam=FALSE,
     scaling=FALSE, alpha=0.001, diss=FALSE,
     critout=TRUE, ns=10, seed=NULL)