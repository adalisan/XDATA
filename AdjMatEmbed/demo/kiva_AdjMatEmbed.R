require(igraph)
require(ff)
require(ffbase)
require(plyr)
require(mclust)
require(bit)

if(!exists("kiva.bbn.data.loc")) {
  print("please define kiva.bbn.data.loc , assuming kiva data lies in ")
  kiva.bbn.data.loc<-(file.path(Sys.getenv("PROJECT_DIR"),"XDATA","jhuxdata","data","kiva","kiva_bbn"))
  print(kiva.bbn.data.loc)
}

kiva.lender_lender.edgelist <- load.kiva.bbn(kiva.bbn.data.loc)


names(kiva.lender_lender.edgelist)<-c("v1","v2")
#count the number of edges incident to each vertex

embed.dim <-10


V.Ecount.ffdf<-OrderVByEcount(kiva.lender_lender.edgelist)


v.chunk.size<-2000

core.v.chunk <-  V.Ecount.ffdf[[1]][1:v.chunk.size]

lender.embed.core<- EmbedGraphCore(V.Ecount.ffdf
                                   , kiva.lender_lender.edgelist
                                   , core.v.chunk
                                   , v.chunk.size=v.chunk.size
                                   , embed.dim=embed.dim)



mclust.model<-Mclust(lender.embed.core)

print(mclust.model)

#plot(mclust.model)
#converting to ff vector with the right dimension ordering (2,1)
lender.embed.core.ff <-as.ffdf( as.ff(lender.embed.core,
                                      dim=c(v.chunk.size,embed.dim),dimorder=c(2,1)))

rownames(lender.embed.core.ff) <- (core.v.chunk)

colnames(lender.embed.core.ff)<- c(paste("x.",1:embed.dim,sep=""))




ff_periph_v_vector_index<- ffwhich(V.Ecount.ffdf
                                   ,!( key %in% core.v.chunk))

#subset for debugging purposes
# ff_periph_v_vector_index <-ff_periph_v_vector_index[1:4000]

ff_periph_v_vector <- as.ff( V.Ecount.ffdf[ff_periph_v_vector_index,][,1])


embed.coords.periph <- NULL

num.periph.vec <- length(ff_periph_v_vector)

chunk.ri<- chunk(ff_periph_v_vector,by=2000)
for (i in chunk.ri){

  chunk.i <- ff_periph_v_vector [i]
  new.embed.coords <- Embed.OOS.chunk (chunk.i,
                                       kiva.lender_lender.edgelist,
                                       core.v.chunk, lender.embed.core )
  
  chunk.i.chr<- as.character(chunk.i)
  rownames(new.embed.coords) <- chunk.i.chr
  embed.coords.periph <- ffappend(embed.coords.periph
                                  ,as.vector(new.embed.coords))
}


dim(embed.coords.periph) <- c(num.periph.vec,embed.dim)

#dimorder ()<- c(2,1)  is necessary, because
#the matrix is stored in row-major order as opposed to the standard R
# assumption of column-major order. 

dimorder(embed.coords.periph) <- c(2,1)
#rownames(embed.coords.periph) <- (ff_periph_v_vector)

#mclust.model<- Mclust(embed.coords.periph)



embed.coords.periph.ffdf <- ffdf( v.name=ff_periph_v_vector ,x=embed.coords.periph)


ffsave.image("lender.embed.ff")



write.csv.ffdf(lender.embed.core.ff,file="kiva.embed.lenders.csv")
write.csv.ffdf(embed.coords.periph.ffdf,file="kiva.embed.lenders.csv"
               ,append=TRUE
)



