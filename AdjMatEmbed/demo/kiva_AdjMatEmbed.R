
run.in.linux <- (.Platform$OS.type == "unix") 
debug.mode <- TRUE  
  
#kiva.data.loc <-   system.file("inst","extdata", package="AdjMatEmbed")
  kiva.data.loc <-   system.file("extdata", package="AdjMatEmbed")
print(kiva.data.loc)


graphEmbedCluster <- function (kiva.lender_lender.edgelist,embed.dim=10
                               ,core.v.chunk.size=2000, v.chunk.size = core.v.chunk.size, save.ffdata=TRUE ) {
  
 
  
  
  V.Ecount.ffdf <- OrderVByEcount(kiva.lender_lender.edgelist)
  
  
  v.chunk.size <- min(dim(V.Ecount.ffdf)[1]- core.v.chunk.size,v.chunk.size)
  
  core.v.chunk <-  V.Ecount.ffdf[[1]][1:core.v.chunk.size]
  
  lender.embed.core<- EmbedGraphCore(V.Ecount.ffdf
                                     , kiva.lender_lender.edgelist
                                     , core.v.chunk
                                     , v.chunk.size=core.v.chunk.size
                                     , embed.dim=embed.dim)
  
  
  
 # mclust.model<-Mclust(lender.embed.core)
  
#  print(mclust.model)
  
  #plot(mclust.model)
  #converting to ff vector with the right dimension ordering (2,1)
  lender.embed.core.ff <-as.ffdf( as.ff(lender.embed.core,
                                        dim=c(core.v.chunk.size,embed.dim),dimorder=c(2,1)))
  
  rownames(lender.embed.core.ff) <- (core.v.chunk)
  
  colnames(lender.embed.core.ff)<- c(paste("x.",1:embed.dim,sep=""))
  
  
  
  
  ff_periph_v_vector_index<- ffwhich(V.Ecount.ffdf
                                     ,!( key %in% core.v.chunk))
  
  #subset for debugging purposes
  # ff_periph_v_vector_index <-ff_periph_v_vector_index[1:4000]
  
  ff_periph_v_vector <- as.ff( V.Ecount.ffdf[ff_periph_v_vector_index,][,1])
  
  
  embed.coords.periph <- NULL
  
  num.periph.vec <- length(ff_periph_v_vector)
  chunk.ri <- NULL
  if (v.chunk.size < num.periph.vec )
  chunk.ri<- chunk(ff_periph_v_vector,by=v.chunk.size)
  else{
    chunk.ri<- chunk(ff_periph_v_vector)
  }
  for (i in chunk.ri){
  
    chunk.i <- ff_periph_v_vector [i]
    new.embed.coords <- Embed.OOS.chunk (chunk.i,
                                         kiva.lender_lender.edgelist,
                                         core.v.chunk, lender.embed.core,scaling=TRUE )
    
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
  
  
  if (save.ffdata) ffsave.image("lender.embed.ff")
  
  
  
  write.csv.ffdf(lender.embed.core.ff,file="kiva.embed.lenders.csv")
  write.csv.ffdf(embed.coords.periph.ffdf,file="kiva.embed.lenders.csv"
                 ,append=TRUE
  )
  return (list(core=lender.embed.core.ff,periph=embed.coords.periph.ffdf))
}


  
subsample.data<- TRUE
if (subsample.data){
  kiva.lender_lender.edgelist <- load.kiva.lender.lender ( kiva.data.loc)
} else {
  kiva.lender_lender.edgelist <- load.kiva.lender.lender ( kiva.data.loc)
}


names(kiva.lender_lender.edgelist)<-c("v1","v2")
#count the number of edges incident to each vertex


# lender.embed.dim.3<- graphEmbedCluster (kiva.lender_lender.edgelist
#                                                     ,embed.dim=3 , v.chunk.size = 3000)
lender.embed.dim.10<- graphEmbedCluster  (kiva.lender_lender.edgelist
                                                     , embed.dim=10
                                          , core.v.chunk.size = 2000
                                          ,v.chunk.size=Inf)  
  
#   lender.embed.dim.35<- graphEmbedCluster (kiva.lender_lender.edgelist
#                                                        ,embed.dim = 35
#                                                        , v.chunk.size = 3000)  
    

lender.embed.core<- cbind(data.frame(v.name=rownames(lender.embed.dim.10$core)),as.data.frame(lender.embed.dim.10$core))




lender.embed.all<- ffdfappend(lender.embed.dim.10$periph,lender.embed.core)

ord <- fforder(lender.embed.all$v.name, decreasing=FALSE)
lender.embed.all <- lender.embed.all[ord,]
rownames(lender.embed.all ) <- lender.embed.all$v.name
  
  
  
  



