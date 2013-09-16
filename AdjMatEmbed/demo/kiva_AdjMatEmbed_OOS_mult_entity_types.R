if (!exists("kiva.lender_lender.edgelist")){
  kiva.adjmat.embed.demo.Rfile <-   system.file("demo","kiva_AdjMatEmbed.R", package="AdjMatEmbed")
  source(kiva.adjmat.embed.demo.Rfile)
}
  

kiva.data.loc <-   system.file("extdata", package="AdjMatEmbed")


kiva.lender_partner.edgelist <- load.kiva.lender.partner(kiva.data.loc)


names(kiva.lender_partner.edgelist)<-c("lender","partner")

embed.dim<-10
lender.embed.all<- lender.embed.all[,-which(colnames(lender.embed.all)=="v.name")]
lender.embed.all <- as.matrix(lender.embed.all)

Partner.Embed <- EmbedOOS (lender.embed.all, kiva.lender_partner.edgelist)


partner.embed.pca <- prcomp(Partner.Embed$X.oos)
DataF.2<- data.frame(PC1=partner.embed.pca[,1] 
                    , PC2=partner.embed.pca[,2] 
                     , PC3=partner.embed.pca[,3] )
p2 <- ggplot() + geom_point(data=DataF.2,aes(x=PC1,y=PC2))
ggsave(p2,filename="lender.partner.embed.pdf")
                             
                             


kiva.all.table <- load.kiva.all.table(kiva.data.loc)

kiva.partner_loan.edgelist<- unique(kiva.all.table[,c("partner","loan")])
Loan.Embed<- EmbedOOS (Partner.Embed, kiva.partner_loan.edgelist)






kiva.loan_borrower.edgelist<- unique(kiva.all.table[,c("loan","borrower")])

Borrower.Embed<- EmbedOOS (Loan.Embed, kiva.loan_borrower.edgelist)


