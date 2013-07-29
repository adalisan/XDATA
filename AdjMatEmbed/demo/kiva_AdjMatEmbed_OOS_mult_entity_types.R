if (!exists("kiva.lender_lender.edgelist"))
  stop("First source("kiva_AdjMatEmbed.R")")


kiva.lender_partner.edgelist <- load.kiva.bbn.lender.partner(kiva.bbn.data.loc)



names(kiva.lender_partner.edgelist)<-c("v1","v2")


Partner.Embed <- EmbedOOS (lender.embed.all, kiva.lender_partner.edgelist)





kiva.all.table <- load.kiva.bbn.all.table(kiva.bbn.data.loc) 
kiva.partner_loan.edgelist<- unique(kiva.all.table[,c("partner","loan")])
Loan.Embed<- EmbedOOS (Partner.Embed, kiva.partner_loan.edgelist)






kiva.loan_borrower.edgelist<- unique(kiva.all.table[,c("loan","borrower")])

Borrower.Embed<- EmbedOOS (Loan.Embed, kiva.loan_borrower.edgelist)



