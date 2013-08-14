#' loads the lender-lender links from file returns the edgelist as a ffdf object
#' @param kiva.file.loc location  of the Kiva data file in the file system  that stores lender-lender links 
#' @return a ffdf object of edgelists where the vertex names are integers
#' @export
load.kiva.lender.lender  <- function(kiva.file.loc,lender_lender_fname="lender_lender_loan.tsv"){
  
  kiva.lender_lender.file <- paste(kiva.file.loc,"/" ,lender_lender_fname,sep="")
  kiva.lender_lender.edgelist<-read.delim.ffdf(file=kiva.lender_lender.file
                                               ,header=FALSE, quote="", sep="\t", na.strings=c(""), fill=TRUE,
                                               colClasses = c("integer"
                                                              ,"integer")
  )
  return(kiva.lender_lender.edgelist)                                    
}

#' loads the lender-partner links from file returns the edgelist as a ffdf object
#' @param kiva.file.loc location  of the Kiva data file in the file system  that stores lender-lender links 
#' @return a ffdf object of edgelists where the vertex names are integers
#' @export
load.kiva.lender.partner <- function(kiva.file.loc,lender_partner_fname="/lender_partner_by_loan.tsv"){
  kiva.lender_partner.file <- file.path(kiva.file.loc
                                        ,lender_partner_fname)
  
  kiva.lender_partner.edgelist<-read.delim.ffdf(file=kiva.lender_partner.file
                                                , header=FALSE, quote="", sep="\t"
                                                , na.strings=c(""), fill=TRUE,
                                                colClasses = c("integer"
                                                               ,"integer")
  )
  return(kiva.lender_partner.edgelist)                                    
}



load.kiva.all.table <- function(kiva.file.loc
                                ,all.table.fname=                                  "/loan_lender_borrower_partner.tsv") {
  
  kiva.all_table.file <- file.path(kiva.file.loc
                                   ,"/loan_lender_borrower_partner.tsv")
  
  kiva.all_table <- read.delim.ffdf(file = kiva.all_table.file
                                    , header=FALSE, quote="", sep="\t"
                                    , na.strings=c(""), fill=TRUE,
                                    colClasses = c("integer"
                                                   ,"integer"
                                                   ,"integer"
                                                   ,"integer")
  )
  names(kiva.all_table)<- c("loan","lender","borrower","partner")
  return (kiva.all_table)                                    
}







#' Takes the teams.csv file in Kiva dataset and replaces the smaller teams by the category of team so that number of members in teams are larger
#'
pruneTeamList <- function(team.file){
  team.csv <- read.delim(file=team.file,quote="",header=FALSE)
  large.teams <-team.csv$V13 > 1000 
  small.teams <- !large.teams
  
  pruned.team.map <- cbind(team.csv$V1,as.character(team.csv$V1))
  pruned.team.map[small.teams,][,2] <- paste("cat",team.csv$V4[small.teams],sep="")
  names(pruned.team.map)<-c("team_id","cluster_id")
  return(pruned.team.map)
  
}


correctedTeamLinks <- function(team.file, teamLenderLink.file,lender_user_id_map.file){
  team.map <- pruneTeamList(team.file)
  
  teamLenderLink <- read.delim(teamLenderLink.file,quote="",sep="\t",header=FALSE)
  teamLenderLink <-teamLenderLink[,1:2]
  members.of.multiple.teams<- duplicated(teamLenderLink$V1)
  remove.lenders    <- unique(teamLenderLink$V1[members.of.multiple.teams])
  teamLenderLink <- teamLenderLink[ !(teamLenderLink$V1 %in% remove.lenders),]
  names(teamLenderLink)<-c("user_id","team_id")
  clusterLenderLink <- merge(team.map,teamLenderLink, by="team_id") 
  
  
  
  uid_embed_row_map <- read.delim(file=lender_user_id_map.file,quote="",sep="\t",header=FALSE)
  names(uid_embed_row_map) <- c("user_id","embedrow_name")
  embed_row_cluster_map <- merge(uid_embed_row_map,clusterLenderLink,by="user_id")
  embed_row_cluster_map_simple <- embed_row_cluster_map[,c("embedrow_name"
                                                           ,"cluster_id"
                                                           ,"user_id")]
  
  write.csv(file="embed_row_cluster_membership.csv", embed_row_cluster_map_simple)
  
}