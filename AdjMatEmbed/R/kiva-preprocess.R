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