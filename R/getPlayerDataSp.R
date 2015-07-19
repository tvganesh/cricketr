##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 19 Jul 2015
# Function: getPlayerDataSp
# This function is a specialized version of getPlayer Data. This function gets the players data 
# along with details on matches' venue( home/abroad) and the result (won,lost,drawn) as 
# 2 separate columns
#
###########################################################################################
getPlayerDataSp <- function(profileNo,tdir="./data",tfile="player001.csv",ttype="batting"){
    
    # Get the data for the player i
    # Home & won
    hw <-getPlayerData(profile=profileNo,dir=tdir,file=tfile,homeOrAway=c(1),result=c(1),type=ttype)
    # Home & lost
    hl <-getPlayerData(profile=profileNo,dir=tdir,file=tfile,homeOrAway=c(1),result=c(2),type=ttype)
    # Home & drawn
    hd <-getPlayerData(profile=profileNo,dir=tdir,file=tfile,homeOrAway=c(1),result=c(4),type=ttype)
    # Away and won
    aw <-getPlayerData(profile=profileNo,dir=tdir,file=tfile,homeOrAway=c(2),result=c(1),type=ttype)
    #Away and lost
    al <-getPlayerData(profile=profileNo,dir=tdir,file=tfile,homeOrAway=c(2),result=c(2),type=ttype)
    # Away and drawn
    ad <-getPlayerData(profile=profileNo,dir=tdir,file=tfile,homeOrAway=c(2),result=c(4),type=ttype)
    
    # Set the values as follows
    # ha := home = 1, away =2
    # result= won = 1, lost = 2, drawn=4
    hw$ha<-1
    hw$result<-1
    
    hl$ha<-1
    hl$result<-2
    
    hd$ha<-1
    hd$result<-4
    
    aw$ha<-2
    aw$result<-1
    
    al$ha<-2
    al$result<-2
    
    ad$ha<-2
    ad$result<-4
    
    a <-rbind(hw,hl,hd,aw,al,ad)
    
    afile <-paste(tdir,tfile,sep="/")
    write.csv(a,file=afile)
    a
   
    
}
