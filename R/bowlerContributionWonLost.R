##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 7 Jul 2015
# Function: bowlerContributionWonLost
# This plots the bowler's contribution to won and lost matches
#
###########################################################################################
bowlerContributionWonLost <- function(profileNo,name="A Doosra") {
    
    # Get the data for the player in which the matches were won
    won <-getPlayerData(profile=profileNo,dir=".",file="won.csv",homeOrAway=c(1,2),
                        result=c(1),type="bowling")
    
    
    # Get the data for the player in which the matches were lost or drawn
    lost <-getPlayerData(profile=profileNo,dir=".", file="lost.csv",homeOrAway=c(1,2),
                         result=c(2,4),type="bowling")
    won <- cleanBowlerData("./won.csv")
    lost <- cleanBowlerData("./lost.csv")
    
    won$status="won"
    lost$status="lost"
    wonLost <- rbind(won,lost)
    atitle <- paste(name,"- Wickets in games won/lost-drawn")
    
    # Create boxplots
    boxplot(Wkts~status,data=wonLost,col=c("red","green"),xlab="Match outcome",
            ylab="Wickets", main=atitle)
    
    
    
    a <- dim(won)
    b <- dim(lost)
    
    
    val1 <- paste(b[1], "games lost/drawn")
    val2 <- paste(a[1],"games won")
    vals <- list(val1,val2)
    legend(x="top", legend=vals, lty=c(1,1),   
           lwd=c(7,7),col=c("red","green"),bty="n")
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=1, col="blue")  
    
}
