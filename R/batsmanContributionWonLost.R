##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 29 Jun 2015
# Function: batsmanContributionWonLost
# This plots the batsman's contribution to won and lost matches
#
###########################################################################################
batsmanContributionWonLost <- function(profileNo,name="A Hitter") {
    
    # Get the data for the player in which the matches were won
    won <-getPlayerData(profile=profileNo,dir="./mytest",file="won.csv",homeOrAway=c(1,2),result=c(1))
    
    
    # Get the data for the player in which the matches were lost or drawn
    lost <-getPlayerData(profile=profileNo,dir="./mytest", file="lost.csv",homeOrAway=c(1,2),result=c(2,4))
    won <- clean("./mytest/won.csv")
    lost <- clean("./mytest/lost.csv")
    
    won$status="won"
    lost$status="lost"
    wonLost <- rbind(won,lost)
    atitle <- paste(name,"- Run contribution in games won and lost/drawn")
    
    # Create boxplots
    boxplot(Runs~status,data=wonLost,col=c("red","green"),xlab="Status of game",
            ylab="Runs scored", main=atitle)
    
    
    a <- dim(won)
    b <- dim(lost)
    
    
    val1 <- paste(b[1], "games lost/drawn")
    val2 <- paste(a[1],"games won")
    vals <- list(val1,val2)
    legend(x="top", legend=vals, lty=c(1,1),   
           lwd=c(7,7),col=c("red","green"),bty="n")
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=1, col="blue")  
    
}
