##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 7 Jul 2015
# Function: bowlerContributionWonLost
# This plots the bowler's contribution to won and lost matches
#
###########################################################################################
bowlerContributionWonLost <- function(file,name="A Doosra") {
    
    
    playersp <- cleanBowlerData(file)
    won <- filter(playersp,result==1)
    lost <- filter(playersp,result==2 | result == 4 )
    
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
