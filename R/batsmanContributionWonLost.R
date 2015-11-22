##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 21 Nov 2015
# Function: batsmanContributionWonLost
# This plots the batsman's contribution to won and lost matches
#
###########################################################################################

batsmanContributionWonLost <- function(file,name="A Hitter") {
    
    playersp <- clean(file)

    won <- filter(playersp,result==1)
    lost <- filter(playersp,result==2 | result == 4 )
    won$status="won"
    lost$status="lost"
    wonLost <- rbind(won,lost)
    atitle <- paste(name,"- Runs in games won/lost-drawn")
    
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

