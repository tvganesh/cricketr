##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 29 Jun 2015
# Function: bowlerPerfHomeAway
# This plots the bowler's performance home and abroad
#
###########################################################################################
bowlerPerfHomeAway <- function(file,name="A Googly") {
    
    ha <- NULL
    playersp <- cleanBowlerData(file)
    home <- filter(playersp,ha==1)
    away <- filter(playersp,ha==2)
    
    home$venue="Home"
    away$venue="Overseas"
    homeAway <- rbind(home,away)
    atitle <- paste(name,"- Wickets-Home & overseas")
    
    # Create boxplots
    boxplot(Wkts~venue,data=homeAway,col=c("blue","green"),xlab="Match venue",
            ylab="Wickets", main=atitle)
    
    
    a <- dim(home)
    b <- dim(away)
    
    
 
    val1 <- paste(a[1],"Home venue")
    val2 <- paste(b[1], "Overseas")
    vals <- list(val1,val2)
    legend(x="top", legend=vals, lty=c(1,1),   
           lwd=c(7,7),col=c("blue","green"),bty="n")
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=1, col="blue")  
    
}
