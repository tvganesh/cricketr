##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 10 Jul 2015
# Function: bowlerAvgWktsOpposition
# This function plots the average runs scored by batsman at the ground. The xlabels indicate
# the number of innings at ground
#
###########################################################################################

bowlerAvgWktsOpposition <- function(file, name="A Chinaman"){
    
    
    bowler <- cleanBowlerData(file)
    
    # Use dplyr's summarise to group by opposition and compute mean and count
    meanWkts <- bowler %>% group_by(Opposition) %>% summarise(m= mean(Wkts))
    countInnings <- bowler %>% group_by(Opposition) %>% summarise(len=length(Wkts))
    
    # Set margins
    par(mar=c(9,4,3,2))
    opposition <- as.vector(meanWkts$Opposition)
    values <- paste(opposition,"-",countInnings$len)
    atitle <- paste(name,"'s Average Wickets versus Opposition")
    barplot(meanWkts$m,names=values,las=2,ylab="Average Wickets", 
            col=rainbow(length(meanWkts$m)),main=atitle)
    abline(h=3,lty=3,lwd=2)
    
    mtext("Opposition - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
    mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}
