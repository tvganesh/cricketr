##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 10 Jul 2015
# Function: bowlerAvgWktsGround
# This function plots the average runs scored by batsman at the ground. The xlabels indicate
# the number of innings at ground
#
###########################################################################################

bowlerAvgWktsGround <- function(file, name="A Chinaman"){
    
    Ground <- Wkts <- NULL
    bowler <- cleanBowlerData(file)
    
    # use dplyr's summarise to group by ground and compute mean & count
    meanWkts <- bowler %>% group_by(Ground) %>% summarise(m= mean(Wkts))
    countInnings <- bowler %>% group_by(Ground) %>% summarise(len=length(Wkts))
    
    # Set margins
    par(mar=c(9,4,3,2))
    ground <- as.vector(meanWkts$Ground)
    values <- paste(ground,"-",countInnings$len)
    atitle <- paste(name,"'s Average Wickets at Ground")
    barplot(meanWkts$m,names=values,las=2,ylab="Average Wickets", 
            col=rainbow(length(meanWkts$m)),main=atitle,cex.names=0.8)
    abline(h=4,lty=3,lwd=2)
    mtext("Ground - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
    mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}
