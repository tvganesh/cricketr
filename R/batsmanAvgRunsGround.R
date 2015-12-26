##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 10 Jul 2015
# Function: batsmanAvgRunsGround
# This function plots the average runs scored by batsman at the ground. The xlabels indicate
# the number of innings at ground
#
###########################################################################################

batsmanAvgRunsGround <- function(file, name="A Latecut"){
    
    batsman <-Ground <- Runs <- NULL
    batsman <- clean(file)
    
    # use dplyr's summarise function to group by Ground and calculate mean & count
    meanRuns <- batsman %>% group_by(Ground) %>% summarise(m= mean(Runs))
    countInnings <- batsman %>% group_by(Ground) %>% summarise(len=length(Runs))
    
    # Set the margins
    par(mar=c(9,4,3,2))
    ground <- as.vector(meanRuns$Ground)
    values <- paste(ground,"-",countInnings$len)
    atitle <- paste(name,"'s Average Runs at Ground")
    barplot(meanRuns$m,names=values,las=2,ylab="Average Runs", 
            col=rainbow(length(meanRuns$m)),main=atitle,cex.names=0.8)
    abline(h=50,lty=3,lwd=2)
    abline(h=100,lty=3,lwd=2,col="blue")
    
    
    mtext("Ground - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
    mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}
