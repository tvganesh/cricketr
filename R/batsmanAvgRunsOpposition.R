##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 10 Jul 2015
# Function: batsmanAvgRunsOpposition
# This function plots the average runs scored by batsman versus the opposition. The xlabels indicate
# the Opposition and the number of innings at ground
#
###########################################################################################

batsmanAvgRunsOpposition <- function(file, name="A Latecut"){
    
    
    batsman <- clean(file)
    
    # Use dplyr's summarise to group by Opposition and compute mean runs and count
    meanRuns <- batsman %>% group_by(Opposition) %>% summarise(m= mean(Runs))
    countInnings <- batsman %>% group_by(Opposition) %>% summarise(len=length(Runs))
    
    # Set margins
    par(mar=c(9,4,3,2))
    opposition <- as.vector(meanRuns$Opposition)
    values <- paste(opposition,"-",countInnings$len)
    atitle <- paste(name,"'s Average Runs versus Opposition")
    barplot(meanRuns$m,names=values,las=2,ylab="Average Runs", 
            col=rainbow(length(meanRuns$m)),main=atitle)
    
    mtext("Opposition - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
    mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}
