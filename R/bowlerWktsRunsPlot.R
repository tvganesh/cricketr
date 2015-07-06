##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 Jul 2015
# Function: bowlerWktsRunsPlot
# This function makes boxplot of Wickets versus Runs concded
###########################################################################################
bowlerWktsRunsPlot <- function(file, name="A Googly") {
    b <- cleanBowlerData(file)
    
    # Create a color palette of 10 colors
    p1 <-colorRampPalette(c("red","blue"))
    palette <- p1(10)
    
    # Create boxplots
    str <- paste(name,"- Wkts vs Runs conceded")
    boxplot(Runs~Wkts,data=b, xlab="Wickets",ylab="Run conceded",main=str,
            col=as.vector(palette))
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}