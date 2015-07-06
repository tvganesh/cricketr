##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 30 Jun 2015
# Function: batsmanRunsFreqPerf
# This function computes and plots the Moving Average of the batsman across his career
#
###########################################################################################
# Plot the performance of the batsman as a continous graph
# Create a performance plot between Runs and RunsFrequency 
batsmanRunsFreqPerf <- function(file, name="A Hookshot") {
    
    df <- clean(file)
    
    # Create breaks in intervals of 10
    maxi <- (max(df$Runs/10) + 1) *10
    v <- seq(0,maxi,by=10)
    a <- hist(df$Runs,breaks=v,plot=FALSE)
    
    # Create mid points
    Runs <- a$mids
    RunFrequency <- a$counts
    df1 <- data.frame(Runs,RunFrequency)
    
    
    # Create a plot
    atitle <- paste(name,"'s", " Runs frequency vs Runs")
    plot(df1$Runs,df1$RunFrequency,pch=16,xlab="Runs",ylab="Runs Frequency", main=atitle)
    lines(df1$Runs,predict(loess(df1$RunFrequency~df1$Runs)),col="blue",lwd=3)
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=2, adj=1.0, cex=0.8, col="blue")
    
    
}