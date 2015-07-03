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
    
    
    # Create a ggplot
    atitle <- paste(name,"'s", " batting performance")
    g <- qplot(df1$Runs,df1$RunFrequency, data=df1,geom=c("point","smooth"),method="loess",
               xlab="Runs",ylab="Run Frequency")
    p <-g + ggtitle(atitle)
    print(p)
    
}