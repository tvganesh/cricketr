##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 1 Jul 2015
# Function: batsmanRunsRanges
# This plots the percentage runs in different run ranges
#
###########################################################################################
batsmanRunsRanges <- function(file, name= "A Hookshot") {
    # Clean file
    df <- clean(file)
    
    # Divide the runs into 20 run ranges from 0 to 400
    f <- cut(df$Runs, breaks=seq(from=0,to=400,by=20))
    
    # Create a table
    g <- table(f)
    
    # Create a vector to store the runs frequency
    v <- as.vector(g)
    
    # Compute percentage of runs in the overall run total
    percentRuns <- (g/sum(g))*100
    runfreq <- c(name, round(percentRuns,1), "\n")
    
    # Add a title
    atitle <- paste(name,"Runs %  vs Run ranges")
    
    # Plot the batting perormance 
    barplot(percentRuns, main = atitle ,xlab="Runs scored",
            ylab="% times runs scored in range (%)",ylim=c(0,100),col="blue")
    axis(side=2, at=seq(0, 100, by=5))
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=2, adj=1.0, cex=0.8, col="blue")  
    
}