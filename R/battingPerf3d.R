##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Jun 2015
# Function: battingPerf3d
# This function creates a 3D scatter plot of Runs scored vs Balls Faced and Minutes in crease. 
# A regression plane is fitted to this.
#
###########################################################################################

battingPerf3d <- function(file, name="A Hookshot") {
    # Clean the batsman file and create a complete data frame
    batsman <- clean(file)
    # Make a 3 D plot and fit a regression plane
    atitle <- paste(name, "- Runs  vs BF & Mins")
    s <-with(data=batsman,scatterplot3d(BF,Mins,Runs, color=rgb(0,0,255,50,maxColorValue=255),
                                xlab="Balls Faced",ylab="Minutes in crease",
                                zlab="Runs scored", main=atitle,pch=16))
    
    # Fit a regression plabe
    fit <- with(data=batsman,lm(Runs ~ BF+Mins))
    
    # Draw the plane
    s$plane3d(fit)
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
   
    
}
