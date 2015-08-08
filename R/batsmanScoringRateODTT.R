##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 8 Aug 2015
# Function: batsmanScoringRateODTT
# This function computes and plots the batsman scoring rate of a One Day batsman
# or a Twenty20 batsman
#
###########################################################################################

batsmanScoringRateODTT <- function(file, name="A Hookshot") {
    
    # Clean the batsman file and create a complete data frame
    batsman <- clean(file)
    
    atitle <- paste(name, "- Runs vs Balls Faced")
    
    # Make a scatter plot of balls faced and runs
    with(data=batsman,plot(BF,Runs,main=atitle))
    
    # Fit a second order polynomial used
    fit2 <- with(data=batsman,lm(Runs~poly(BF,2,raw=TRUE)))
    
    # Create seq from 0 to max batsman balls faced
    xx <- seq(from=0,to = max(batsman$BF),by=5)
    
    # Compute the predicted runs
    yy <- NULL
    for (i in seq_along(xx)) {
        yy[i] <- fit2$coefficients[3] * xx[i]^2 + fit2$coefficients[2] * xx[i] + fit2$coefficients[1] 
        
    }
    # Draw the predicted runs
    lines(xx,yy,col="blue",lwd=2.0)
    bf=50
    runs = fit2$coefficients[3] * bf^2 + fit2$coefficients[2] * bf + fit2$coefficients[1] 
    abline(v=50,lty=2,lwd=2,col="blue")
    abline(h=runs,lty=2,lwd=2,col="blue")
   
    bf=100
    runs = fit2$coefficients[3] * bf^2 + fit2$coefficients[2] * bf + fit2$coefficients[1] 
    abline(v=100,lty=3,lwd=2,col="red")
    abline(h=runs,lty=3,lwd=2,col="red")
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
    
    
}