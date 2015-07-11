##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Jun 2015
# Function: batsman4s
# This function plots the number of 4s vs the runs scored in the innings by the batsman
#
###########################################################################################
batsman4s <- function(file, name="A Hookshot") {
    
    # Clean the batsman file and create a complete data frame
    df <- clean (file)
    
    # Get numnber of 4s and runs scored
    x4s <- as.numeric(as.vector(df$X4s))
    runs <- as.numeric(df$Runs) 
    
    # Set margins
    par(mar=c(4,4,2,2))
    
    atitle = paste(name,"-","Runs scored vs No of 4s" )
    
    # Plot no of 4s and a 2nd order curve fit   
    plot(runs,x4s, xlab = "Runs", ylab = "Number of 4's", main = atitle,
         pch=20,col=adjustcolor("red",alpha=0.5))
    
    # Second order polynomial used
    fit2 <- lm(x4s~poly(runs,2,raw=TRUE))
    
    xx <- seq(from=0,to = max(runs),by=20)
    yy <- NULL
    for (i in seq_along(xx)) {
        yy[i] <- fit2$coefficients[3] * xx[i]^2 + fit2$coefficients[2] * xx[i] + fit2$coefficients[1] 
        
    }
    lines(xx,yy,col="blue",lwd=2.0)
    # Plot predicted 4s in 50 runs
    a <- predict(fit2,data.frame(runs=50))
    abline(v=50,lty=3)
    abline(h=a,lty=3)
    
    # Plot predicted 4s in 100 runs
    a <- predict(fit2,data.frame(runs=100))
    abline(v=100,lty=4)
    abline(h=a,lty=4)
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
    
    
}
