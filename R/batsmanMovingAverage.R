##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 30 Jun 2015
# Function: batsmanMovingAverage
# This function computes and plots the Moving Average of the batsman across his career
#
###########################################################################################

batsmanMovingAverage <- function(file,name="A Squarecut") {
    # Compute the moving average of the time series
    df <- clean(file) 
    
    #Subset runs and career dates
    runs <- df$Runs
    date <- dmy(df$Start.Date)
    
    timeframe <- data.frame(runs,date)
    
    
    atitle <- paste(name,"'s moving average (runs) over career")
    plot(timeframe$date,timeframe$runs,type="o",col="grey", xlab ="Year", ylab = "Runs", main=atitle)
    
    # Use loess regression to fit the moving average
    lines(timeframe$date,predict(loess(runs~as.numeric(date),timeframe)),col="blue", lwd=2)
    
    vals <- list("Runs scored", "Moving Average")
    legend(x="topleft", legend=vals, lty=c(1,1),   
           lwd=c(2,2),col=c("grey","blue"),bty="n",cex=0.8)
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
    
}
