##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 4 Jul 2015
# Function: bowlerMovingAverage
# This function computes and plots the Moving Average of the Wickets taken for a bowler
# across his career
#
###########################################################################################

bowlerMovingAverage <- function(file,name="A Doosra") {
    # Compute the moving average of the time series
    df <- cleanBowlerData(file) 
    
    #Subset wickets taken and career dates
    wickets <- df$Wkts
    date <- dmy(df$Start.Date)
    
    timeframe <- data.frame(wickets,date)
    
    
    atitle <- paste(name,"'s moving average (wickets) over career")
    plot(timeframe$date,timeframe$wickets,type="o",col="grey", xlab ="Year", 
             ylab = "Wickets", main=atitle)
    
    # Use loess regression to fit the moving average
    lines(timeframe$date,predict(loess(wickets~as.numeric(date),timeframe)),col="blue", lwd=2)
    
    vals <- list("Wickets taken", "Moving Average")
    legend(x="topleft", legend=vals, lty=c(1,1),   
           lwd=c(2,2),col=c("grey","blue"),bty="n",cex=0.8)
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=2, adj=1.0, cex=0.8, col="blue")
    
}
