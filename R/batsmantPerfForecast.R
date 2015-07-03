##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 30 Jun 2015
# Function: batsmanPerfForecast
# This function forecasts the batsmans performance based on past performance
#
###########################################################################################
# Plot the batting performance as a combined box plot and histogram
batsmanPerfForecast <- function(file, name="A Squarecut") {
    #a <- read.csv("tendulkar.csv",na.strings=c(NA,"-"),stringsAsFactors=FALSE)
    b <- clean(file)
    
    # Read day, month and year
    date <- dmy(b$Start.Date)
    runs <- b$Runs
 
    # Create a training and a test set
    # Subset 90 percent of the rows of the time series
    rows <- length(runs)
    i <- floor(0.9 * rows)
    
    # Note the start/end month and year
    startMonth = month(date[1])
    startYear = year(date[1])
    endMonth = month(date[i])
    endYear = year(date[i])
    
    # Create training set with the 90 percent career 
    ts.train <- ts(runs, start = c(startYear,startMonth), end = c(endYear,endMonth),frequency=12)
    
    
    # Make a test set with the remaining 10 percent
    startMonth1 <- month(date[i+1])
    startYear1 = year(date[i+1])
    endMonth1 = month(date[rows])
    endYear1 = year(date[rows])
  
    ts.test <- ts(runs, start = c(startYear1,startMonth1), end = c(endYear1,endMonth1),frequency=12)
   
    # Fit a Holt Winters Model with the training set
    fit <-HoltWinters(ts.train)
    
    # Forecast based on the model
    fcast <- forecast(fit)
    atitle = paste(name,"-","Runs forecast" )
    plot(fcast,main=atitle,col="blue",lwd=1.5,xlab="Year",ylab="Runs scored")
    lines(ts.train,col="magenta")
    
    # Draw the test set
    lines(ts.test,col="red",lwd=1.5)
    
    vals <- c("forecasted runs","actual runs scored")
    col1 <- c("blue","red")
    legend(x="topleft",vals, lty=c(1,1),   
           lwd=c(1.5,1.5),col=col1,bty="n",cex=0.8)
    accuracy(fcast,ts.test)
    
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}