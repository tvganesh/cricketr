##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 30 Jun 2015
# Function: batsmanMeanStrikeRate
# This function plot the Mean Strike Rate of the batsman against Runs scored as a continous graph
#
###########################################################################################

batsmanMeanStrikeRate <- function(file, name="A Hitter"){

  batsman <- clean(file)
  
  # Create a vector of runs with intervals of 15
  maxi <- (max(batsman$Runs/15) + 1) *15
  v <- seq(0,maxi,by=15)
  a <- hist(batsman$Runs,breaks=v,plot=FALSE)
  
  
  # Compute the Mean Strike Rate for each run range
  SR <- NULL
  for(i in 2:length(a$breaks))  {
    b <- batsman$Runs > a$breaks[i-1] & batsman$Runs <= a$breaks[i] 
    c <- batsman[b,]
    SR[i-1] <- mean(as.numeric(as.character(c$SR)))
  }
  
  # Find all intervals where there is no data i.e. NA
  b <- !is.na(SR)
  
  #Subset and remove the NAs for counts
  c <- a$mid[b]
  
  #Subset and remove the NAs for Strike Rate
  SR <- SR[b]
  

  
  par(mar=c(4,4,2,2))
  atitle <- paste(name,"'s Mean Strike Rate versus Runs")
  plot(c,SR,pch=16,xlab="Runs",ylab="Mean Strike Rate",ylim=c(0,90), main=atitle)
  lines(c,predict(loess(SR~c)),col="blue",lwd=3)
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=2, adj=1.0, cex=0.8, col="blue")
  
}