##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 7 Aug 2015
# Function: bowlerWktRateTT
# This function plots the Frequency percentage of wickets taken for the bowler in Twenty20 Int
#
###########################################################################################
bowlerWktRateTT <- function(file, name="A Bowler") {
    bowler <- clean(file)
    wktRate <- NULL
    w <- NULL
    for (i in 0: max(as.numeric(as.character(bowler$Wkts)))) {
        
        # Create a vector of Economy rate  for number of wickets 'i'
        balls <- as.numeric(bowler[bowler$Wkts == i,]$Overs) *6
       
        if(length(balls != 0)) {
            # Compute the mean Wicket rate by using lapply on the list 
            wktRate[i] <- lapply(list(balls),mean) 
            w[i] <-i
        } 
        
    }
    # Check for NULL ( or no Wickets in the range taken)
    a <- sapply(wktRate,is.null)
    
    # Set NaN to plot a break if the bowler has not taken 'n' wickets
    wktRate[a] <- NaN
  
    atitle <- paste(name,"-No. balls vs wkts")
    
    plot(w,wktRate,type="o",pch=13,col="red",lwd=3,xlab="Wickets",ylab="Mean number of deliveries",main=atitle)
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}