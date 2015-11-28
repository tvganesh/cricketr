##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 7 Aug 2015
# Function: relativeWktRateTT
# This function computes and plots the relative bowling Economy Rate of the bowlers
#
###########################################################################################


relativeWktRateTT <- function(frames, names) {
    
    col1 <- rainbow(length(frames))
    for(i in 1:length(frames))
    {   
        # Clean the bowler data frame
        bowler <- cleanBowlerData(frames[[i]])       
        
        # Get the max wickets taken by bowler
        wkts <- c(0:max(bowler$Wkts))
        wkts <- wkts[wkts != 0]
        
        #compute mean economy rate  for the bowler
        wktRate <- WR(frames[[i]])
        
        # Check for NULL ( or no Wickets in the range taken)
        a <- sapply(wktRate,is.null)
        wktRate <- wktRate[!a]
        wkts <- wkts[!a]
        
        # Plot the Economy Rate vs Wickets
        if(i == 1) {
            
            plot(wkts,wktRate,type="o",pch=13,col=col1[i],lwd=3,xlim=c(1,7),ylim=c(18,25),
                 xlab="wickets",ylab="Mean number of deliveries",main="Relative Wicket Rate vs Deliveries")
        }
        
        lines(wkts,wktRate,col=col1[i],lwd=3.0)
        points(wkts,wktRate,pch=13)
    }
    #i <- 1
    type = rep(1,length(frames))
    width = rep(2.5,length(frames))
    legend(x="topright",legend=names, lty=type,  
           lwd=width,col=col1,bty="n",cex=0.8)
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
    
}

WR <- function(file){
    
    bowler <- clean(file)
    wktRate <- NULL
    w <- NULL
    for (i in 0: max(as.numeric(as.character(bowler$Wkts)))) {
        
        # Create a vector of Economy rate  for number of wickets 'i'
        balls <- bowler[bowler$Wkts == i,]$Overs *6
        
        if(length(balls != 0)) {
            # Compute the mean Wicket rate by using lapply on the list 
            wktRate[i] <- lapply(list(balls),mean) 
            w[i] <-i
        } 
        
    }
    #w <- w[!is.na(w)]
    # Check for NULL ( or no Wickets in the range taken)
    a <- sapply(wktRate,is.null)
    
    # Set NaN to plot a break where the 
    wktRate[a] <- NaN
    wktRate
}