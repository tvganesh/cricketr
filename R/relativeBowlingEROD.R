##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 31 Jul 2015
# Function: relativeBowlingER
# This function computes and plots the mean relative bowling Economy Rate of the bowlers
#
###########################################################################################


relativeBowlingEROD <- function(frames, names) {
    
    col1 = c("red","blue","cyan","black","brown")
   
    for(i in 1:length(frames))
    {   
        # Clean the bowler data frame
        bowler <- cleanBowlerData(frames[[i]])       
        
        # Get the max wickets taken by bowler
        wkts <- c(0:max(bowler$Wkts))
        
        #compute mean economy rate  for the bowler
        eRate <- ER(frames[[i]])
       
        # Plot the Economy Rate vs Wickets
        if(i == 1) {
            
            plot(wkts,eRate,type="o",pch=13,col=col1[i],lwd=3,
                 xlab="wickets",ylab="Economy rate",ylim=c(2,7), main="Relative economy rate")
        }
        lines(wkts,eRate,col=col1[i],lwd=3.0)
    }
    #i <- 1
    type = rep(1,length(frames))
    width = rep(2.5,length(frames))
    legend(x="topright",legend=names, lty=type,   
           lwd=width,col=col1,bty="n",cex=0.8)
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
    
}

ER <- function(file){
    
    # Clean the bowler data before processing
    bowler <- cleanBowlerData(file)
    
    econRate <- NULL
    # Calculate mean economy rate versus number of wickets taken. Loop for 0 to max wickets
    for (i in 0: max(as.numeric(as.character(bowler$Wkts)))) {
        
        # Create a vector of Economy rate  for number of wickets 'i'
        a <- bowler[bowler$Wkts == i,]$Econ
        b <- as.numeric(as.character(a))
        
        # Compute the mean economy rate by using lapply on the list 
        econRate[i+1] <- lapply(list(b),mean)
                        
    }
    econRate
}