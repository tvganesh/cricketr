##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 1 Jul 2015
# Function: relativeBowlingPerf
# This function computes and plots the relative bowling performance of bowlers
#
###########################################################################################

relativeBowlingPerf <- function(frames, names) {

    col1 <- rainbow(length(frames))
    for(i in 1:length(frames))
    {    
               
        #compute percentage wickets for the bowler
        
        pWkts <- percentWkts(frames[[i]])
              
        # Plot the bowling performances
        if(i == 1) {
            
            plot(pWkts$Wickets, pWkts$freqPercent, type="l", xlab="Wickets", ylab = "Wicket percentages (%)",
                 main = "Relative wickets percentage", xlim=c(1,10),ylim=c(0,50),lwd=3.0)
        }
        
        lines(pWkts$Wickets, pWkts$freqPercent,col=col1[i],lwd=2.0)
        
    }
    i <- 1
    type = rep(1,length(frames))
    width = rep(2.5,length(frames))
    legend(x="right",legend=names, lty=type,   
           lwd=width,col=col1,bty="n")
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}

percentWkts <- function(file) {
    bowler <- cleanBowlerData(file)
    wktsDF <- NULL
    # Create a table of wickets
    wktsTable <- table(bowler$Wkts)
    
    #Convert to dataframe for easy processing
    wktsDF <- as.data.frame(wktsTable)
    
    #Remove column with "-"
    wktsDF <- wktsDF[2:nrow(wktsDF),]
   
    
    #Rename columns
    colnames(wktsDF) <- c("Wickets","Freq")
  
    #Calculate wickets percentage
    wktsDF$freqPercent <- (wktsDF$Freq/sum(wktsDF$Freq)) * 100
    
    wktsDF$Wickets <- as.numeric(as.character(wktsDF$Wickets))
    # Ensure ascending rrder of wickets
    wktsDF <- wktsDF[order(as.numeric(as.character(wktsDF$Wickets))),]
    
    wktsDF
    
    
}
