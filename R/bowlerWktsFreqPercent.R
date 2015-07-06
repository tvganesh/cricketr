##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Jun 2015
# Function: bowlerWktsFreqPercent
# This function plots the Frequency percentage of wickets taken for the bowler
#
###########################################################################################
bowlerWktsFreqPercent <- function(file, name="A Bowler") {
    bowler <- clean(file)
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
    
    # Ensure ascending order of wickets
    wktsDF <- wktsDF[order(as.numeric(as.character(wktsDF$Wickets))),]
    
    
    atitle <- paste(name,"'s", "Wkts freq (%) vs Wkts")
    # Set margin
    par(mar=c(5.1,4.1,4.1,2.1))
    plot(as.numeric(as.character(wktsDF$Wickets)), wktsDF$freqPercent, type="o",
         xlab="Wickets", ylab = "Wicket Freq Percentages (%)",
         main = atitle, ylim=c(0,50),pch=15, col="blue",lwd="3")
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}