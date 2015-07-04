##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 30 Jun 2015
# Function: bowlerHistWickets
# This function plots the a histogram of Wicket percentages versus wickets
#
###########################################################################################
bowlerHistWickets <- function(file,name="A Googly") {
    bowler <- cleanBowlerData(file);
    
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
    #wktsDF
    atitle <- paste(name,"'s", " - Wicket percentage (%) vs wickets")
    plot(as.numeric(as.character(wktsDF$Wickets)), wktsDF$freqPercent, type="o", xlab="Wickets", ylab = "Wicket percentages (%)",
         main = atitle, ylim=c(0,50),pch=15, col="blue",lwd="2")
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}