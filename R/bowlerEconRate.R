##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Jun 2015
# Function: bowlerEconRate
# This function plots the Frequency percentage of wickets taken for the bowler
#
###########################################################################################
bowlerEconRate <- function(file, name="A Bowler") {
    bowler <- clean(file)
 
    econRate <- NULL
    for (i in 0: max(as.numeric(as.character(bowler$Wkts)))) {
        
        # Create a vector of Economy rate  for number of wickets 'i'
        a <- bowler[bowler$Wkts == i,]$Econ
        b <- as.numeric(as.character(a))
        
        # Compute the mean economy rate by using lapply on the list 
        econRate[i+1] <- lapply(list(b),mean)  
        
    }
    
    
    
    wkts <- c(0:max(as.numeric(as.character(bowler$Wkts))))
    atitle <- paste(name,"'s", " - Mean economy rate (%) vs Wkts")
    plot(wkts,econRate,type="o",pch=13,col="red",lwd=3,xlab="Wickets",ylab="Mean Economy rate",main=atitle)
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}