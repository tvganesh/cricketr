
##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 1 Jul 2015
# Function: relativeBatsmanSR
# This function computes and plot relative Mean Strike Rates of batsmen vs RUns scored
#
###########################################################################################
relativeBatsmanSR <- function(frames, names) {
    col1 = c("red","blue","cyan","black")
    for(i in 1:length(frames))
    {
        # Clean batsman data
        batsman <- clean(frames[[i]])
        
        # Create a vector of runs with intervals of 15
        maxi <- (max(batsman$Runs/15) + 1) *15
        v <- seq(0,maxi,by=15)
        a <- hist(batsman$Runs,breaks=v,plot=FALSE)        
        
        # Compute the Mean Strike Rate for each run range
        SR <- NULL
        for(j in 2:length(a$breaks))  {
            b <- batsman$Runs > a$breaks[j-1] & batsman$Runs <= a$breaks[j] 
            c <- batsman[b,]
            SR[j-1] <- mean(as.numeric(as.character(c$SR)))
        }
        
        # Find all intervals where there is no data i.e. NAN
        b <- !is.na(SR)
        
        #Subset and remove the NANs for counts
        c <- a$mid[b]
        
        #Subset and remove the NANs for Strike Rate
        SR <- SR[b]
        
        par(mar=c(4,4,1,1))
        if(i==1) {
            plot(c,predict(loess(SR~c)),xlab="Runs",ylab="Mean Strike Rate",
                 xlim=c(0,400), ylim=c(0,90), type="l",lty=1,lwd=3, col=col1[i],
                 main="Relative Mean Strike Rate")
        } else {
            lines(c,predict(loess(SR~c)),col=col1[i],lwd=3)
        }
    }
    
    type = rep(1,length(frames))
    width = rep(2.5,length(frames))
    legend(x="topright",legend=names, lty=type,   
           lwd=width,col=col1,bty="n",cex=0.8)
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
    
}