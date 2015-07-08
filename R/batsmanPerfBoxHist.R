##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 28 Jun 2015
# Function: batsmanPerfBoxHist
# This function makes a box plot showing the mean, median and the 25th & 75th percentile runs. The
# histogram shows the frequency of scoring runs in different run ranges
#
###########################################################################################
# Plot the batting performance as a combined box plot and histogram
batsmanPerfBoxHist <- function(file, name="A Hitter") {
    
    df <- clean(file)
    atitle <- paste(name,"'s", " - Runs Frequency vs Runs")
    
    # Set the layout and the margins. 
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
    par(mar=c(2,2,1,1))
    
    # Draw the boxplot
    boxplot(df$Runs, horizontal=TRUE,  outline=TRUE,ylim=c(0,max(df$Runs)), 
            frame=F, col = "green1")
    
    # Draw lines showing the mean and meadian
    abline(v=median(df$Runs),col="blue",lwd=3.0)
    abline(v=mean(df$Runs),col="red",lwd=3.0)
    mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=4, adj=1.0, cex=0.8, col="blue")
    
    # Create a vector from 0 with intervals of 10 for the intervals
    maxi <- (max(df$Runs/10) + 1) *10
    v <- seq(0,maxi,by=10)
    
      
    # Draw a histogram
    hist(df$Runs,breaks=v,xlab="Runs",ylab="Runs frequency", 
         main = atitle,labels=TRUE,col="grey")
    
    # Draw the median, mean, 1st and 3rd quantiles
    abline(v=median(df$Runs),col="blue",lwd=3.0)
    abline(v=mean(df$Runs),col="red",lwd=3.0)
    abline(v=quantile(df$Runs,.25),col="black",lwd=3.0,lty=2)
    abline(v=quantile(df$Runs,.75),col="black",lwd=3.0,lty=2)
    
    # Draw a rug below the histogram
    rug(df$Runs,col="blue",lwd=2)
    
    mn <- paste("Mean runs over career:",round(mean(df$Runs),2))
    md <- paste("Median runs over career:", round(median(df$Runs),2))
    
    # Get the value of count to determine the height of graph
    a <- hist(df$Runs, breaks=v,plot=FALSE)
    ht <- max(a$counts)

    text(200,ht-15,mn,col="brown")
    text(200,ht-20,md,col="brown")

    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=4, adj=1.0, cex=0.8, col="blue")
    # reset the layout
    par(mfrow=c(1,1))
    
}

