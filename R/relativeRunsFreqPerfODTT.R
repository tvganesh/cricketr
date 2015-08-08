##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 8 Aug 2015
# Function: relativeRunsFreqPerfODTT
# This function computes and plots the run frequencies of a list of batsman
#
###########################################################################################

# Overall performance used for comparing relative performance of batsman
relativeRunsFreqPerfODTT <- function(frames, names) {
    col1 = c("red","blue","cyan","black","brown")
    
    for (i in 1:length(frames)) {
        
        
        # Create run ranges of 10s
        g <- seq(from=5,to=250,by=10)
        
        # Calculate the runs frequency percent
        pR <- percentRuns(frames[[i]])
        
        a <- as.data.frame(pR)
        
        #Plot for the entire list
        if(i==1) {
            plot(g,a$Freq, cex=0.8, xlab="Runs", ylab = "Run frequency Percentages (%)", type="l",
                 lty=1, main = "Relative runs freq (%) vs Runs", xlim =c(0,150),ylim=c(0,50))
        }
        lines(g,a$Freq,col=col1[i], lwd=2.5,lty=1,type="l")
    }
    
    type = rep(1,length(frames))
    width = rep(2.5,length(frames))
    legend(x="right",legend=names, lty=type,   
           lwd=width,col=col1,bty="n")
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}



# Helper function that calculates the percentage of runs in ranges of 10
percentRuns <- function(file) {
    
    batsman <- clean(file)
    
    # Divide batting into groups of 20 runs
    f <- cut(batsman$Runs, breaks=seq(from=0,to=250,by=10))
    
    # Create a table
    g <- table(f)
    
    # Compute percentage
    percentRuns <- (g/sum(g))*100
    percentRuns
}