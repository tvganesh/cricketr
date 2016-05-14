
##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 12 May 2016
# Function: relativeBatsmanCumulativeAvgRuns
# This function computes and plots the relative cumulative average runs of batsmen
#
###########################################################################################
relativeBatsmanCumulativeAvgRuns <- function(frames, names){
    Runs=cs=no=batsman=NULL
    g <- NULL
    for(i in 1:length(frames))
    {
        # Clean batsman data
        df <- clean(frames[[i]])
        
        b <- select(df,Runs)
        b$no<-seq.int(nrow(b))
        c <- select(b,no,Runs)
        
        d <- mutate(c,cs=cumsum(Runs)/no)
        d$batsman <- names[[i]]
        g <- rbind(g,d)
    }
    
    plot.title= paste("Relative Cumulative Average Runs vs No of innings")
    ggplot(g,aes(no,cs,colour=batsman)) +geom_line(size=1) +
        xlab("No of innings") + ylab("Cumulative Avg. runs") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:ESPN Cricinfo"),""))))
    
}

