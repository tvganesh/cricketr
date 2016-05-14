##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 13 May 2016
# Function: relativeBowlerCumulativeAvgWickets
# This function computes and plots the relative cumulative average wickets of bowlers
#
###########################################################################################

relativeBowlerCumulativeAvgWickets <- function(frames, names){
    Wkts=cs=no=bowler=NULL
    g <- NULL
    for(i in 1:length(frames))
    {
        df <- cleanBowlerData(frames[[i]]) 
        b <- select(df,Wkts)
        b$no<-seq.int(nrow(b))
        c <- select(b,no,Wkts)
        
        d <- mutate(c,cs=cumsum(Wkts)/no)
        d$bowler <- names[[i]]
        g <- rbind(g,d)
    }
    plot.title= paste("Relative cumulative avg wickets vs No innings")
    ggplot(g,aes(no,cs,colour=bowler)) +geom_line(size=1) +
        xlab("No of innings") + ylab("Cumulative Avg. wickets") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:ESPN Cricinfo"),""))))
}
