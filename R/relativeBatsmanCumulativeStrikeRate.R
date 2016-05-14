##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 12 May 2016
# Function: relativeBatsmanCumulativeStrikeRate
# This function computes and plots the relative cumulative average strike rate of
# batsmen
#
###########################################################################################

relativeBatsmanCumulativeStrikeRate <- function(frames, names){
    strikeRate=cs=no=BF=Runs=batsman=NULL
    g <- NULL
    for(i in 1:length(frames))
    {
        # Clean batsman data
        df <- clean(frames[[i]])
        df <- mutate(df,strikeRate=(Runs/BF)*100)
        b <- select(df,strikeRate)
        b$no<-seq.int(nrow(b))
        c <- select(b,no,strikeRate)
        
        d <- mutate(c,cs=cumsum(strikeRate)/no)
        d$batsman <- names[[i]]
        g <- rbind(g,d)
        
    }
    plot.title= paste("Relative Cumulative Average Strike Rate vs No of innings")
    ggplot(g,aes(no,cs,colour=batsman)) +geom_line(size=1) +
        xlab("No of innings") + ylab("Cumulative Avg. Strike Rates") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:ESPN Cricinfo"),""))))
}
