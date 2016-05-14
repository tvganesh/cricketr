##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 12 May 2016
# Function: bowlerCumulativeAvgWickets
# This function computes and plots the cumulative average wickets of a bowler
#
###########################################################################################

bowlerCumulativeAvgWickets <- function(file,name="A Googly"){
    Wkts=cs=no=NULL
    df <- cleanBowlerData(file) 
    b <- select(df,Wkts)
    b$no<-seq.int(nrow(b))
    c <- select(b,no,Wkts)

    d <- mutate(c,cs=cumsum(Wkts)/no)
    plot.title= paste(name,"- Cumulative avg wkts vs No innings")
    ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
        xlab("No of innings") + ylab("Cumulative Avg. wickets") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:ESPN Cricinfo"),""))))
}
