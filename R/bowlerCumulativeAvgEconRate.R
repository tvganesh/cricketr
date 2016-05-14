##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 12 May 2016
# Function: bowlerCumulativeAvgEconRate
# This function computes and plots the cumulative average economy rate of a bowler
#
###########################################################################################

bowlerCumulativeAvgEconRate <- function(file,name="A Googly"){
    Econ=cs=no=NULL
    df <- cleanBowlerData(file) 
    b <- select(df,Econ)
    b$no<-seq.int(nrow(b))
    c <- select(b,no,Econ)

    d <- mutate(c,cs=cumsum(Econ)/no)
    plot.title= paste(name,"- Cum. avg Econ Rate vs No innings")
    ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
        xlab("No of innings") + ylab("Cumulative Avg. Economy Rate") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:ESPN Cricinfo"),""))))
}
