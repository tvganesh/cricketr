##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 12 May 2016
# Function: batsmanCumulativeAverageRuns
# This function computes and plots the cumulative average runs by a batsman
#
###########################################################################################
batsmanCumulativeAverageRuns <- function(file,name="A Leg Glance"){
    Runs=cs=no=BF=NULL
    df <- clean(file) 
    b <- select(df,Runs)
    b$no<-seq.int(nrow(b))
    c <- select(b,no,Runs)

    d <- mutate(c,cs=cumsum(Runs)/no)
    plot.title= paste(name,"- Cumulative Average vs No of innings")
    ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
        xlab("No of innings") + ylab("Cumulative Avg. runs") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:ESPN Cricinfo"),""))))
}
