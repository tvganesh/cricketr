##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 12 May 2016
# Function: batsmanCumulativeStrikeRate
# This function computes and plots the cumulative average strike rate of a batsman
#
###########################################################################################

batsmanCumulativeStrikeRate <- function(file,name="A Leg Glance"){
    strikeRate=cs=no=BF=Runs=NULL
    df <- clean(file) 
    df <- mutate(df,strikeRate=(Runs/BF)*100)
    b <- select(df,strikeRate)
    b$no<-seq.int(nrow(b))
    c <- select(b,no,strikeRate)

    d <- mutate(c,cs=cumsum(strikeRate)/no)
    plot.title= paste(name,"- Cumulative Strike Rate vs No of innings")
    ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
        xlab("No of innings") + ylab("Cumulative Avg. Strike Rates") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:ESPN Cricinfo"),""))))
}
