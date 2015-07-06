##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Jun 2015
# Function: batsman6s
# This function plots the number of 6s vs the runs scored in the innings by the batsman
#
###########################################################################################
# Plot the Percentage of runs in 6s vs Runs scored
batsman6s <- function(file, name="A Hookshot") {
    
    # Clean the batsman file and create a complete data frame
    df <- clean (file)  
  
    # Remove all rows which have 0 6's
    b <-filter(df,X6s !=0)
    x6s <- as.numeric((b$X6s))
    runs <- as.numeric(b$Runs)
   
    # Create a color palette
    p1 <-colorRampPalette(c("red","blue"))
    palette <- p1(max(x6s))    
    
    atitle = paste(name,"-","Runs scored vs No of 6s" )
    
    # Create box plot of number of 6s and the runs range
    boxplot(runs~x6s,main=atitle,xlab="Number of 6s",ylab="Runs scored", col=as.vector(palette))
   
    
    mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
    
}
