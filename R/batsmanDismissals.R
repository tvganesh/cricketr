# Plot the dismissals of the batsman as a pie chart
batsmanDismissals <- function(file, name="A Squarecut") {
  
  batsman <- clean(file)
  
  lbls <- NULL
  
  d <- batsman$Dismissal
  
  # Convert to data frame
  dismissal <- data.frame(table(d))
  
  # Create a 3D pie chart
  lbls <- dismissal$d
  slices <- dismissal$Freq
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  atitle <- paste(name, "-Pie chart of dismissals")
  
  # Important note: Ensure the number of labels & slices match
  pie3D(slices, labels=lbls,explode=0.1, main= atitle,pty="s")
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=1, col="blue") 
  
}