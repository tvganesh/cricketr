##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 Aug 2015
# Function: batsman4s6s
# This function computes and plots the percent of 4s,6s in total runs
#
###########################################################################################

batsman4s6s <-  function(frames, names) {
 
        #col1 = c("blue","orange","red")
        batsman4s6s <- NULL
        for(i in 1:length(frames))
        {
            # Clean batsman data
            batsman <- clean(frames[[i]])
            #batsman <- clean(file)
            Runs <- sum(batsman$Runs)
            batsman4s <- sum(batsman$X4s * 4)
            batsman6s <- sum(batsman$X6s * 6)
            #name <- names[[i]]
            a <-   c(Runs,batsman4s,batsman6s)       
            batsman4s6s <- cbind(batsman4s6s,a)
            
        }
        rownames(batsman4s6s) <- c('Runs(1s,2s,3s)',"4s","6s")
        colnames(batsman4s6s) <- names
        
        # Calculate the percentages and create a table
        prop <- prop.table(batsman4s6s,margin=2) *100
        par(mar=c(4,4,2,14),xpd=TRUE)
        
        # Create a stacked bar plot
        barplot(prop, col=heat.colors(length(rownames(prop))), width=2,
                ylab="% of total runs",main="Runs(1s,2s,3s), 4s, 6s as % of Total Runs")
        
        legend("topright",inset=c(-0.40,0), fill=heat.colors(length(rownames(prop))), 
               legend=rownames(prop),cex=0.8)  
        mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=3, adj=1.0, cex=0.7, col="blue")
        round(prop,2)
}