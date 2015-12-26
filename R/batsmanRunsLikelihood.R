##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 Jul 2015
# Function: batsmanRunsLikelihood
# This function used K-Means to compute and plot the runs likelihood for the batsman
#
###########################################################################################

batsmanRunsLikelihood <- function(file, name="A Squarecut") {

   Runs <- BF <-Mins <- Ground <-Wkts <- NULL
   batsman <- clean(file)
   data <- select(batsman,Runs,BF,Mins)

   # Use K Means with 3 clusters
   fit <- kmeans(data, 3)
   str <- paste(name,"'s Runs likelihood vs BF, Mins")
   # Create a 3D scatterplot
   s <-scatterplot3d(data$BF,data$Mins,data$Runs,color="lightblue",main=str,pch=20,
                     xlab="Balls Faced",ylab="Minutes at crease",zlab="Runs scored")
   
   # Plot the centroids
   s$points3d(fit$centers[1,2],fit$centers[1,3],fit$centers[1,1],col="blue",type="h", pch=15,lwd=4)
   s$points3d(fit$centers[2,2],fit$centers[2,3],fit$centers[2,1],col="red",type="h", pch=15,lwd=4)
   s$points3d(fit$centers[3,2],fit$centers[3,3],fit$centers[3,1],col="black",type="h", pch=15,lwd=4)
   mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
   
   p1 <- (sum(fit$cluster==1)/length(fit$cluster)) * 100
   p2 <- (sum(fit$cluster==2)/length(fit$cluster)) * 100
   p3 <- (sum(fit$cluster==3)/length(fit$cluster)) * 100
   
   # Print the summary of the centroids
   
   cat("Summary of ",name,"'s runs scoring likelihood\n")
   cat("**************************************************\n\n")
   cat("There is a",round(p1,2), "% likelihood that",name," will make ",round(fit$centers[1,1]),
              "Runs in ",round(fit$centers[1,2]),"balls over",round(fit$centers[1,3])," Minutes \n")
   
   cat("There is a",round(p2,2), "% likelihood that",name," will make ",round(fit$centers[2,1]),
       "Runs in ",round(fit$centers[2,2]),"balls over ",round(fit$centers[2,3])," Minutes \n")
   
   cat("There is a",round(p3,2), "% likelihood that",name," will make ",round(fit$centers[3,1]),
       "Runs in ",round(fit$centers[3,2]),"balls over",round(fit$centers[3,3])," Minutes \n")
   
}
