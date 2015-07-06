##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 1 Jul 2015
# Function: batsmanRunsPredict
# This function predicts the runs that will be scored by the batsman for a given numbers
# of balls faced and minutes at crease
#
###########################################################################################
batsmanRunsPredict <- function(file, name="A Coverdrive", newdataframe) {
    batsman <- clean(file)
    
    # Fit a linear regression line between Runs and BF & Mins
    fit <- lm(as.numeric(Runs) ~ BF+Mins,data=batsman)

    # Predict based on the fitted model
    Runs <- predict(fit,newdata=newdataframe)
  
    newdataframe$Runs <- Runs
    names(newdataframe) <- c("Balls Faced","Minutes",'Runs')
    newdataframe
}