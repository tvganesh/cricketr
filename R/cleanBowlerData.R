##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 28 Jun 2015
# Function : cleanBowlerData
# This function cleans the bowler's data file and returns the cleaned data frame for use in
# other functions
##########################################################################################
cleanBowlerData <- function(file) {
    
    BPO <- Overs <- NULL
    # Read the <bowler>.csv file
    df <- read.csv(file,stringsAsFactor=FALSE,na.strings=c(NA,"-"))
    
    # Remove rows with did not bowl
    a <- df$Overs != "DNB"
    bowler <- df[a,]
    
    # Remove rows with 'TDNB' - team did not bowl
    c <- bowler$Overs != "TDNB"
    bowler <- bowler[c,]
    
    # Get all complete cases
    c <- complete.cases(bowler)
    bowlerComplete <- bowler[c,]

    # Normalize overs which had 8 balls per over to the number of overs if there 8 balls per over
    if(names(bowlerComplete)[3] == "BPO") {
        bowlerComplete <- mutate(bowlerComplete, Overs = ifelse(BPO==8,as.numeric(Overs)*8/6,Overs))   
    }
    
    #Return the data frame 
    bowlerComplete
}