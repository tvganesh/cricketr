##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Jun 2015
# Function : clean
# This function cleans the batsman's data file and returns the cleaned data frame for use in
# other functions
##########################################################################################
clean <- function(file) {
  
    df <- read.csv(file,stringsAsFactor=FALSE,na.strings=c(NA,"-"))
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
  
  # Remove the "* indicating not out
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
  
  c <- complete.cases(batsman)
  
  batsmanComplete <- batsman[c,]
  
  list(val=dim(batsmanComplete),names = names(batsmanComplete),h=head(batsmanComplete))
  
  #Return the data frame 
  batsmanComplete
}

