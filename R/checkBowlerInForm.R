##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 29 Jun 2015
# Function: checkBowlerInForm
# This function checks whether the bowler is In-Form or Out-of-Form
#
###########################################################################################
checkBowlerInForm <- function(file, name="A N Inswinger",alpha=0.05) {
    
    # Clean the file
    bowler <- cleanBowlerData(file)
    
    # Get the runs in career
    wkts <- bowler$Wkts
    len <- length(wkts)
    
    # Take 90% as polpulation
    poplen <- floor(0.9 * len)
    popwkts <- wkts[1:poplen]
    
    #Calculate the mean 'mu' of this runs population
    mu <- round(mean(popwkts),2)
    
    # Get the offset for the sample 
    m <- poplen+1
    
    # Create a sample from last 10% of runs scored
    sample <- wkts[m:len]
    
    # Calculate mean & SD of sample
    xbar <- round(mean(sample),2)
    s <- round(sd(sample),2)
    
    # No of degress of freedom
    n <- len-poplen
    
    # The NULL hypothesis H0 will be the batsman scores at the mean or above the meanb
    # Compute t statistic
    t <- (xbar - mu)/(s/sqrt(n))
    
    # The Ha will be that the batsman scores less than the current average and hence out of form
    # Compute the lower tail
    pValue <- round(pt(t, n, lower.tail = TRUE),6)
    
    
    if(pValue > alpha){
        
        str4 <- paste(name,"'s Form Status: In-Form because the p value:", pValue," is greater than alpha= ", alpha)
    } else {
        #print(str2)
        str4 <-paste(name, "'s Form Status: Out-of-Form because the p value:", pValue," is less than alpha= ", alpha)
    }
    # Output the text as a string
    m1 <- paste("**************************** Form status of",name,"****************************\n\n")
    m2 <- paste("Population size:",poplen," Mean of population:",mu,"\n")
    m3 <- paste("Sample size:",n," Mean of sample:",xbar, "SD of sample:", s,"\n\n")
    m4 <- paste("Null hypothesis H0 :",name,"'s sample average is within 95% confidence interval 
        of population average\n")
    m5 <- paste("Alternative hypothesis Ha :",name,"'s sample average is below the 95% confidence
        interval of population average\n\n")
    m6 <- paste(str4,"\n")
    m7 <- "*******************************************************************************************\n\n"
    m8 <- paste(m1,m2,m3,m4,m5,m6,m7)
    m8
}