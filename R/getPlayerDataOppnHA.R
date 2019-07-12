##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 10 Jul 2019
# Function: getPlayerDataOppnHA
# This function creates a filtered CSV file with a players data for matches against specified opposition
# at home/away/neutral venues during an interval
###########################################################################################
#' @title
#' Return a filtered CSV file for a player against specified opposition, at home/away venues during an interval
#'
#' @description
#' This function saves the filtered players data as a CSV file for matches against specified opposition,
#' at home.away venues for a specified interval
#'
#' @usage
#' getPlayerDataOppnHA(infile,outfile,dir=".",opposition=c("all"),homeOrAway=c("all"),
#'                           startDate="2001-01-01",endDate="2019-01-01")
#'
#' @param infile
#' The input CSV HA file for the player
#'
#'
#' @param outfile
#' The name of the output CSV file which is filtered file based on opposition,home/away for a period
#'
#' @param dir
#' The name of the directory to store output file
#'
#' @param opposition
#' This is a vector of opposition for e.g. c("Australia","India","South Africa"). Default is c("all")
#'
#'
#' @param homeOrAway
#' This is a vector of "home","away" or "neutral". Default is c("all")
#'
#' @param startDate
#' This is a date from which you would like the data for player "yyyy-mm-dd" format
#'
#' @param endDate
#' This is a end date till which you need data to be filtered of "yyyy-mm-dd" format
#'
#' @return dataframe
#'
#' @references
#' \url{http://www.espncricinfo.com/ci/content/stats/index.html}\cr
#' \url{https://gigadom.in/}\cr
#'
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' #Get data for Kohli  against England in 'away' venues in the year 2014
#' df=getPlayerDataOppnHA(infile="kohliHA.csv",outfile="kohliEAN2014.csv",
#'             opposition=c("England","Australia","New Zealand"),
#' homeOrAway=c("away"),startDate="2014-01-01",endDate="2015-01-01")
#'
#' # Get data for Tendulkar between 2001 and 2002
#' df1=getPlayerDataOppnHA(file,outfile="tendulkar2001.csv",startDate="2001-01-01",
#'                                                          endDate="2002-01-01")
#'
#' }
#' @seealso
#' \code{\link{teamWinLossStatusVsOpposition}}
#' \code{\link{batsman4s6s}}
#'
#' @export
#'
getPlayerDataOppnHA <- function(infile,outfile,dir=".",opposition=c("all"),homeOrAway=c("all"),
                                startDate="2001-01-01",endDate="2019-01-01") {
    Opposition <- ha <-NULL
    df1=clean(infile)
    
    # Check the opposition vector
    if("all" %in% opposition){
        #Do not filter
    } else {
        df1 <- df1 %>% filter(Opposition %in% opposition)
    }

    #Check home/away/neutral and filter
    if("all" %in% homeOrAway ){
        # Do not filter
    } else {
        df1 <- df1 %>% filter(ha  %in% homeOrAway)
    }

    # Convert to date
    df1$date = dmy(df1$`Start.Date`)


    # Filter between start and end dates
    df2= df1 %>% filter(date >=  startDate & date <= endDate)

    dir.create(dir,mode="0777",showWarnings=FALSE)
    file <-paste(dir,outfile,sep="/")
    file.create(file)
    write.csv(df2,file=file)

    # Return the data frame
    df2

}
