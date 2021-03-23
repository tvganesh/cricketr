##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 07 Jun 2019
# Function: cleanTeamData
# This function cleans the team data for Test, ODI and T20
#
###########################################################################################
#' @title
#' Clean the team data for Test, ODI and T20
#'
#' @description
#' This function cleans the team data for Test, ODI and T20
#'
#' @usage
#' cleanTeamData(df,matchType)
#'
#' @param df
#' Data frame
#'
#' @param matchType
#' Match type - Test, ODI, T20
#'
#' @return The cleaned Data frame
#' @references
#' \url{https://www.espncricinfo.com/ci/content/stats/index.html}\cr
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
#' #Get the team data for India for Tests
#' df<-getTeamDataHomeAway(file="india.csv",teamName="India",matchType='Test')
#' df1 <-cleanTeamData(df,"Test")
#' }
#' @seealso
#' \code{\link{teamWinLossStatusVsOpposition}}
#' \code{\link{teamWinLossStatusAtGrounds}}
#' \code{\link{plotTimelineofWinsLosses}}
#'
#' @export
#'
cleanTeamData <-function(df,matchType){
  #Remove rows with 'DNB
  a=df$Score != 'DNB'
  df1 = df[a,]

  if (matchType =="Test"){
    # Remove columns 9 & 13 for Tests. They have empty columns
    df2 = df1[, -c(9,13)]
    # Remove columns 8 & 12 for ODI and T20. They have empty columns
  } else if ((matchType == "ODI") || (matchType == "T20")){
    df2 = df1[, -c(8,12)]
  }

  # Fix the Opposition column, remove "^ v"
  df2$Opposition =gsub("^v ","",df2$Opposition)
  # Return cleaned dataframe
  df2
}
