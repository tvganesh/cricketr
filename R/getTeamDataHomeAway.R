##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 07 Jun 2019
# Function: getTeamData
# This function returns the team data as a CSV file and data frame for a given matchType
# homeOrAway and result vectors along with a column which includes whether the match was
#home, away, neutral zone etc. This can be done for a given team as 'bat' or 'bowl'
# option
#
###########################################################################################
#' @title
#' Get the data for a team in a match type  viz.for Test, ODI and T20 with the home/overseas/neutral
#'
#' @description
#' This function returns team data as a CSV file and/or a dataframe for Test, ODI and T20 with an additional
#' column showing home, away or neutral venue where the match was played
#'
#' @usage
#' getTeamDataHomeAway(dir=".",teamView="bat",matchType="Test",file="team001HA.csv",save=TRUE,teamName)
#'
#' @param dir
#' The directory where the team data CSV file be saved
#'
#' @param teamView
#' Team view can be either 'bat' (batting team) or 'bowl' (bowling team)
#'
#' @param matchType
#' The match type - Test, ODI , T20
#'
#' @param file
#' The name of te file to save to
#'
#' @param save
#' This can be TRUE or FALSE
#'
#' @param teamName
#' Team name is the team namely - Australia, India, England etc
#'
#' @return The required data frame
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
#' #Get the team data for India for Tests
#'
#' getTeamDataHomeAway(teamName="India",file="india.csv")
#' }
#' @seealso
#' \code{\link{teamWinLossStatusVsOpposition}}
#' \code{\link{teamWinLossStatusAtGrounds}}
#' \code{\link{plotTimelineofWinsLosses}}
#'
#' @export
#'
getTeamDataHomeAway <- function(dir=".",teamView="bat",matchType="Test",file="team001HA.csv",save=TRUE,teamName){
  print("Working...")
  # Check if match type is Test. The result is won, lost, draw or tie
  if(matchType == "Test"){
    df1= getTeamData(dir=".",file="team001.csv",matchType=matchType,homeOrAway=c(1),result=c(1,2,3,4),teamName=teamName,teamView)
    df2= getTeamData(dir=".",file="team001.csv",matchType=matchType, homeOrAway=c(2),result=c(1,2,3,4),teamName=teamName,teamView)
    df3= getTeamData(dir=".",file="team001.csv",matchType=matchType, homeOrAway=c(3),result=c(1,2,3,4),teamName=teamName,teamView)
  } else { #ODI & T20. The result is won, lost, tie or no result

    df1= getTeamData(dir=".",file="team001.csv",matchType=matchType,homeOrAway=c(1),result=c(1,2,3,5),teamName=teamName,teamView)
    df2= getTeamData(dir=".",file="team001.csv",matchType=matchType, homeOrAway=c(2),result=c(1,2,3,5),teamName=teamName,teamView)
    df3= getTeamData(dir=".",file="team001.csv",matchType=matchType, homeOrAway=c(3),result=c(1,2,3,5),teamName=teamName,teamView)
  }
  # Create the column values home, away or neutral
  df1$ha="home"
  df2$ha="away"
  df3$ha = "neutral"

  # Stack the rows to create dataframe
  df <- NULL
  #Check if empty
  if(!is.null(dim(df1)))
    df <- rbind(df,df1)

  #Check if empty
  if(!is.null(dim(df2)))
    df <- rbind(df,df2)

  #Check if empty
  if(!is.null(dim(df3)))
    df <- rbind(df,df3)

  # Save if required
  if(save){
    dir.create(dir,mode="0777",showWarnings=FALSE)
    file <-paste(dir,file,sep="/")
    file.create(file)
    write.csv(df,file=file)
  }
  print("Done")
  # Return the data frame
  df
}
