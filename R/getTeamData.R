##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 07 Jun 2019
# Function: getTeamData
# This function returns the team data as a CSV file and data frame for a given matchType
# homeOrAway and result vectors. This can be done for a given team as 'bat' or 'bowl'
# option
#
###########################################################################################
#' @title
#' Get the data for a team in a match type  viz.for Test, ODI and T20
#'
#' @description
#' This function returns team data as a CSV file and/or a dataframe for Test, ODI and T20
#'
#' @usage
#' getTeamData(dir=".",file="team001.csv",matchType="Test",
#'         homeOrAway=c(1,2,3),result=c(1,2,3,4),teamView="bat",save=FALSE,teamName)
#'
#' @param dir
#' The directory where the team data CSV file be saved
#'
#' @param file
#' The name of the CSV file to save to
#'
#' @param matchType
#' The match type - Test, ODI , T20
#'
#' @param homeOrAway
#' Whether the data has to be got for home-1, away(overseas)-2 or neutral -3
#'
#' @param result
#' The result of the match for which data is to be saved - won-1, lost -2, tied-3, draw-4
#'
#' @param teamView
#' This can be 'bat' - batting team or 'bowl' - bowling team
#'
#' @param save
#'  This can be set as TRUE or FALSE
#'
#' @param teamName
#' This is team name
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
#' df=getTeamData(dir=".",file="australia.csv", matchType="Test",homeOrAway=c(1,2,3),
#'        result=c(1,2,3, 4),teamView='bat',teamName="Australia")
#' }
#' @seealso
#' \code{\link{teamWinLossStatusVsOpposition}}
#' \code{\link{teamWinLossStatusAtGrounds}}
#' \code{\link{plotTimelineofWinsLosses}}
#'
#' @export
#'
getTeamData <- function(dir=".",file="team001.csv",matchType="Test",
                        homeOrAway=c(1,2,3),result=c(1,2,3,4),teamView="bat",save=FALSE,teamName) {

  # Initialize url to ""
  url <-""
  suburl1 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html"

  # Get the match tyoe
  match <-getMatchType(matchType)
  #Create suburl2
  suburl2 <- paste("?class=",match,";",sep="")
  #suburl2 <-"?class=1;"
  suburl3 <- "template=results;"
  suburl4 <- "type=team;"
  suburl5 <- "size=200;"
  suburl6 <- "view=innings;"

  # Set the home or away depending on the values in homeOrAway vector
  str1=str2=str3=""
  if(is.element(1, homeOrAway)){
    str1 ="home_or_away=1;"
  }
  if (is.element(2, homeOrAway)) {
    str2="home_or_away=2;"
  }
  if (is.element(3, homeOrAway)) {
    str3="home_or_away=3;"
  }
  HA<-paste(str1,str2,str3,sep="")

  # Set the result based on  result vector
  str1=str2=str3=str4=""
  if(is.element(1, result)){
    str1 ="result=1;"
  }
  if(is.element(2, result)){
    str2 ="result=2;"
  }
  if(is.element(3, result)){
    str3 ="result=3;"
  }

  # Test has result 'draw-4' while ODI and T20 have result 'no result-5'
  if(matchType == "Test"){ #Test
    if(is.element(4, result)){
      str4 ="result=4;"
    }
  } else { #ODI & T20
    if(is.element(5, result)){
      str4 ="result=5;"
    }
  }
  result<- paste(str1,str2,str3,str4,sep="")
  # Set the team

  # Get the team number
  team=getTeamNumber(teamName, matchType)

  # Set the team for which data is required
  theTeam= paste("team=",team,";",sep="")

  # Set the data view
  dataView <- paste("team_view=",teamView,";",sep="")

  # Create composite URL
  baseurl <- paste(suburl1,suburl2,suburl3,suburl4,suburl5,suburl6,HA,result,theTeam,dataView, sep="")

  # Loop the pages
  notDone <- TRUE
  i <- 0
  df <- NULL

  # Loop through the data 200 (max)rows at a time
  while (notDone){
    i <- i+1
    page <-""
    page <- paste("page=",i,sep="")
    url <- paste(baseurl,page,sep="")
    tabs <- GET(url)
    tables=readHTMLTable(rawToChar(tabs$content),stringsAsFactors = F)
    
    t <- tables$"Innings by innings list"
    if(dim(t)[1] == 1){
      notDone <- FALSE
      break
    }
    else{
      df <- rbind(df,t)
    }
  }

  # Check if save is required
  if(save){
    dir.create(dir,mode="0777",showWarnings=FALSE)
    file <-paste(dir,file,sep="/")
    file.create(file)
    write.csv(df,file=file)
  }
  # Return the data frame
  df
}
