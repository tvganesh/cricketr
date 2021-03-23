##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 07 Jun 2019
# Function: plotTimelineofWinsLosses
# This function plot the timelines of win/lost/draw/tie against opposition at
# venues for Test, ODI and T20s
#
###########################################################################################
#' @title
#' Plot the time line of wins/losses/draw/tied etc for a Team in Test, ODI or T20
#'
#' @description
#' This function returns plots a time line of won,lost,draw,tied or no result for a team against
#' other teams in home/away or neutral venues
#'
#' @usage
#' plotTimelineofWinsLosses(file,teamName,opposition=c("all"),homeOrAway=c("all"),
#'       startDate="2001-01-01",endDate="2019-01-01",matchType="Test")
#'
#' @param file
#' The CSV file for which the plot is required
#'
#' @param teamName
#' The name of the team for which plot is required
#'
#' @param opposition
#' Opposition is a vector namely c("all") or c("Australia", "India", "England")
#'
#' @param homeOrAway
#' This parameter is a vector which is either c("all") or a vector of venues c("home","away","neutral")
#'
#' @param startDate
#' The start date from which time line is required
#'
#' @param endDate
#' The end data for which the time line plot is required
#'
#' @param matchType
#' Match type - Test, ODI or T20
#'
#' @return None
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
#'
#' df <- getTeamDataHomeAway(teamName="India",file="indiaOD.csv",matchType="ODI")
#' plotTimelineofWinsLosses("indiaOD.csv",teamName="India",
#'         startDate="2015-01-01",endDate="2019-01-01", matchType="ODI")
#' }
#' @seealso
#' \code{\link{teamWinLossStatusVsOpposition}}
#' \code{\link{teamWinLossStatusAtGrounds}}
#' \code{\link{plotTimelineofWinsLosses}}
#'
#' @export
#'
plotTimelineofWinsLosses <- function(file,teamName,opposition=c("all"),homeOrAway=c("all"),
                                     startDate="2001-01-01",endDate="2019-01-01",matchType="Test") {
  Opposition <- ha <- team <- result <- NULL

  # Read CSV
  df = read.csv(file,stringsAsFactors = FALSE)

  #Clean data
  df1<- cleanTeamData(df,matchType)


  # Get the vector of countries in opposition
  if("all" %in% opposition){
    #Do not filter
  } else {
    df1 <- df1 %>% filter(Opposition %in% opposition)
  }

  #Check home/away/neutral
  if("all" %in% homeOrAway ){
    # Do not filter
  } else {
    df1 <- df1 %>% filter(ha  %in% homeOrAway)
  }

  #Collapse vector of opposition
  oppn = paste(opposition,collapse='-')
  # Collapse vector of homeOrAway
  ground = paste(homeOrAway,collapse='-')

  atitle <- paste("Timeline of Win/Loss status of",teamName,"in",matchType,"(s)")
  asub <- paste("Against",oppn," teams at", ground, "grounds")

  # FIlter won and set to 1
  a= df1$Result == "won"
  df2 = df1[a,]
  if(dim(df2)[1] != 0)
    df2$result=1

  #Filter tie and set to 0.5
  a= df1$Result == "tie"
  df3=df1[a,]
  # No tie
  if (dim(df3)[1] != 0)
    df3$result=0.5

  # Test has w
  if(matchType == "Test"){
    # FIlter draw and set to -0.5
    a= df1$Result == "draw"
    df4 = df1[a,]
    if(dim(df4)[1] != 0) # No draw
      df4$result=-0.5
  } else if ((matchType == "ODI" ) || (matchType == "T20")){
    # FIlter 'no result' and set to 0
    a= df1$Result == "n/r"
    df4 = df1[a,]
    if(dim(df4)[1] != 0) #
      df4$result=0
  }

  # Filter lost and set to -1
  a= df1$Result == "lost"
  df5=df1[a,]
  if(dim(df5)[1] != 0)
    df5$result=-1

  # Rbind the dataframes with a new column
  df6=rbind(df2,df3,df4,df5)

  # Convert to date
  df6$date = dmy(df6$`Start.Date`)

  # Sort dates
  df7 = df6 %>% select(date,result) %>% arrange(date)

  # Filter between start and end dates
  df8= df7 %>% filter(date >=  startDate & date <= endDate)

  # Plot the timeline of wins losses
  # Match type is Test
  if(matchType == "Test"){
    ggplot(data=df8, aes(x=date, y=result, group=1)) +
      geom_line(color="red")+
      geom_point() +
      labs(x="Date",
           y="Win/loss status",
           title= atitle,
           subtitle= asub,
           caption = "Data source-Courtesy:ESPN Cricinfo", side=1,
           line=4, adj=1.0, cex=0.8, col="blue") +
      geom_hline(yintercept=1, linetype="dashed", color = "blue",size=1) +

      geom_hline(yintercept=0.5, linetype="dashed", color = "orange",size=1) +
      geom_hline(yintercept=-0.5, linetype="dashed", color = "coral1",size=1) +
      geom_hline(yintercept=-1, linetype="dashed", color = "black",size=1) +
      # Add Labels
      geom_text(aes(x=as.Date(startDate),y=1,label="Won",vjust=-0.5)) +
      geom_text(aes(x=as.Date(startDate),y=0.5,label="Tie",vjust=-0.5)) +
      geom_text(aes(x=as.Date(startDate),y=-0.5,label="Draw",vjust=-0.5)) +
      geom_text(aes(x=as.Date(startDate),y=-1,label="Lost",vjust=-0.5))
  } else if ((matchType == "ODI") || (matchType == "T20")){
    #Match type is ODI and T20s

    ggplot(data=df8, aes(x=date, y=result, group=1)) +
      geom_line(color="red",size=1)+
      geom_point() +
      labs(x="Date",
           y="Win/loss status",
           title= atitle,
           subtitle= asub,
           caption = "Data source-Courtesy:ESPN Cricinfo", side=1,
           line=4, adj=1.0, cex=0.8, col="blue") +
      geom_hline(yintercept=1, linetype="dashed", color = "blue",size=1) +

      geom_hline(yintercept=0.5, linetype="dashed", color = "orange",size=1) +
      geom_hline(yintercept=0, linetype="dashed", color = "coral1",size=1) +
      geom_hline(yintercept=-1, linetype="dashed", color = "black",size=1) +
      # Add Labels
      geom_text(aes(x=as.Date(startDate),y=1,label="Won",vjust=-0.5)) +
      geom_text(aes(x=as.Date(startDate),y=0.5,label="Tie",vjust=-0.5)) +
      geom_text(aes(x=as.Date(startDate),y=0,label="No result",vjust=-0.5)) +
      geom_text(aes(x=as.Date(startDate),y=-1,label="Lost",vjust=-0.5))
  }

}
