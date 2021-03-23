##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 07 Jun 2019
# Function: getTeamNumber
# This function returns the team number for a team name for Tests, ODIs and T20s
#
###########################################################################################
#' @title
#' Get the number of the Team
#'
#' @description
#' This function returns the number of the Team for which analysis is to be done
#'
#' @usage
#' getTeamNumber(teamName,matchType)
#'
#' @param teamName
#' The name of the team e.g Australia, India, Ghana etc
#'
#' @param matchType
#' The match type - Test, ODI or T20
#'
#'
#' @return The numerical value of the team
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
#' teamNi <-getTeamNumber(teamName="India",matchType="Test")
#' }
#' @seealso
#' \code{\link{teamWinLossStatusVsOpposition}}
#' \code{\link{teamWinLossStatusAtGrounds}}
#' \code{\link{plotTimelineofWinsLosses}}
#'
#' @export
#'
getTeamNumber <- function(teamName,matchType){

  # Match type is Test
  if(matchType == "Test"){
    # Test teams
    teams <-list("Afghanistan"=40, "Australia"=2,"Bangladesh"=25,"England"=1,"ICC World X1"=40,
                 "India"=6,"Ireland"=29,"New Zealand"=5,"Pakistan"=7,"South Africa"=3,
                 "Sri Lanka"=8,"West Indies"=4,"Zimbabwe"=9)

    # Match type is ODI
  } else if (matchType == "ODI"){
    # ODI teams
    teams <-list("Afghanistan"=40, "Africa XI"=4058, "Asia XI"=106, "Australia"=2,
                 "Bangladesh"=25,"Bermuda"=12 ,"England"=1,"ICC World X1"=140,
                 "India"=6,"Ireland"=29,"New Zealand"=5,"Pakistan"=7,"South Africa"=3,
                 "Sri Lanka"=8,"West Indies"=4,"Zimbabwe"=9,"Canada"=17,"East Africa"=14,
                 "Hong Kong"=19,"ICC World XI"=140,"Ireland"=29,"Kenya"=26,"Namibia"=28,
                 "Nepal"=32,"Netherlands"=15,"Oman"=37,"Papua New Guinea"=20,"Scotland"=30,
                 "United Arab Emirates"=27,"United States of America"=11, "Zimbabwe"=9)


  } else if (matchType == "T20") {
    # T20 Teams
    teams <-list("Afghanistan"=40, "Australia"=2,"Bahrain"=108, "Bangladesh"=25,
                 "Belgium"=42, "Belize"=115,"Bermuda"=12,"Botswana" =116,"Canada"=17,"Costa Rica"=4082,
                 "Germany"=35,"Ghana"=135,"Guernsey"=1094,"Hong Kong"=19,"ICC World X1"=140,
                 "India"=6,"Ireland"=29,"Italy"=31, "Jersey"=4083,"Kenya"=26,"Kuwait"=38,
                 "Maldives"=164,"Malta"=45,"Mexico"=165,"Namibia"=28,"Nepal"=32,"Netherlands"=15,
                 "New Zealand"=5,"Nigeria"=173, "Oman"=37,"Pakistan"=7,"Panama"=183,"Papua New Guinea"=20,
                 "Philippines"=179,"Qatar"=187,"Saudi Arabia"=154,"Scotland"=30,"South Africa"=3,
                 "Spain"=200, "Sri Lanka"=8,"Uganda"=34,"United Arab Emirates"=27,
                 "United States of America"=11,"Vanuatu"=216,"West Indies"=4)
  } else {
    print("Unknown match ")
  }
  # Return team number
  teams[[teamName]]
}
