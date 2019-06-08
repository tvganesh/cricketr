##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 07 Jun 2019
# Function: getMatchType
# This function returns the match number for a  match type viz. Tests, ODIs and T20s
#
###########################################################################################
#' @title
#' Get the number of the match type viz.for Test, ODI and T20
#'
#' @description
#' This function returns the number of the match type
#'
#' @usage
#' getMatchType(matchType)
#'
#' @param matchType
#' The match type - Test, ODI or T20
#'
#'
#' @return The numerical value of match type
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
#' match <-getMatchType("Test")
#' }
#' @seealso
#' \code{\link{teamWinLossStatusVsOpposition}}
#' \code{\link{teamWinLossStatusAtGrounds}}
#' \code{\link{plotTimelineofWinsLosses}}
#'
#' @export
#'
getMatchType <- function(matchType){
  match <- list("Test"=1,"ODI"=2,"T20"=3)
  match[[matchType]]
}
