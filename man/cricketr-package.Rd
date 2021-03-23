\name{cricketr-package}
\alias{cricketr-package}
\alias{cricketr}
\docType{package}
\title{
\packageTitle{cricketr}
This package analyzes the performances of cricketers using ESPN Cricinfo Statsguru data. The analysis can be done for Test, ODI and Twenty20 cricket for both batsman & bowlers
}
\description{
\packageDescription{cricketr}
This package analyzes the performances of cricketers using ESPN Cricinfo Statsguru data. The analysis can be done for Test, ODI and Twenty20 cricket for both batsman & bowlers
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{cricketr}
\packageIndices{cricketr}
Tools for analyzing performances of cricketers based on stats in  ESPN Cricinfo 
Statsguru. The toolset can  be used for analysis of Tests,ODIs  and Twenty20
matches of both batsmen and bowlers.
}
\author{
\packageAuthor{cricketr}
Tinniam V Ganesh
Maintainer: \packageMaintainer{cricketr}
Tinniam V Ganesh
tvganesh.85@gmail.com
}
\references{
Details in my post https://gigadom.in/2015/07/04/introducing-cricketr-a-r-package-to-analyze-performances-of-cricketers/
}
\keyword{ package }
\seealso{
    https://www.youtube.com/edit?o=U&video_id=q9uMPFVsXsI
}
\examples{
\dontrun{
getPlayerData(profile,opposition="",host="",dir="./data",file="player001.csv",
type="batting", homeOrAway=c(1,2),result=c(1,2,4))
getPlayerDataOD(profile, opposition="",host="",dir = "../", file = "player001.csv", 
type = "batting", homeOrAway = c(1, 2, 3), result = c(1, 2, 3,5))
getPlayerDataTT(profile, opposition="",host="",dir = "./data", file = "player001.csv", 
type = "batting", homeOrAway = c(1, 2, 3), result = c(1, 2, 3,5))
batsmanAvgRunsGround(file, name = "A Latecut")
bowlerAvgWktsGround(file, name = "A Chinaman")
tendulkar <- system.file("data", "tendulkar.csv", package = "cricketr")
ganguly <- system.file("data", "ganguly.csv", package = "cricketr")
dravid <- system.file("data", "dravid.csv", package = "cricketr")
batsmen <- list(tendulkar,dravid,ganguly)
names <- list("Tendulkar","Dravid","Ganguly")
relativeBatsmanCumulativeAvgRuns(batsmen,names)
}
}
