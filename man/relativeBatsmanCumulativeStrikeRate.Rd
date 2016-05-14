\name{relativeBatsmanCumulativeStrikeRate}
\alias{relativeBatsmanCumulativeStrikeRate}
\title{Relative batsmen cumulative average strike rate}
\usage{
relativeBatsmanCumulativeStrikeRate(frames, names)
}
\arguments{
  \item{frames}{
This is a list of <batsman>.csv files obtained with an initial getPlayerData()
}
  \item{names}{
A list of batsmen names who need to be compared
}
}
\value{
None
}
\description{
This function computes and plots the cumulative average strike rate  of  batsmen
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
# Retrieve the file path of a data file installed with cricketr
tendulkar <- system.file("data", "tendulkar.csv", package = "cricketr")
ganguly <- system.file("data", "ganguly.csv", package = "cricketr")
dravid <- system.file("data", "dravid.csv", package = "cricketr")

batsmen <- list(tendulkar,dravid,ganguly)
names <- list("Tendulkar","Dravid","Ganguly")
relativeBatsmanCumulativeStrikeRate(batsmen,names)
}
}
\author{
Tinniam V Ganesh
}
\references{
http://www.espncricinfo.com/ci/content/stats/index.html\cr
https://gigadom.wordpress.com/
}
\seealso{
\code{\link{relativeBatsmanCumulativeAvgRuns}}
\code{\link{relativeBowlerCumulativeAvgEconRate}}
\code{\link{relativeBowlerCumulativeAvgWickets}}
}

