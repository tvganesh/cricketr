\name{relativeBatsmanCumulativeAvgRuns}
\alias{relativeBatsmanCumulativeAvgRuns}
\title{Relative batsman's cumulative average runs}
\usage{
relativeBatsmanCumulativeAvgRuns(frames, names)
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
This function computes and plots the relative cumulative average runs of  batsmen
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
# Retrieve the file path of a data file installed with cricketr
tendulkar <- system.file("data", "tendulkar.csv", package = "cricketr")
ganguly <- system.file("data", "ganguly.csv", package = "cricketr")

batsmen <- list(tendulkar,ganguly)
names <- list("Tendulkar","Ganguly")
relativeBatsmanCumulativeAvgRuns(batsmen,names)
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
\code{\link{relativeBatsmanCumulativeStrikeRate}}
\code{\link{relativeBowlerCumulativeAvgEconRate}}
\code{\link{relativeBowlerCumulativeAvgWickets}}
}

