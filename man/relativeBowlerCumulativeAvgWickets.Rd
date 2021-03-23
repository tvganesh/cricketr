\name{relativeBowlerCumulativeAvgWickets}
\alias{relativeBowlerCumulativeAvgWickets}
\title{Relative bowlers cumulative average wickets}
\usage{
relativeBowlerCumulativeAvgWickets(frames, names)
}
\arguments{
  \item{frames}{
This is a list of <bowler>.csv files obtained with an initial getPlayerData()
}
  \item{names}{
A list of Twenty20 bowlers names who need to be compared
}
}
\value{
None
}
\description{
This function computes and plots the relative cumulative average wickets of a bowler
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{)

# Retrieve the file path of a data file installed with cricketr
kumble <- system.file("data", "kumble.csv", package = "cricketr")
warne <- system.file("data", "warne.csv", package = "cricketr")
murali <- system.file("data", "murali.csv", package = "cricketr")

frames <- list(kumble,warne,murali)
names <- c("Kumble","Warne","Murali")
relativeBowlerCumulativeAvgWickets(frames,names)
}
}
\author{
Tinniam V Ganesh
}
\references{
https://www.espncricinfo.com/ci/content/stats/index.html\cr
https://gigadom.in/
}
\seealso{
\code{\link{relativeBatsmanCumulativeAvgRuns}}
\code{\link{relativeBowlerCumulativeAvgEconRate}}
\code{\link{relativeBatsmanCumulativeStrikeRate}}
}

