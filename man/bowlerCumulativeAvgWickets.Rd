\name{bowlerCumulativeAvgWickets}
\alias{bowlerCumulativeAvgWickets}
\title{Bowler's cumulative average wickets}
\usage{
bowlerCumulativeAvgWickets(file,name)
}
\arguments{
\item{file}{Data frame}

\item{name}{Name of batsman}
}
\value{
None
}
\description{
This function computes and plots the cumulative average wickets of a bowler
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{)
pathToFile <- system.file("data", "kumble.csv", package = "cricketr")
bowlerCumulativeAvgWickets(pathToFile,"Anil Kumble")
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
\code{\link{batsmanCumulativeAverageRuns}}
\code{\link{bowlerCumulativeAvgEconRate}}
\code{\link{batsmanCumulativeStrikeRate}}
}

