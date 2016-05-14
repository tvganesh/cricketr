\name{bowlerCumulativeAvgEconRate}
\alias{bowlerCumulativeAvgEconRate}
\title{Bowler's cumulative average economy rate}
\usage{
bowlerCumulativeAvgEconRate(file,name)
}
\arguments{
\item{file}{Data frame}

\item{name}{Name of batsman}
}
\value{
None
}
\description{
This function computes and plots the cumulative average economy rate  of a bowler
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{)
pathToFile <- system.file("data", "kumble.csv", package = "cricketr")
bowlerCumulativeAvgEconRate(pathToFile,"Anil Kumble")
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
\code{\link{bowlerCumulativeAvgWickets}}
\code{\link{batsmanCumulativeStrikeRate}}
}

