\name{batsmanCumulativeStrikeRate}
\alias{batsmanCumulativeStrikeRate}
\title{Batsman's cumulative average strike rate}
\usage{
batsmanCumulativeStrikeRate(file,name= "A Leg Glance")
}
\arguments{
\item{file}{Data frame}

\item{name}{Name of batsman}
}
\value{
None
}
\description{
This function computes and plots the cumulative average strike rate  of a batsman
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
batsmanCumulativeStrikeRate(pathToFile, "Sachin Tendulkar")
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
\code{\link{bowlerCumulativeAvgWickets}}
}


