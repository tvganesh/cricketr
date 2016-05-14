\name{batsmanCumulativeAverageRuns}
\alias{batsmanCumulativeAverageRuns}
\title{Batsman's cumulative average runs}
\usage{
batsmanCumulativeAverageRuns(file,name= "A Leg Glance")
}
\arguments{
\item{file}{Data frame}

\item{name}{Name of batsman}
}
\value{
None
}
\description{
This function computes and plots the cumulative average runs of a batsman
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
# retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
batsmanCumulativeAverageRuns(pathToFile, "Sachin Tendulkar")
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
\code{\link{batsmanCumulativeStrikeRate}}
\code{\link{bowlerCumulativeAvgEconRate}}
\code{\link{bowlerCumulativeAvgWickets}}
}

