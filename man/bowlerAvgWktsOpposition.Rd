\name{bowlerAvgWktsOpposition}
\alias{bowlerAvgWktsOpposition}
\title{
This function computes and plot the average wickets against different oppositon
}
\description{
This function computes the average wickets taken against different opposition by the bowler. It also shows the number innings against each opposition
}
\usage{
bowlerAvgWktsOpposition(file, name = "A Chinaman")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the <bowler>.csv file obtained with an initial getPlayerData()
}
  \item{name}{
Name of the bowler
}
}
\details{
More details can be found in my short video tutorial in Youtube
https://www.youtube.com/watch?v=q9uMPFVsXsI
}
\value{
None
}
\references{
https://www.espncricinfo.com/ci/content/stats/index.html\cr
https://gigadom.in/
}
\author{
Tinniam V Ganesh
}
\note{
Maintainer: Tinniam V Ganesh <tvganesh.85@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bowlerWktsFreqPercent}}
\code{\link{relativeBowlingER}}
\code{\link{relativeBowlingPerf}}
\code{\link{bowlerAvgWktsGround}}
}
\examples{
\dontrun{
# Get or use the <bowler>.csv obtained with getPlayerData()
# a <- getPlayerData(30176,file="kumble.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "kumble.csv", package = "cricketr")
bowlerAvgWktsOpposition(pathToFile,"Anil Kumble")

# Note: This example uses the file kumble.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory
# The general format is pkg-function(pathToFile,par1,...)
}
}
