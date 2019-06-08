\name{bowlerWktRateTT}
\alias{bowlerWktRateTT}

\title{
Compute and plot the Mean number of deliveries versus wickets taken
}
\description{
This function computes and plots the Mean number of deliveries versus wickets taken for bowlers
in Twenty20 Internation
}
\usage{
bowlerWktRateTT(file, name = "A Bowler")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
his is the <bowler>.csv file obtained with an initial getPlayerDataTT()
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
http://www.espncricinfo.com/ci/content/stats/index.html\cr
https://gigadom.wordpress.com/
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
}
\examples{
\dontrun{
# Get or use the <bowler>.csv obtained with getPlayerData()
# a <- getPlayerDataTT(26421,dir=".",file="ashwin.csv",type="bowling", 
# homeOrAway=c(1,2,3), result=c(1,2,3,5))

# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "ashwin.csv", package = "cricketr")
bowlerWktRateTT(pathToFile,"R Ashwin")

# Note: This example uses the file kumble.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}
}
