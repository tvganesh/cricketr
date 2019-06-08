\name{batsmanScoringRateODTT}
\alias{batsmanScoringRateODTT}
\title{
Compute and plot the predicted scoring rate for a One day batsman or Twenty20
}
\description{
This function computes and plots a 2nd order polynomial between the balls faced and runs scored for ODI or Twenty20
}
\usage{
batsmanScoringRateODTT(file, name = "A Hookshot")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the <batsman>.csv file obtained with an initial getPlayerDataOD() or getPlayerTT()
}
  \item{name}{
Name of the batsman
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
\code{\link{batsman6s}}
\code{\link{relativeBatsmanSRODTT}}
\code{\link{relativeRunsFreqPerfODTT}}
}
\examples{
\dontrun{
# Get or use the <batsman>.csv obtained with getPlayerDataOD() or or getPlayerTT()
#sehwag <-getPlayerData(35263,dir="./mytest", file="sehwag.csv",type="batting",
# homeOrAway=c(1,2),result=c(1,2,4))

# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "sehwag.csv", package = "cricketr")
batsmanScoringRateODTT(pathToFile,"Sehwag")

# Note: This example uses the file sehwag.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}
}
