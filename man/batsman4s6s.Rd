\name{batsman4s6s}
\alias{batsman4s6s}
\title{
Compute and plot a stacked barplot of runs,4s and 6s
}
\description{
Compute and plot a stacked barplot of percentages of runs in (1s,2s and 3s),4s and 6s
}
\usage{
batsman4s6s(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
List of batsman
}
  \item{names}{
Names of batsman
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
\code{\link{batsmanScoringRateODTT}}, 
\code{\link{relativeRunsFreqPerfODTT}},
\code{\link{batsmanPerfBoxHist}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerDataOD()
#sehwag <-getPlayerData(35263,dir="./data", file="sehwag.csv",type="batting", 
#homeOrAway=c(1,2),result=c(1,2,4))

# retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
batsman4s6s(pathToFile, "Sachin Tendulkar")

#Note: This example uses the file tendulkar.csv from the /data directory. However
#you can use any directory as long as the data file exists in that directory.
#The general format is pkg-function(pathToFile,par1,...)
}

