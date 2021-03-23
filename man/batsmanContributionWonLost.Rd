\name{batsmanContributionWonLost}
\alias{batsmanContributionWonLost}
\title{
Disply the batsman's contribution in matches that were won and those that were lost
}
\description{
Plot the comparative contribution of the batsman in matches that were won and lost as box plots
}
\usage{
batsmanContributionWonLost(file, name = "A Hitter")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
CSV file  of batsman from ESPN Cricinfo obtained with getPlayerDataSp()
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
\code{\link{batsmanMovingAverage}}
\code{\link{batsmanRunsPredict}}
\code{\link{batsmanPerfBoxHist}}
}
\examples{
\dontrun{
# Get or use the <batsman>.csv obtained with getPlayerData()
#tendulkarsp <-getPlayerDataSp(35320,".","tendulkarsp.csv","batting")
# retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkarsp.csv", package = "cricketr")
batsmanContributionWonLost(pathToFile,"Sachin Tendulkar")

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}
}

