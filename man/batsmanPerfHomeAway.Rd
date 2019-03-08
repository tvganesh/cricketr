\name{batsmanPerfHomeAway}
\alias{batsmanPerfHomeAway}
\title{
This function analyses the performance of the batsman at home and overseas
}
\description{
This function plots the runs scored by the batsman at home and overseas
}
\usage{
batsmanPerfHomeAway(file, name = "A Hitter")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
CSV file of batsman from ESPN Cricinfo obtained with getPlayerDataSp()
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
\seealso{
\code{\link{batsmanMovingAverage}}
\code{\link{batsmanRunsPredict}}
\code{\link{batsmanPerfBoxHist}}
\code{\link{bowlerContributionWonLost}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerData()
#tendulkarSp <-getPlayerDataSp(35320,".","tendulkarsp.csv","batting")

# retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkarsp.csv", package = "cricketr")
batsmanPerfHomeAway(pathToFile,"Sachin Tendulkar")

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}
