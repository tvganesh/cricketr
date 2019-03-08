\name{bowlerPerfForecast}
\alias{bowlerPerfForecast}
\title{
Forecast the bowler performance based on past performances using Holt-Winters forecasting
}
\description{
This function forecasts the performance of the bowler based on past performances using HoltWinters
forecasting model
}
\usage{
bowlerPerfForecast(file, name = "A Googly")
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
\code{\link{bowlerEconRate}}, 
\code{\link{bowlerMovingAverage}}, 
\code{\link{bowlerContributionWonLost}}
}
\examples{
# Get or use the <bowler>.csv obtained with getPlayerData()
# a <- getPlayerData(30176,file="kumble.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "kumble.csv", package = "cricketr")
bowlerPerfForecast(pathToFile,"Anil Kumble")

# Note: This example uses the file kumble.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}
