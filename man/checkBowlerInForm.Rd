\name{checkBowlerInForm}
\alias{checkBowlerInForm}
\title{
Check whether the bowler is In-Form or Out-Of-Form by looking at his last 10 percent scores
}
\description{
This function checks whether the bowler is In-Form or Out-Of-Form by doing hypothesis testing and
generating a p-value. The last 10 percent of runs scored (sample) are used as a sample against the 
rest 90 percent of runs scored (population) by the bowler.A significance value of 0.05 is used. The lower tail is checked. The NULL hypothesis is that the bowler is In-Form with the sample mean being within 
95 percent confidence interval. If the sample mean is outside this 95 percent range and the p-value is less than the significance value the bowler is considered to be  Out-Of-Form
}
\usage{
checkBowlerInForm(file, name = "A N Inswinger", alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the <bowler>.csv file obtained with an initial getPlayerData()
}
  \item{name}{
Name of the bowler
}
  \item{alpha}{
Significance value
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
\code{\link{bowlerMovingAverage}}
\code{\link{batsmanPerfForecast}}
\code{\link{bowlerContributionWonLost}}
}
\examples{
# Get or use the <bowler>.csv obtained with getPlayerData()
# a <- getPlayerData(30176,file="kumble.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "kumble.csv", package = "cricketr")
checkBowlerInForm(pathToFile,"Anil Kumble")

#Note: This example uses the file kumble.csv from the /data directory. However
#you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
