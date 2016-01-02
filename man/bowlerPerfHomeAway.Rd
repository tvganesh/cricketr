\name{bowlerPerfHomeAway}
\alias{bowlerPerfHomeAway}
\title{
This function analyses the performance of the bowler at home and overseas
}
\description{
This function plots the Wickets taken by the batsman at home and overseas
}
\usage{
bowlerPerfHomeAway(file, name = "A Googly")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
CSV file of the bowler from ESPN Cricinfo (for e.g. Kumble's profile no:30176)
}
  \item{name}{
Name of bowler
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
\code{\link{bowlerPerfForecast}}
\code{\link{checkBowlerInForm}}
\code{\link{bowlerContributionWonLost}}
}
\examples{
# Get or use the <bowler>.csv obtained with getPlayerDataSp()
#kumbleSp <-getPlayerDataSp(30176,".","kumblesp.csv","bowling")

# Retrieve the file path of a data file installed with cricketr
path <- system.file("data", "kumblesp.csv", package = "cricketr")
bowlerPerfHomeAway(path,"Anil Kumble")

# Note: This example uses the file kumble.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
