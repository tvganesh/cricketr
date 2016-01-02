\name{relativeBowlingPerf}
\alias{relativeBowlingPerf}
\title{
Plot the relative performances of bowlers
}
\description{
This function calculates and plots the relative performance of the suers
}
\usage{
relativeBowlingPerf(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
This is a list of <bowler>.csv files obtained with an initial getPlayerData()
}
\item{names}{
A list of bowlers names who need to be compared
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
\code{\link{bowlerHistWickets}}
}
\examples{
# Get the list of the <batsman>.csv files obtained with getPlayerData() for batsmen to be compared
# kumble  <- getPlayerData(30176,file="kumble.csv",type="bowling", 
# homeOrAway=c(1,2),result=c(1,2,4))

# Retrieve the file path of a data file installed with cricketr
kumble <- system.file("data", "kumble.csv", package = "cricketr")
warne <- system.file("data", "warne.csv", package = "cricketr")
murali <- system.file("data", "murali.csv", package = "cricketr")

frames <- list(kumble,warne,murali)
names <- c("Kumble","Warne","Murali")
relativeBowlingPerf(frames,names)

# Note: This example uses the /data directory for the files. However
# you can use any directory as long as the data files exists in that directory.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
