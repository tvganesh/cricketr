\name{batsmanRunsFreqPerf}
\alias{batsmanRunsFreqPerf}
\title{
Calculate and run frequencies in ranges of 10 runs and plot versus Runs the performance of the batsman
}
\description{
This function calculates frequencies of runs in 10 run buckets and plots this percentage
}
\usage{
batsmanRunsFreqPerf(file, name="A Hookshot")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the <batsman>.csv file obtained with an initial getPlayerData()
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
\code{\link{batsmanDismissals}}, 
\code{\link{batsmanMovingAverage}}, 
\code{\link{batsmanPerfBoxHist}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  <- getPlayerData(35320,file="tendulkar.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

batsmanRunsFreqPerf("../cricketr/data/tendulkar.csv","Sachin Tendulkar")

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
