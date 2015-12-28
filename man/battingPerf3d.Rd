\name{battingPerf3d}
\alias{battingPerf3d}

\title{
Make a 3D scatter plot of the Runs scored versus the Balls Faced and Minutes at Crease.
}
\description{
Make a 3D plot of the Runs scored by batsman vs Minutes in crease and Balls faced. Fit a linear regression 
plane
}
\usage{
battingPerf3d(file, name="A Hookshot")
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
\code{\link{batsmanMeanStrikeRate}}, 
\code{\link{batsmanMovingAverage}}, 
\code{\link{batsmanPerfBoxHist}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  tendulkar<- getPlayerData(35320,file="tendulkar.csv",type="batting", 
#homeOrAway=c(1,2),result=c(1,2,4))

battingPerf3d("../cricketr/data/tendulkar.csv","Sachin Tendulkar")

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
