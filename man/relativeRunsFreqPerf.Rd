\name{relativeRunsFreqPerf}
\alias{relativeRunsFreqPerf}
\title{
Calculate and compute the relative run frequencies of a list of  cricketers
}
\description{
This function computes the run frequencies in ranges of 10 and plots these for a list of
batsmen
}
\usage{
relativeRunsFreqPerf(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
This is a list of <batsman>.csv files obtained with an initial getPlayerData()
}
  \item{names}{
A list of batsmen names who need to be compared
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
Tinniam V Ganesh <tvganesh.85@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{batsmanDismissals}}, 
\code{\link{batsmanMovingAverage}}, 
\code{\link{batsmanPerfBoxHist}}
}
\examples{
# Get the list of the <batsman>.csv files obtained with getPlayerData() for batsmen to be compared
#  <- getPlayerData(35320,file="tendulkar.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

batsmen <- list("../cricketr/data/tendulkar.csv","../cricketr/data/dravid.csv", "../cricketr/data/ganguly.csv")
names <- list("Tendulkar","Dravid","Ganguly")
relativeRunsFreqPerf(batsmen,names)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
