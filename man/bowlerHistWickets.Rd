\name{bowlerHistWickets}
\alias{bowlerHistWickets}
\title{
Plot a histogram of Wicket percentages versus wickets taken
}
\description{
This function computes the percentages of wickets taken versus wickets in the bowler's career
}
\usage{
bowlerHistWickets(file,name="A Googly")
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
Tinniam V Ganesh <tvganesh.85@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ 
\code{\link{bowlerWktsFreqPercent}}
\code{\link{relativeBowlingER}}
\code{\link{relativeBowlingPerf}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  <- getPlayerData(30176,file="kumble.csv",type="bowling", homeOrAway=c(1,2),result=c(1,2,4))

bowlerHistWickets("../cricketr/data/kumble.csv", "Anil Kumble")

# Note: This example uses the file kumble.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
