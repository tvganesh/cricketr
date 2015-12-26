\name{relativeWktRateTT}
\alias{relativeWktRateTT}
\title{
Compute and plot the relative Mean Wicket Rate of the bowlers in Twenty20 International
}
\description{
This function computes and plots the relative Wicket Rate of the bowlers in Twenty20 International
}
\usage{
relativeWktRateTT(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
This is a list of Twenty20 <bowler>.csv files obtained with an initial getPlayerDataTT()
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
\code{\link{relativeBowlingPerf}}
\code{\link{bowlerHistWickets}}
\code{\link{bowlerWktRateTT}}
}
\examples{
# Get or use the <bowler>.csv obtained with getPlayerData()
# a <- getPlayerDataTT(26421,dir=".",file="ashwin.csv",type="bowling", homeOrAway=c(1,2,3),                # result=c(1,2,3,5))

frames <- list("../cricketr/data/mendis.csv","../cricketr/data/badree.csv","../cricketr/data/narine.csv")
names <- c("Mendis","Badree","Narine")
relativeWktRateTT(frames,names)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
