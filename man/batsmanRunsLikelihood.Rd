\name{batsmanRunsLikelihood}
\alias{batsmanRunsLikelihood}
\title{
This function uses K-Means to determine the likelihood of the batsman to get runs
}
\description{
This function used K-Means to get the likelihood of getting runs based on clusters of runs the batsman
made in the past.It uses K-Means for this.
}
\usage{
batsmanRunsLikelihood(file, name = "A Squarecut")
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{batsmanMovingAverage}}
\code{\link{batsmanRunsPredict}}
\code{\link{battingPerf3d}}
\code{\link{batsmanContributionWonLost}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  <- getPlayerData(35320,file="tendulkar.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

batsmanRunsLikelihood("<path>/tendulkar.csv","Sachin Tendulkar")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
