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
\code{\link{bowlerMovingAverage}}
\code{\link{bowlerPerfForecast}}
\code{\link{checkBowlerInForm}}
\code{\link{bowlerContributionWonLost}}
}
\examples{
# Get or use the <bowler>.csv obtained with getPlayerDataSp()
kumbleSp <-getPlayerDataSp(30176,".","kumblesp.csv","bowling")
bowlerPerfHomeAway("kumblesp.csv","Anil Kumble")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
