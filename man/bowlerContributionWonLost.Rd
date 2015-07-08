\name{bowlerContributionWonLost}
\alias{bowlerContributionWonLost}
\title{
Disply the bowler's contribution in matches that were won and those that were lost
}
\description{
Plot the comparitive contribution of the bowler in matches that were won and lost as box plots
}
\usage{
bowlerContributionWonLost(profileNo, name = "A Doosra")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profileNo}{
Profile number of the bowler from ESPN Cricinfo (for e.g. Kumble's profile no:30176)
}
  \item{name}{
Name of the bowler
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
}
\examples{
bowlerContributionWonLost(30176,"Anil Kumble")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
