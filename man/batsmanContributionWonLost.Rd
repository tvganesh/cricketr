\name{batsmanContributionWonLost}
\alias{batsmanContributionWonLost}
\title{
Disply the batsman's contribution in matches that were won and those that were lost
}
\description{
Plot the comparitive contribution of the batsman in matches that were won and lost as box plots
}
\usage{
batsmanContributionWonLost(profileNo, name = "A Hitter")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profileNo}{
%%     ~~Describe \code{profileNo} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
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
\code{\link{batsmanPerfBoxHist}}
}
\examples{

batsmanContributionWonLost(35320,"Sachin Tendulkar")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
