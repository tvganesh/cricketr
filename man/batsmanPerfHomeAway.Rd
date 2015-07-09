\name{batsmanPerfHomeAway}
\alias{batsmanPerfHomeAway}
\title{
This function analyses the performance of the batsman at home and overseas
}
\description{
This function plots the runs scored by the batsman at home and overseas
}
\usage{
batsmanPerfHomeAway(profileNo, name = "A Hitter")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profileNo}{
Profile number of batsman from ESPN Cricinfo (for e.g. Tendulkar's profile no:35320)
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{batsmanMovingAverage}}
\code{\link{batsmanRunsPredict}}
\code{\link{batsmanPerfBoxHist}}
\code{\link{bowlerContributionWonLost}}
}
\examples{
batsmanPerfHomeAway(35320,"Sachin Tendulkar")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
