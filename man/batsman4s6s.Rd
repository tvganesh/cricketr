\name{batsman4s6s}
\alias{batsman4s6s}
\title{
Compute and plot a stacked barplot of runs,4s and 6s
}
\description{
Compute and plot a stacked barplot of percentages of runs in (1s,2s and 3s),4s and 6s
}
\usage{
batsman4s6s(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
List of batsman
}
  \item{names}{
Names of batsman
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
\code{\link{batsmanScoringRateOD}}, 
\code{\link{relativeRunsFreqPerfOD}},
\code{\link{batsmanPerfBoxHist}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerDataOD()
#sehwag <-getPlayerData(35263,dir="./mytest", file="sehwag.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

batsmen <- list("./sehwag.csv","./devilliers.csv", "./gayle.csv","./maxwell.csv")
names <- list("Sehwag","Devilliers","Gayle","Maxwell")
batsman4s6s(batsmen,names)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
