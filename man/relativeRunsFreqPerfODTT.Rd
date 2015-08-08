\name{relativeRunsFreqPerfODTT}
\alias{relativeRunsFreqPerfODTT}
\title{
Calculate and compute the relative run frequencies of a list of  cricketers
}
\description{
This function computes the run frequencies in ranges of 10 and plots these for a list of
batsmen
}
\usage{
relativeRunsFreqPerfODTT(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
This is a list of <batsman>.csv files obtained with an initial getPlayerDataOD() or getPlayerTT()
}
  \item{names}{
A list of batsmen names who need to be compared
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
\seealso{
\code{\link{batsmanScoringRateODTT}}, 
\code{\link{relativeRunsFreqPerfODTT}},
\code{\link{batsmanPerfBoxHist}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerDataOD() or getPlayerTT()
#sehwag <-getPlayerData(35263,dir="./mytest", file="sehwag.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

batsmen <- list("./sehwag.csv","./devilliers.csv", "./gayle.csv","./maxwell.csv")
names <- list("Sehwag","Devilliers","Gayle","Maxwell")
relativeRunsFreqPerfODTT(batsmen,names)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
