\name{batsmanRunsPredict}
\alias{batsmanRunsPredict}
\title{
Predict the runs for the batsman given the Balls Faced and Minutes in crease
}
\description{
Fit a linear regression plane between Runs scored and Minutes in Crease and Balls Faced. This will be used to predict the batsman runs for time in crease and balls faced
}
\usage{
batsmanRunsPredict(file, name="A Coverdrive", newdataframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the <batsman>.csv file obtained with an initial getPlayerData()
}
\item{name}{
Name of the batsman
}
\item{newdataframe}{
This is a data frame with 2 columns BF(Balls Faced) and Mins(Minutes)
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
\code{\link{battingPerf3d}}
\code{\link{batsmanContributionWonLost}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  <- getPlayerData(35320,file="tendulkar.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# Use a single value for BF and Mins
BF <- 30
Mins <- 20

batsmanRunsPredict("<path>/tendulkar.csv","Sachin Tendulkar",newdataframe=data.frame(BF,Runs)

or give a data frame
BF <- seq(20,200, length=15)
Mins <- seq(30,220,length=15)

batsmanRunsPredict("<path>/tendulkar.csv","Sachin Tendulkar",newdataframe=data.frame(BF,Runs)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
