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
More details can be found in my short video tutorial in Youtube
https://www.youtube.com/watch?v=q9uMPFVsXsI
}
\value{
Returns a data frame with the predicted runs for the Balls Faced and Minutes at crease
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
# tendulkar <- getPlayerData(35320,file="tendulkar.csv",type="batting", 
# homeOrAway=c(1,2), result=c(1,2,4))

# Use a single value for BF and Mins
BF <- 30
Mins <- 20

# retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
batsmanRunsPredict(pathToFile,"Sachin Tendulkar",newdataframe=data.frame(BF,Mins))

#or give a data frame
#BF <- seq(20,200, length=15)
#Mins <- seq(30,220,length=15)

#values <- batsmanRunsPredict("../cricketr/data/tendulkar.csv","Sachin Tendulkar",
    #newdataframe=data.frame(BF,Runs)
#print(values)

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
