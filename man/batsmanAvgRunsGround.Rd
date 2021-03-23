\name{batsmanAvgRunsGround}
\alias{batsmanAvgRunsGround}
\title{
This function computes and plots the Average runs scored in the different grounds played by batsman
}
\description{
This function computed the Average Runs scored on different pitches and also indicates the number of innings played at these venues
}
\usage{
batsmanAvgRunsGround(file, name = "A Latecut")
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
More details can be found in my short video tutorial in Youtube
https://www.youtube.com/watch?v=q9uMPFVsXsI
}
\value{
None
}
\references{
https://www.espncricinfo.com/ci/content/stats/index.html\cr
https://gigadom.in/
}
\author{
Tinniam V Ganesh
}
\note{
Maintainer: Tinniam V Ganesh <tvganesh.85@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{batsmanDismissals}},  
\code{\link{batsmanMovingAverage}}, 
\code{\link{batsmanPerfBoxHist}}
}
\examples{
\dontrun{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  <- getPlayerData(35320,file="tendulkar.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
batsmanAvgRunsGround(pathToFile,"Sachin Tendulkar")

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
#The general format is pkg-function(pathToFile,par1,...)
}
}
