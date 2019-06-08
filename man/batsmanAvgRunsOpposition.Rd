\name{batsmanAvgRunsOpposition}
\alias{batsmanAvgRunsOpposition}
\title{
This function computes and plots the Average runs against different opposition played by batsman
}
\description{
This function computes the mean runs scored by batsman against different opposition
}
\usage{
batsmanAvgRunsOpposition(file, name = "A Latecut")
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
\code{\link{batsmanDismissals}},  
\code{\link{batsmanMovingAverage}}, 
\code{\link{batsmanPerfBoxHist}}
\code{\link{batsmanAvgRunsGround}}
}
\examples{
\dontrun{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  <- getPlayerData(35320,file="tendulkar.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# retrieve the file path of a data file installed with cricketr
path <- system.file("data", "tendulkar.csv", package = "cricketr")
batsmanAvgRunsOpposition(path,"Sachin Tendulkar")

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
}
}

