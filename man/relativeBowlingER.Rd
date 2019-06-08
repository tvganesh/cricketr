\name{relativeBowlingER}
\alias{relativeBowlingER}
\title{
Compute and plot the relative mean Economy Rate(ER) of the bowlers
}
\description{
This function computes and plots the relative Economy Rate of the bowlers
}
\usage{
relativeBowlingER(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
This is a list of <bowler>.csv files obtained with an initial getPlayerData()
}
  \item{names}{
A list of Twenty20 bowlers names who need to be compared
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
\code{\link{bowlerWktsFreqPercent}}
\code{\link{relativeBowlingPerf}}
\code{\link{bowlerHistWickets}}
}
\examples{
\dontrun{
# Get the list of the <batsman>.csv files obtained with getPlayerData() for batsmen to be compared
# kumble  <- getPlayerData(30176,file="kumble.csv",type="bowling", 
# homeOrAway=c(1,2),result=c(1,2,4))

# Retrieve the file path of a data file installed with cricketr
kumble <- system.file("data", "kumble.csv", package = "cricketr")
warne <- system.file("data", "warne.csv", package = "cricketr")
murali <- system.file("data", "murali.csv", package = "cricketr")

frames <- list(kumble,warne,murali)
names <- c("Kumble","Warne","Murali")
relativeBowlingER(frames,names)

# Note: This example uses the /data directory for the files. However
# you can use any directory as long as the data files exists in that directory.
}
}
