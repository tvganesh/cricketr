\name{relativeWktRateTT}
\alias{relativeWktRateTT}
\title{
Compute and plot the relative Mean Wicket Rate of the bowlers in Twenty20 International
}
\description{
This function computes and plots the relative Wicket Rate of the bowlers in Twenty20 International
}
\usage{
relativeWktRateTT(frames, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frames}{
This is a list of Twenty20 <bowler>.csv files obtained with an initial getPlayerDataTT()
}
  \item{names}{
A list of bowlers names who need to be compared
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
\code{\link{bowlerWktRateTT}}
}
\examples{
\dontrun{
# Get or use the <bowler>.csv obtained with getPlayerData()
# a <- getPlayerDataTT(26421,dir=".",file="ashwin.csv",type="bowling", 
# homeOrAway=c(1,2,3), result=c(1,2,3,5))

# Retrieve the file path of a data file installed with cricketr
mendis <- system.file("data", "mendis.csv", package = "cricketr")
narine  <- system.file("data", "narine.csv", package = "cricketr")
badree <- system.file("data", "badree.csv", package = "cricketr")

frames <- list(mendis, badree,narine)
names <- c("Mendis","Badree","Narine")
relativeWktRateTT(frames,names)

#Note: This example uses the /data directory for the files. However
#you can use any directory as long as the data files exists in that directory.
}
}
