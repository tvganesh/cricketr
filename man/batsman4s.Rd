\name{batsman4s}
\alias{batsman4s}
\title{
Plot the numbers of 4s against the runs scored by batsman
}
\description{
This function plots the number of 4s against the total runs scored by batsman. A 2nd order polynomial regression curve is also plotted. The predicted number of 4s for 50 runs and 100 runs scored is also plotted
}
\usage{
batsman4s(file, name="A Hookshot")
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
\code{\link{batsman6s}}
}
\examples{
# Get or use the <batsman>.csv obtained with getPlayerData()
#tendulkar  <- getPlayerData(35320,dir="../",file="tendulkar.csv",type="batting",
#homeOrAway=c(1,2),result=c(1,2,4))

# retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
batsman4s(pathToFile, "Sachin Tendulkar")

# Note: The above example uses the file tendulkar.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory. The
# general format is pkg-function(pathToFile,par1,...)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
