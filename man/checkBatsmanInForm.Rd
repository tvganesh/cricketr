\name{checkBatsmanInForm}
\alias{checkBatsmanInForm}
\title{
Check whether the batsman is In-Form or Out-Of-Form by looking at his last 10 percent scores
}
\description{
This function checks whether the batsman is In-Form or Out-Of-Form by doing hypothesis testing and
generating a p-value. The last 10 percent of runs scored (sample) are used as a sample against the 
rest 90 percent of runs scored (population) by the batsman.A significance value of 0.05 is used. The lower tail is checked. The NULL hypothesis is that the batsman is In-Form with the sample mean being within 
95 percent confidence interval. If the sample mean is outside this 95 percent range and the p-value is less than the significance value the batsman is considered to be  Out-Of-Form
}
\usage{
checkBatsmanInForm(file, name = "A Hitter", alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the <batsman>.csv file obtained with an initial getPlayerData()
}
  \item{name}{
Name of the batsman
}
  \item{alpha}{
Significance value
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
\code{\link{batsmanMeanStrikeRate}}, 
\code{\link{batsmanMovingAverage}}, 
\code{\link{batsmanPerfBoxHist}}
}
\examples{
\dontrun{

# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
checkBatsmanInForm(pathToFile,"Sachin Tendulkar")

#Note: This example uses the file tendulkar.csv from the /data directory. However
#you can use any directory as long as the data file exists in that directory.
# The general format is pkg-function(pathToFile,par1,...)
}
}

