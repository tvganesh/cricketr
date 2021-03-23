\name{relativeBatsmanSRODTT}
\alias{relativeBatsmanSRODTT}
\title{
Calculate and plot the relative Mean Strike Rate (SR) for each batsman for ODI or Twenty20 batsmen
}
\description{
Calculate and plot the relative MEan Strike Rate (SR) for each batsman for ODI or Twenty20 batsmen
}
\usage{
relativeBatsmanSRODTT(frames, names)
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
\code{\link{batsmanScoringRateODTT}} 
\code{\link{relativeRunsFreqPerfODTT}}
\code{\link{batsmanPerfBoxHist}}
}
\examples{
\dontrun{
# Get or use the <batsman>.csv obtained with getPlayerDataOD() or getPlayerTT()
#sehwag <-getPlayerData(35263,dir="./mytest", file="sehwag.csv",
#type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# Retrieve the file path of a data file installed with cricketr
sehwag <- system.file("data", "sehwag.csv", package = "cricketr")
devilliers <- system.file("data", "devilliers.csv", package = "cricketr")
gayle <- system.file("data", "gayle.csv", package = "cricketr")
maxwell <- system.file("data", "maxwell.csv", package = "cricketr")

batsmen <- list(sehwag,devilliers,gayle,maxwell)
names <- list("Sehwag","Devilliers","Gayle","Maxwell")
relativeBatsmanSRODTT(batsmen,names)

# Note: This example uses the /data directory for the files. However
# you can use any directory as long as the data files exists in that directory.
}
}
