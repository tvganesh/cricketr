\name{clean}
\alias{clean}
\title{
Create a batsman data frame given the batsman's CSV file
}
\description{
The function removes rows from the batsman dataframe where the batsman did not bat (DNB) or the team did not bat (TDNB). COnverts not outs '*' (97*, 128*) to 97,128 by stripping the '*' character. It picks all the complete cases and returns the data frame
}
\usage{
clean(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
CSV file with the batsman data obtained with getPlayerData
}
}
\details{
More details can be found in my short video tutorial in Youtube
https://www.youtube.com/watch?v=q9uMPFVsXsI
}
\value{
Returns the cleaned batsman dataframe
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
\code{\link{cleanBowlerData}}
\code{\link{getPlayerData}}
\code{\link{batsman4s}}
\code{\link{batsmanMovingAverage}}
}
\examples{
\dontrun{
# Get or use the <batsman>.csv obtained with getPlayerData()
#  <- getPlayerData(35320,file="tendulkar.csv",type="batting", homeOrAway=c(1,2),result=c(1,2,4))

# clean the dataframe
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
clean(pathToFile)

# Note: This example uses the file kumble.csv from the /data directory. However
# you can use any directory as long as the data file exists in that directory.
}
}

