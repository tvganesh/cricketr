\name{getPlayerDataSp}
\alias{getPlayerDataSp}
\title{
Get the player data along with venue and 
}
\description{
This function is a specialized version of getPlayer Data. This function gets the players data 
along with details on matches' venue (home/abroad) and the result of match(won,lost,drawn) as 
2 separate columns (ha & result). The column ha has 1:home and 2: overseas. The column result
has values 1:won , 2;lost and :drawn match
}
\usage{
getPlayerDataSp(profileNo, tdir = "./data", tfile = "player001.csv", 
ttype = "batting")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profileNo}{
This is the profile number of the player to get data. This can be obtained from https://www.espncricinfo.com/ci/content/player/index.html. Type the name of the player and click search. This will display the details of the player. Make a note of the profile ID. For e.g For Sachin Tendulkar this turns out to be
https://www.espncricinfo.com/india/content/player/35320.html. Hence the profile for Sachin is 35320
}
  \item{tdir}{
Name of the directory to store the player data  into. If not specified the data is stored in a default directory "./data". Default="./tdata"
}
  \item{tfile}{
Name of the file to store the data into for e.g. tendulkar.csv. This can be used for subsequent functions. Default="player001.csv"
}
  \item{ttype}{
type of data required. This can be "batting" or "bowling"
}
}
\details{
More details can be found in my short video tutorial in Youtube
https://www.youtube.com/watch?v=q9uMPFVsXsI
}
\value{
Returns the player's dataframe along with the homeAway and the result columns
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
\code{\link{getPlayerData}}
}
\examples{
\dontrun{
# Only away. Get data only for won and lost innings
tendulkar <-getPlayerDataSp(35320,tdir="../cricketr/data", tfile="tendulkarsp.csv",ttype="batting")

# Get bowling data and store in file for future
kumble <- getPlayerDataSp(30176,tdir="../cricketr/data",tfile="kumblesp.csv",ttype="bowling")
}
}
