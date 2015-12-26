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
getPlayerDataSp(profileNo, tdir = "./data", tfile = "player001.csv", ttype = "batting")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profileNo}{
This is the profile number of the player to get data. This can be obtained from http://www.espncricinfo.com/ci/content/player/index.html. Type the name of the player and click search. This will display the details of the player. Make a note of the profile ID. For e.g For Sachin Tendulkar this turns out to be
http://www.espncricinfo.com/india/content/player/35320.html. Hence the profile for Sachin is 35320
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
\code{\link{getPlayerData}}
}
\examples{
# Only away. Get data only for won and lost innings
tendulkar <-getPlayerData(35320,dir="../", file="tendulkarsp.csv",type="batting")

# Get bowling data and store in file for future
kumble <- getPlayerData(30176,dir="../",file="kumblesp.csv",type="bowling")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
