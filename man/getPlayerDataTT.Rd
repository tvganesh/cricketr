\name{getPlayerDataTT}
\alias{getPlayerDataTT}
\title{
Get the Twenty20 International player data from ESPN Cricinfo based on specific inputs and store in a file in a given directory~
}
\description{
Get the Twenty20 player data given the profile of the batsman/bowler. The allowed inputs are home,away, neutralboth and won,lost,tied or no result of matches. The data is stored in a <player>.csv file in a directory specified. This function also returns a data frame of the player
}
\usage{
getPlayerDataTT(profile, dir = "./data", file = "player001.csv", 
type = "batting", homeOrAway = c(1, 2, 3), result = c(1, 2, 3,5))
}
\arguments{
  \item{profile}{
This is the profile number of the player to get data. This can be obtained from http://www.espncricinfo.com/ci/content/player/index.html. Type the name of the player and click search. This will display the details of the player. Make a note of the profile ID. For e.g For Virat Kohli this turns out to be 253802
http://www.espncricinfo.com/india/content/player/35263.html. Hence the profile for Sehwag is 35263
}
  \item{dir}{
Name of the directory to store the player data  into. If not specified the data is stored in a default directory "./data". Default="./data"
}
  \item{file}{
Name of the file to store the data into for e.g. kohli.csv. This can be used for subsequent functions. Default="player001.csv"
}
  \item{type}{
type of data required. This can be "batting" or "bowling"
}
  \item{homeOrAway}{
This is vector with either or all  1,2, 3. 1 is for home 2 is for away, 3 is for neutral venue
}
  \item{result}{
This is a vector that can take values 1,2,3,5. 1 - won match 2- lost match 3-tied 5- no result 
}
}
\details{
More details can be found in my short video tutorial in Youtube
https://www.youtube.com/watch?v=q9uMPFVsXsI
}
\value{
Returns the player's dataframe
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
\code{\link{bowlerWktRateTT}}
\code{\link{getPlayerData}}
}
\examples{
# Only away. Get data only for won and lost innings
kohli <-getPlayerDataTT(253802,dir="../cricketr/data", file="kohli1.csv",
type="batting")

# Get bowling data and store in file for future
ashwin <- getPlayerDataTT(26421,dir="../cricketr/data",file="ashwin1.csv",
type="bowling")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
