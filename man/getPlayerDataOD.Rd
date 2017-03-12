\name{getPlayerDataOD}
\alias{getPlayerDataOD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the One day player data from ESPN Cricinfo based on specific inputs and store in a file in a given directory
}
\description{
Get the player data given the profile of the batsman. The allowed inputs are home,away or both and won,lost or draw of matches. The data is stored in a <player>.csv file in a directory specified. This function also returns a data frame of the player
}
\usage{
getPlayerDataOD(profile, opposition="",host="",dir = "../", file = "player001.csv", 
type = "batting", homeOrAway = c(1, 2, 3), result = c(1, 2, 3,5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{profile}{
This is the profile number of the player to get data. This can be obtained from http://www.espncricinfo.com/ci/content/player/index.html. Type the name of the player and click search. This will display the details of the player. Make a note of the profile ID. For e.g For Virender Sehwag this turns out to be
http://www.espncricinfo.com/india/content/player/35263.html. Hence the profile for Sehwag is 35263
}
\item{opposition}{
The numerical value of the opposition country e.g.Australia,India, England etc. The values are 
Australia:2,Bangladesh:25,Bermuda:12, England:1,Hong Kong:19,India:6,Ireland:29,
Netherlands:15,New Zealand:5,Pakistan:7,Scotland:30,South Africa:3,Sri Lanka:8,United Arab Emirates:27,
West Indies:4, Zimbabwe:9;  Africa XI:405
Note: If no value is entered for opposition then all teams are considered
}
\item{host}{
The numerical value of the host country e.g.Australia,India, England etc. The values are 
Australia:2,Bangladesh:25,England:1,India:6,Ireland:29,Malaysia:16,New Zealand:5,Pakistan:7,
Scotland:30,South Africa:3,Sri Lanka:8,United Arab Emirates:27,West Indies:4, Zimbabwe:9
Note: If no value is entered for host then all host countries are considered
}
\item{dir}{
Name of the directory to store the player data  into. If not specified the data is stored in a default directory "../data". Default="../data"
}
  \item{file}{
Name of the file to store the data into for e.g. tendulkar.csv. This can be used for subsequent functions. Default="player001.csv"
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
\code{\link{getPlayerDataSp}}
\code{\link{getPlayerData}}
}
\examples{

\dontrun{
# Both home and away. Result = won,lost and drawn
sehwag <-getPlayerDataOD(35263,dir="../cricketr/data", file="sehwag1.csv",
type="batting", homeOrAway=c(1,2),result=c(1,2,3,5))

# Only away. Get data only for won and lost innings
sehwag <-getPlayerDataOD(35263,dir="../cricketr/data", file="sehwag2.csv",
type="batting",homeOrAway=c(2),result=c(1,2))

# Get bowling data and store in file for future
malinga <- getPlayerData(49758,dir="../cricketr/data",file="malinga1.csv",
type="bowling")

# Get Dhoni's ODI record in Australia against Australua
dhoni <- getPlayerDataOD(28081,opposition = 2,host=2,dir=".",
file="dhoniVsAusinAusOD",type="batting")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
