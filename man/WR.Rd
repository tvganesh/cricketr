\name{WR}
\alias{WR}
\title{
This function caculates the wicket rate vs mean number of deliveries
}
\description{
This function caculates the wicket rate vs mean number of deliveries
}
\usage{
WR(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Name of file
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file) 
{
    bowler <- clean(file)
    wktRate <- NULL
    w <- NULL
    for (i in 0:max(as.numeric(as.character(bowler$Wkts)))) {
        balls <- bowler[bowler$Wkts == i, ]$Overs * 6
        if (length(balls != 0)) {
            wktRate[i] <- lapply(list(balls), mean)
            w[i] <- i
        }
    }
    a <- sapply(wktRate, is.null)
    wktRate[a] <- NaN
    wktRate
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
