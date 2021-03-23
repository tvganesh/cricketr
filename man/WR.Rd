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

\arguments{
  \item{file}{
Name of file
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

