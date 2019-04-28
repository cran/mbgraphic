\name{skew1d}
\alias{skew1d}
\title{ Measure for one-dimensional skewness
}
\description{
A measure for one-dimensional skewness based on quantiles.
}
\usage{
skew1d(x)
}

\arguments{
  \item{x}{
 A numeric vector, a numeric matrix or a data frame.
}
}

\value{
A single value or a vector including the results of \code{skew1d} for each variable (in case of numeric matrices or data frames). If variables from data frames are categorical, 'NA' is returned.
}

\author{
Katrin Grimm
}

\seealso{
\code{\link{multimod1d}}, \code{\link{discrete1d}}, \code{\link{iaunivariate}} 
}
\examples{
data(Election2005)
\dontrun{
skew <- skew1d(Election2005)
maxv <- order(skew,decreasing=TRUE)[1:4]
par(mfrow=c(2,2))
for(i in 1:4){
  hist(Election2005[,maxv[i]], xlab="",breaks=30,
  main=paste(names(Election2005[maxv[i]])))
}


# Explore skewness, multimodality and discreteness within interactive environment.
iaunivariate(Election2005)
}
}
\keyword{one-dimensional meausures}