\name{discrete1d}
\alias{discrete1d}
\title{Measure for the degree of discreteness
}
\description{
A simple measure for the degree of discreteness of a variable, of the columns of a numeric matrix or of the variables of a data frame based on the number of unique values. 
}
\usage{
discrete1d(x)
}

\arguments{
  \item{x }{ 
  A numeric vector, a numeric matrix or a data frame.
}
}

\value{
A single value or a vector including the results of \code{discrete1d} for each variable (in case of numeric matrices or data frames). If variables from data frames are categorical, 'NA' is returned.
}

\author{
Katrin Grimm
}


\seealso{
\code{\link{multimod1d}}, \code{\link{skew1d}}, \code{\link{iaunivariate}} 
}
\examples{
data(Election2005)

\dontrun{
discrete <- discrete1d(Election2005)
maxv <- order(discrete,decreasing=TRUE)[1:4]
par(mfrow=c(2,2))
for(i in 1:4){
  hist(Election2005[,maxv[i]], ,xlab="",breaks=30, 
  main=paste(names(Election2005[maxv[i]])))
}

# Explore skewness, multimodality and discreteness 
# within an interactive environment.
iaunivariate(Election2005)
}
}
\keyword{one-dimensional meausures}