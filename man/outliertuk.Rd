\name{outliertuk}
\alias{outliertuk}

\title{
Outlier classification
}
\description{
Creates a matrix of outlier classification ("outl" or "non") for the data set based on Tukey's defnition. 
}
\usage{
outliertuk(x)
}

\arguments{
  \item{x}{A numeric vector, a numeric matrix or a data frame. In case of a data frame only the numeric variables are used.
}
}

\value{
A data frame with same dimension as \code{x} minus non-numeric variables.
}

\author{
Katrin Grimm 
}




\seealso{
\code{\link{outlier1dv}}
}
\examples{
data(Election2005)
outlc <- outliertuk(Election2005)

# sort and plot with function "visid" from package "extracat"
\dontrun{
library(extracat)
visid(outlc,sort="b")
}
}
\keyword{outliers}