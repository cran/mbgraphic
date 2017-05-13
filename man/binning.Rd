\name{binning}
\alias{binning}
\title{Binning of single variables or data frames 
}
\description{
Bins a single vector, a pair of vectors, a matrix or a data frame  by equidistant or quantile based binning. 
}
\usage{
binning(x, y = NULL, b = 20, bin = "equi", anchor = "min")
}
\arguments{
  \item{x}{A numeric vector, a numeric matrix or a data frame.  In case of a data frame only the numeric variables are used.
}
  \item{y}{\code{NULL} (default) or a vector with same length as x.
}
  \item{b}{A positive integer. Number of bins in each variable.
}
  \item{bin}{A character string giving the binning method. "\code{equi}" (default) for equdistant binning or "\code{quant}" for quantile based binning.
}
  \item{anchor}{A chraracter string or a numeric. How should the anchor point be chosen? "\code{min}" (default) for the minimum of each variable, "\code{ggplot}" for the method used in ggplot graphics, "\code{nice}" for a "pretty"" anchorpoint, or a user specified value.
}
}

\value{
A data frame giving the mean of each bin and the numbers of counts. 
}

\author{
Katrin Grimm 
}

\seealso{
 \code{\link{binningplot}}
}
\examples{
df <- data.frame(
  x = rnorm(1000),
  y = rnorm(1000),
  z = rnorm(1000))

# A common binning for all variables of df with 5 bins in each dimension:
binning(df,b=5)
}