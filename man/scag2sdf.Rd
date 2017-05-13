\name{scag2sdf}
\alias{scag2sdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating (user defined) scagnostics 
}
\description{
The function calculates scagnostics for a data frame based on the functions giving by the user and converts them to a list of class \code{"sdfdata"}.
}
\usage{
scag2sdf(data, scagfun.list, arguments.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame. If it contains categorical variables, they are excluded. 
}
  \item{scagfun.list}{
A list of functions (see Details). 
}
  \item{arguments.list}{
A list of arguments (see Details).
}
}
\details{
\code{scagfun.list} is a list of functions. Each of the functions needs to be a function with arguments \code{x} (a numeric vector) and \code{y} (a numeric vector). The function can also include further parameters, which can be specified in \code{arguments.list}. 


}
\value{
A list of class \code{"sdfdata"} including data frames \code{sdf} and \code{data}.
  \item{sdf}{The scagnostics for every single pair of variables for the functions given in \code{scagfun.list}.}
\item{data}{The original data frame minus categorical variables.} 
}

\author{
Katrin Grimm 
}



\seealso{
\code{\link{scag2sdf}}
}
\examples{
data(Election2005)
set.seed(345456)
election05_small <- Election2005[,sample(5:70,10)]

# Use correlation, distance correlation and a spline based 
# measure as scagnostics.

scagdf <- scag2sdf(election05_small,
    scagfun.list=
      list(cor=cor,dcor2d=dcor2d,splines2d=splines2d))
head(scagdf$sdf)

# Use sperman correlation 
scagdf <- scag2sdf(election05_small,
    scagfun.list=
      list(cor=cor,dcor2d=dcor2d, splines2d=splines2d),
    arguments.list = 
      list(list(method="spearman"),NULL,NULL))
}
\keyword{two-dimensional meausures and selections}