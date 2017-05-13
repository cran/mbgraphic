\name{datatype}
\alias{datatype}
\title{Description of the datatype of variables
}
\description{
Simple calculations to describe the datatype of variables and quantify the degree of discreteness.}
\usage{
datatype(x, limit = 0.8)
}
\arguments{
  \item{x}{A vector, a numeric matrix or a data frame.  
}
  \item{limit}{The limit used to define if a variable is treated as discrete or continous, depending on the value of \code{\link{discrete1d}}.
}
}

\value{
A data frame with the following variables:
\item{numeric}{A logical value. Is variable of mode "numeric"?}
\item{discrete}{Value of function  \code{\link{discrete1d}}.}
\item{valdisc}{A logical value. Is variable discrete under the given definition of discreteness?}
}


\author{
Katrin Grimm 
}

\seealso{
\link{discrete1d}
}

\examples{
data(Election2005)
datatype(Election2005[,1:10])
}
