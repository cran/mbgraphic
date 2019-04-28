\name{dcor2d}
\alias{dcor2d}
\title{ Distance correlation for pairs of variables
}
\description{
Calculates the bivariate distance correlation for a given pair of variables, a numeric matrix or a data frame.
}
\usage{
dcor2d(x, y = NULL, binning = FALSE, b = 50, anchor = "min",parallel=FALSE)
}

\arguments{
  \item{x}{ A numeric vector, a numeric matrix or a data frame. In case of a data frame only the numeric variables are used.
}
  \item{y}{ A numeric vector.
}
  \item{binning}{A logical value. Whether or not binning should be used. TRUE, "equi" for equidistant binng, "quant" for quantile based binning or "hexb" for hexagonal binning. Default is FALSE.
}
  \item{b}{A positive integer. Number of bins in each variable.
}
  \item{anchor}{A chraracter string or a numeric value. How should the anchor point be chosen? "\code{min}" (default) for the minimum of each variable, "\code{ggplot}" for the method used in ggplot graphics, "\code{nice}" for a "pretty"" anchorpoint, or a user specified value.
}
\item{parallel}{A logical value. Whether or not parallelization should be used. Default is FALSE.
}
}

\value{
A numeric value describing the value of the measure if a pair of vectors is given.
Otherwise a data frame with the following variables:
\item{splines2d}{Value of the measure.}
\item{x1}{Number of first variable}
\item{x2}{Number of second variable.}
\item{nx1}{Name of first variable (missing if \code{x} is not a data frame).}
\item{nx2}{Name of second variable (missing if \code{x} is not a data frame).}
}


\references{
G. J. Szekely, M. L. Rizzo und N. K. Bakirov (2007) Measuring and testing dependence by correlation of distances.\emph{The Annals of Statistics} \bold{35}(6) 2769--2794.

A. Pilhoefer und A. Unwin (2013) New Approaches in Visualization of Categorical
Data: R Package extracat \emph{Journal of Statistical Software} \bold{53}(1) 1--25.
} 
\author{
Katrin Grimm 
}



\seealso{
%\code{\link[extracat]{wdcor}} in package \pkg{extracat}, 
\code{\link{splines2d}}
}
\examples{
data(Election2005)
\dontrun{
# distance correlation for all pairs of variables
dcor <- dcor2d(Election2005)
# put the pairs in decreasing order
o_dcor <- dcor[order(dcor$dcor2d,decreasing=TRUE),]

# Show the 10 pairs with highest values
o_dcor[1:10,]

# Show the 4 scatterplots with highest values
par(mfrow=c(2,2))
for(i in 1:4){
plot(with(Election2005,get(as.character(o_dcor$nx1[i]))),
  with(Election2005,get(as.character(o_dcor$nx2[i]))), 
  xlab=paste(o_dcor$nx1[i]),ylab=paste(o_dcor$nx2[i]),pch=19)
}
}
}
\keyword{two-dimensional meausures and selections}