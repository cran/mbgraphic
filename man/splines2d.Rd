\name{splines2d}
\alias{splines2d}
\title{ Spline-based dependency measure for pairs of variables

}
\description{
The function calculates a smoothing spline-based measure for quantifying functional dependencies between two variables. The function \code{gam} from package \pkg{mgcv} is used. 
}
\usage{
splines2d(x, y = NULL, binning = FALSE, b = 50, anchor = "min", parallel=FALSE)
}
\arguments{
  \item{x}{A numeric vector, a numeric matrix or a data frame. In case of a data frame only the numeric variables are used.
}
  \item{y}{ A numeric vector.
}
  \item{binning}{A logical value. Whether or not binning should be used. TRUE, "equi" for equidistant binng, "quant" for quantile based binning or "hexb" for hexagonal binning. Default is FALSE.
}
  \item{b}{A positive integer. Number of bins in each variable.
}
  \item{anchor}{A chraracter string or a numeric value. How should the anchorpoint be chosen? "\code{min}" (default) for the minimum of each variable, "\code{ggplot}" for the method used in ggplot graphics, "\code{nice}" for a "pretty" anchorpoint, or a user specified value.
}
\item{parallel}{A logical value. Whether or not parallelization should be used. Default is FALSE.
}
}
\details{
For each pair of variables \code{x} and \code{y} a model  where \code{x} depends on \code{y} and a model where \code{y} depends on \code{x} are calculated. The proportions of the explained variance is calculated for both models and the maximum is returned. "cr" basis is used for faster calculation. 

The number of start knots depends on the number of unique values in the independent variable. If the number is smaller than \code{20}, \code{3} start knots are used, \code{10} otherwise. 

The smoothing parameter is determined by cross validation.
}
\value{
A numeric value decribing the value of the measure if a pair of vectors is given.
Otherwise a data frame with the following variables:
\item{splines2d}{Value of the measure.}
\item{x1}{Number of first variable}
\item{x2}{Number of second variable.}
\item{nx1}{Name of first variable (missing if \code{x} is not a data frame).}
\item{nx2}{Name of second variable (missing if \code{x} is not a data frame).}
\item{tarvar}{The variable which was use as target variable (delivered higher value in the measure).}
}
\references{
S. N. Wood (2006) Generalized Additive Models: An Introduction with R. CRC Press,
London.

S. N. Wood (2016). mgcv: Mixed GAM Computation Vehicle with GCV/AIC/REML Smoothness Estimation.  \url{https://cran.r-project.org/package=mgcv}

}
\author{
Katrin Grimm
}


\seealso{ \code{\link[mgcv]{gam}} in \pkg{mgcv}, \code{\link{dcor2d}}
}
\examples{
data(Election2005)
\dontrun{
# spline-based measure for all pairs of variables
spl <- splines2d(Election2005)

# order the pairs decreasing
o_spl <- spl[with(spl,order(spl[,1],decreasing=TRUE)),]

# show the 10 pairs with highest values
o_spl[1:10,]

# Show the 4 scatterplots with highest values
par(mfrow=c(2,2))
for(i in 1:4){
plot(with(Election2005,get(as.character(o_spl$nx1[i]))),
  with(Election2005,get(as.character(o_spl$nx2[i]))), 
  xlab=paste(o_spl$nx1[i]),ylab=paste(o_spl$nx2[i]),pch=19)
}
}
}
\keyword{two-dimensional meausures and selections}
