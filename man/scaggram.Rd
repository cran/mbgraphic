\name{scaggram}
\alias{scaggram}

\title{
Visualization of scagnostics results
in  matrix graphics
}

\description{
Scaggram are thought as a generalization of corrgrams. It is possible to use up to three arbitrary scagnostics at the same time. Each of the measures is represented by one of the colors red, green, or blue. The resulting color of a cell (representing a pair of variables) in the scaggram is given by the mixture of the (up to) three colors according to the values of the scagnostics. 
}

\usage{
scaggram(sdfdata,select=1,add=FALSE,label=FALSE,order= FALSE)
}

\arguments{
\item{sdfdata}{
A list of class \code{"sdfdata"}.
}
  \item{select}{
Numeric or character vector of length \code{1, 2} or \code{3}. 
Give the column numbers of the measures or their names. Default is \code{1}.
}
  \item{add}{
Logical.
Which additional features should be added to the plot? \code{FALSE} for none, \code{"splom"} for scatterplots and \code{"glyphs"} for star glyphs of all scagnostics stored in \code{sdfdata$sdf}.
}
  \item{label}{
Should the names of the variables be plotted? Default is  \code{FALSE}.
}
\item{order}{
Should the variables be reordered? Default is  \code{FALSE}. Use \code{2} or \code{"quicksort"} for quick reorering with function \code{\link{sdf_quicksort}}, \code{3} or \code{"All scagnostics"} for reordering with function \code{\link{sdf_sort}} based on all scagnosticsand \code{4} or \code{"Selected scagnostics"} for reordering with  \code{\link{sdf_sort}} based on the selected scagnostics.
}
}

\details{
Given three scagnostics with values \code{s1, s2, s3} describing a pair of variables, then the color of the respective cell of the scaggram is determined by \code{rgb(s1,s2,s3, alpha = max(s1,s2,s3))}. 

If glyphs or a splom are added by setting \code{add}, the part above the diagonal is used for the additional feature and the color of the cells is drawn with a transparency of \code{max(s1,s2,s3,0.2))}. 
}

\note{
The scagnostics need to range between 0 and 1. 

For a good and informative color result, it is required that the scagnostics used have similar scales. Otherewise the result can be strongly influenced by single measures. 
}

\author{
Katrin Grimm 
}

\seealso{
\code{\link{iascaggram}}, \code{\link{sdf}}, \code{\link{scag2sdf}}, \code{\link{sdf_sort}}, 
\code{\link{sdf_quicksort}}

\code{\link{rgb}}
}

\examples{
data(Election2005)
# Results of the election
\dontrun{
sdfres <- sdf(Election2005[,41:70])
# Use scagnostics "Outlying", "Clumpy" and "Monotonic"
scaggram(sdfres,select=c(1,3,9))
}
}
\keyword{two-dimensional meausures and selections}
