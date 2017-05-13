\name{sdf_sort}
\alias{sdf_sort}

\title{
Reordering a list of class "sdfdata"
}
\description{
The function reorders a list of class \code{"sdfdata"} for displaying scagnostics results in scaggrams. 
 The reordering algorithm is based on distances between "direct" neighbors. See Details. }
\usage{
sdf_sort(sdfdata, maxt = NULL, select = "All", printmsum = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sdfdata}{
A list of class \code{"sdfdata"}.
}
  \item{maxt}{
\code{NULL}  or a positive integer giving a time limit. Default is \code{NULL} (no time limit).
}
\item{select}{\code{"All"} or a vector of integers or strings giving the scagnostics which should be used for reordering.}
\item{printmsum}{logical. Should the optimality criterion be printed after each iteration? Default is \code{FALSE}.}

}
\details{
The implemented algorithm to reorder the \code{"sdfdata"} list is greedy. In each iteration all variable changes of exactly two variables are considered, that means associated rows and columns within the scaggram are switched. The (p choose 2) different orders are compared by \code{msum}. The goal is to minimize \code{msum}, which is defined as the sum of distances of all row-wise and all column-wise neighbors. The algorithm stops if no improvement is possible by changing two arbitrary variables or the specified time limit is exceeded. 

  
}
\value{
A list of class \code{"sdfdata"}.
}

\author{
Katrin Grimm 
}



\seealso{
\code{\link{sdf}}, \code{\link{scag2sdf}}, \code{\link{sdf_quicksort}}, \code{\link{scaggram}}, \code{\link{iascaggram}} 
}

\examples{
data(Election2005)
# consider only demographic/economic information
election05_ds <- Election2005[,1:40]
\dontrun{
scagdf <- sdf(election05_ds)
# ordering based on all scagnostics from sdf
scagdf_o2 <- sdf_sort(scagdf)
# compare scaggrams
par(mfrow=c(1,2))
scaggram(scagdf,select=(c(1,5,9)))
scaggram(scagdf_o2,select=(c(1,5,9)))
}
}
\keyword{two-dimensional meausures and selections}
