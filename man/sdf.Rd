\name{sdf}
\alias{sdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates measures from package \pkg{scagnostics}.
}
\description{
The function calculates the scagnostics from package \pkg{scagnostics} and converts them to a list of class \code{"sdfdata"}.
}
\usage{
sdf(data, selection = "All")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame.  If it contains categorical variables, they got excluded. 
}
  \item{selection}{
Numeric or character. 
Do you want to use all nine measures or only a selection? Give the numbers of the measures or their names. Default is \code{"All"}. 
}
}

\value{
A list of class \code{"sdfdata"} including data frames \code{sdf} and \code{data}.
  \item{sdf}{The (up to) 9 scagnostics from the package \pkg{scagnostics} and a variable \code{status}, which describes if a plot is classified as "Outlier", "Exemplar" or not.}
\item{data}{The original data frame minus categorical variables.}

}
\references{
L. Wilkinson, A. Anand and R. Grossman (2005) Graph-Theoretic Scagnostics
\emph{Proceedings of the 2005 IEEE Symposium on Information Visualization} 157--164.

L. Wilkinson and A. Anand (2012) scagnostics: Compute scagnostics - scatterplot diagnostics.  \url{https://cran.r-project.org/package=scagnostics}.

}
\author{
Katrin Grimm
}
\note{
Uses '1-Convex' instead of 'Convex'.

The nine measures are: \code{"Outlying",  "Skewed",    "Clumpy",    "Sparse",   "Striated",  "1-Convex",  "Skinny",    "Stringy", "Monotonic"}
}


\seealso{
 \code{\link{scag2sdf}},

\code{\link[scagnostics]{scagnostics}} in package \pkg{scagnostics}
}
\examples{
data(Election2005)
set.seed(345456)
election05_small <- Election2005[,sample(5:70,10)]

scagdf <- sdf(election05_small)
head(scagdf$sdf)

# Use only "Outlying", "Clumpy" and "1-Convex"
scagdf_sel <- sdf(election05_small,selection=c("Outlying", "Clumpy","1-Convex"))
# the same as 
scagdf_sel <- sdf(election05_small,selection=c(1,3,6))
}
\keyword{two-dimensional meausures and selections}