\name{varclust}
\alias{varclust}
\title{
Clustering of numeric variables
}
\description{
The function clusters variables using the average linkage algorithm. 
}
\usage{
varclust(data, c = NULL, mincor = NULL)
}
\arguments{
  \item{data}{
A data frame.
}
  \item{c}{
A positive integer. The number of clusters.
}
  \item{mincor}{
Numeric. The minimal correlation within each of the single clusters.
}
}
\details{
The number of clusters can be set by specifying \code{c} or choosing \code{mincor} (but not both at the same time). If \code{mincor} is used, the number of clusters is defined by a minimum correlation. That means every single pair of variables in the individual clusters is correlated with a value which is at least \code{mincor}.
}
\value{
A list with the following object:
 \item{c}{The number of clusters.}
\item{mincor}{The minimal correlation within each of the single clusters.}
\item{clusters}{The cluster allocation for each variable.}
\item{clusrep}{A character vector indicating the variables which are used as representatives for the clusters.}
\item{dfclusrep}{A data frame which only holds the cluster representatives as variables.}
}

\author{
Katrin Grimm
}


\examples{
data(Election2005)

# Variable clustering based on minimum correlation
vc1 <- varclust(Election2005,mincor=0.8)
vc1$c # The number of clusters is 45

# Variable clustering based on the number of clusters
vc2 <- varclust(Election2005,30)
vc2$mincor # The minimum correlation is 0.63
}
\keyword{two-dimensional meausures and selections}