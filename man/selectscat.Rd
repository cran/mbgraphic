\name{selectscat}  
\alias{selectscat}
\title{
Selecting a scatterplot matrix based on scagnostics 
}
\description{
Selects a scatterplot matrix from a data frame including the \code{k} variables with approximately highest "relevance". If no own measure of relevance is defined, the function uses the maximum of the measures \code{"Outlying", "Clumpy",   "Sparse",   "Striated", "1-Convex"} and  \code{"Stringy"} from the \pkg{scagnostics} package. See Details and Note.
}
\usage{
selectscat(data,relmat=NULL,k=5,r=k,plot=TRUE,criteria="maxm")
}
\arguments{
  \item{data}{
A data frame or a list of class "sdfdata". If \code{data} is a data frame and contains categorical variables, they are excluded. 
}
\item{relmat}{
\code{NULL} or a matrix which can be  interpreted as a similarity matrix \cr
(\code{m_ii = 1, m_ij = m_ji, 0 <= m_ij <= 1}).
}
  \item{k}{
A positive integer. 
The number of variables to include in the scatterplot matrix.
}
  \item{r}{
A positive integer (greater or equal to \code{k}). Controls the goodness of the approximation (see Details).
}
  \item{plot}{
Logical. Should the plot be drawn? Default is \code{TRUE}.
}
  \item{criteria}{
\code{"maxm"} or \code{"cor"}. Use the maximum of the measures (\code{"maxm"}) or the correlation as the measure of relevance. Ignored if  \code{relmat} is not equal to \code{NULL}. 
}

}
\details{
To make this selection work fast in case of data sets with a huge number of variables, considering all possible combinations needs to be avoided. The implemented algorithm reorders the variables on optimal leafs. That means an average linkage clustering is done based on the criterion of relevance which is interpreted as a similarity measure. The new order of the variables is chosen so that pairs of variables with high values in the criteria are grouped. That allows us to search around the diagonal of the reordered matrix including all variables for the optimal matrix of size \code{k}. The size of the area around the diagonal in which the optimal matrix is searched is controlled by \code{r}. If \code{r} = \code{p} (number of numeric variables of the data set) than every possible combination is considered. Otherwise it is not certain that the optimal matrix is found.
}
\value{
A ggpair object (if \code{plot=TRUE})  or a character vector including the variable names selected by the function (if \code{plot=FALSE}).
}

\note{
When using more than one measure, results can be strongly influenced by differences in the scales of the measures. Make sure that all measures have similar scales. 

When using the function defaults, results can strongly depend on the measure "\code{1-Convex}".
}

\author{
Katrin Grimm 
}

\references{
B. Schloerke et al. (2016) GGally: Extension to ggplot2. \url{https://cran.r-project.org/package=GGally}
}


\seealso{
\code{\link{sdf}}, \code{\link{scag2sdf}}
}
\examples{
data(Election2005)

\dontrun{
# Use whole data set with default settings
selectscat(Election2005)
# 7 variables and a higher chance of finding optimal matrix 
selectscat(Election2005,k=7,r=15)
}

# Use correlation as the measure of relevance 
selectscat(Election2005,criteria="cor")
# boring for the election data 
# same result as
election_num <- Election2005[,sapply(Election2005,is.numeric)] 
selectscat(election_num,relmat=cor(election_num),plot=FALSE)

\dontrun{
# If a list of class "sdfdata" is already calculated
sdfdf <- sdf(Election2005)
# Use only measure "Outlying"
sdfdf_O <- sdfdf
sdfdf_O$sdf <- sdfdf_O$sdf[,c(1,10,11)]
selectscat(sdfdf_O,k=7,r=15) 
}
}
\keyword{two-dimensional meausures and selections}