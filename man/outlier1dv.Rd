\name{outlier1dv}
\alias{outlier1dv}
\title{
Outlier measure based on tukey definition
}
\description{
Creates a matrix of outlier scores for the data set based on Tukey's defnition. 
}
\usage{
outlier1dv(x)
}

\arguments{
  \item{x}{
A numeric vector, a numeric matrix or a data frame. In case of a data frame only the numeric variables are used.
}
}


\value{
A data frame with same dimension as \code{x} minus non-numeric variables.
}

\author{
Katrin Grimm
}

\seealso{
\code{outliertuk}
}
\examples{
data(Election2005)
\dontrun{
outlm <- outlier1dv(Election2005)

# order data frame based on absolute row and column sums
abssum <- function(x){
  x <- abs(x)
  return(sum(x))
}
outlm2 <- outlm[order(apply(outlm,1,abssum),decreasing=TRUE),]
# consider only the 50 rows with  hightest values
outlm3 <- outlm2[1:50,1:68]
outlm4 <- outlm3[,order(apply(outlm3,2,abssum),decreasing=TRUE)]

# plot in heatmap

library(gplots)
heatmap.2(as.matrix(outlm4), col=colorpanel(20, "red", "white","blue"),
          breaks=seq(-1,1,2/20),dendrogram="none",
          key=FALSE,trace="none",Rowv=FALSE,Colv=FALSE)
          }
}
\keyword{outliers}