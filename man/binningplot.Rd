\name{binningplot}
\alias{binningplot}
\title{Two-dimensional binningplot
}
\description{Visualisation of a two-dimensional binning based on equidistant or quantile based binning. 
}
\usage{
binningplot(x, y, b = 10, bin = "equi", anchor = "min")
}
\arguments{
  \item{x}{ A numeric vector.
}
  \item{y}{ A numeric vector.
}
  \item{b}{A positive integer. Number of bins in each variable.
}
  \item{bin}{A chraracter string. Binning method
  "equi" (default) for equidistant binning or "quant" for quantile binning.
}
  \item{anchor}{A chraracter string or a numeric. How should the anchorpoint be chosen? "\code{min}" (default) for the minimum of each variable, "\code{ggplot}" for the method used in ggplot graphics, "\code{nice}" for a "pretty"" anchor point, or a user specified value.
}
}

\value{
A ggplot object. 
}

\references{
H. Wickham (2009) \emph{ggplot2: Elegant Graphics for Data Analysis} New York: Springer
}

\author{
Katrin Grimm
}


\seealso{
\code{\link{binning}}
}
\examples{
x <- rnorm(10000)
y <- rnorm(10000)

# equidistant binning with 20 bins in each variable
binningplot(x,y,b=20)

# quantile based binning with 20 bins in each variable
binningplot(x,y,b=20,bin="quant")
}
