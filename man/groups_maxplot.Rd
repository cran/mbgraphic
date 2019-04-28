\name{groups_maxplot}
\alias{groups_maxplot}

\title{
Plots the variables with highest values in groups
}
\description{
Plots the variables with highest values in groups within histograms.  
}
\usage{
groups_maxplot(data, classvar, m = 5, samebinsize = FALSE)
}

\arguments{
  \item{data}{
A data frame. 
}
\item{classvar}{
A factor. The variable which is used to explain the numeric variables from \code{x}.
}
  \item{m}{
A positive integer. Number of plots to draw. Maximum is \code{10}.
}
  \item{samebinsize}{
Logical.  Whether or not bins are drawn with the same size. Default is FALSE. 
}
}

\value{
A ggplot object.
}

\author{
Katrin Grimm
}
\note{
You can only use factors with up to 7 different values. 
}

\references{
H. Wickham (2009) \emph{ggplot2: Elegant Graphics for Data Analysis} New York: Springer
}


\seealso{
\code{\link{groups}}
}
\examples{
data(Election2005)
\dontrun{
# Define new variable which explains affiliation of the constituencies
# to east Germany, west Germany and Berlin 
OstWest <-  ifelse(Election2005$Land \%in\% 
    c("Thueringen","Sachsen","Sachsen-Anhalt",
    "Brandenburg","Mecklenburg-Vorpommern"),  "Ost", "West")
Election2005$OstWestBerlin <- 
  as.factor(ifelse(Election2005$Land == "Berlin","Berlin",OstWest))

# Plot 5 variables with highest values in groups 
groups_maxplot(Election2005,"OstWestBerlin")
}
}
\keyword{two-dimensional meausures and selections}