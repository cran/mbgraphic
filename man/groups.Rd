\name{groups}
\alias{groups}

\title{
Quantifying the influence of a factor variable
}
\description{
The function calculates a linear model by calling \code{lm(xi~classvar)} for all numeric variables (\code{xi}) from \code{x} and returns the coefficients of determination. The aim is to find numeric variables for which high ratios of the variability can be explained by a factor variable \code{classvar}. 
}
\usage{
groups(data, classvar)
}

\arguments{
  \item{data}{
A data frame. 
}
  \item{classvar}{
A factor. The variable which is used to explain the numeric variables from \code{x}.
}
}

\value{
If \code{x} is a numeric vector, the coefficient of determination of the model described by \code{lm(x~classvar)} is returned. Otherwise a data frame with the following two variables:
\item{groups}{coefficient of determination.}
\item{variable}{actual variable.}
}

\author{
Katrin Grimm
}


\seealso{
\code{\link{groups_maxplot}}, \code{\link{lm}}
}
\examples{
data(Election2005)

# Define new variable which explains affiliation of the constituencies
# to east Germany, west Germany and Berlin 
OstWest <-  ifelse(Election2005$Land \%in\% 
    c("Thueringen","Sachsen","Sachsen-Anhalt",
    "Brandenburg","Mecklenburg-Vorpommern"),  "Ost", "West")
Election2005$OstWestBerlin <- 
  as.factor(ifelse(Election2005$Land == "Berlin","Berlin",OstWest))

# Calculate measure groups 
groupm <- groups(Election2005, "OstWestBerlin")
# Show highest values 
groupm[with(groupm,order(groups,decreasing=TRUE))[1:10],]
}
\keyword{two-dimensional meausures and selections}