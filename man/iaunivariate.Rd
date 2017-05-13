\name{iaunivariate}
\alias{iaunivariate}
\title{
Interactive app for exploring univariate anomalies
}
\description{
Generates an interactive app programmed with the  package \pkg{shiny}. It uses the functions \code{\link{discrete1d}}, \code{\link{skew1d}} and  \code{\link{multimod1d}} and displays them in histograms. %The user can interactively choose the number of bins for the histograms of the measures. 

Which variables from the data set are plotted can be decided interactively by selecting bins within the histograms of the measures. Character variables from the data set can be used for highlighting categories in the plots of the variables. 
}
\usage{
iaunivariate(data, exp = 1, seed = NULL)
}
\arguments{
  \item{data}{
A data frame. 
}
  \item{exp}{
A positive  integer. See \code{\link{multimod1d}}.
}
  \item{seed}{
An integer. See \code{\link{multimod1d}}.
}
}

\value{
A shiny app object. 
}
\references{
W. Chang, J. Cheng, J. Allaire, Y. Xie and J. McPherson (2016) shiny: Web Application Framework for R. \url{https://cran.r-project.org/package=shiny}.}
\author{
Katrin Grimm 
}

\seealso{
\code{\link{discrete1d}}, \code{\link{skew1d}} and  \code{\link{multimod1d}}
}
\examples{
\dontrun{
data(Electon2005)
iaunivariate(Election2005)
}
}
\keyword{interactive apps}
