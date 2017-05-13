\name{iascaggram}
\alias{iascaggram}

\title{
Interactive scaggrams
}
\description{
An interactive version of the scaggrams from function \code{\link{scaggram}} programmed with the package \pkg{shiny}. Selections within the scaggram can be made by clicking or brushing. A selection implies drawing the corresponding scatterplot(s).
}
\usage{
iascaggram(sdfdata)
}

\arguments{
  \item{sdfdata}{
A list of class \code{"sdfdata"}.
}
}
\details{
Reordering is done with the functions \code{\link{sdf_sort}} (options 'Use all scagnostics' and 'Use only chosen scagnostics') or \code{\link{sdf_quicksort}} (option 'Quick'). The glyphs drawn by choosing option 'add -> glyphs' represent all given scagnostics. 
}
\value{
A shiny app object. 
}
\references{
W. Chang, J. Cheng, J. Allaire, Y. Xie and J. McPherson (2016) shiny: Web Application Framework for R. \url{https://cran.r-project.org/package=shiny}.
}
\author{
Katrin Grimm 
}
\note{
The scagnostics need to lie between 0 and 1. 

\code{\link{sdf_sort}} can be very slow for data sets with a huge number of variables. Use Option 'Quick' in this case for a fast reordering or use \code{\link{sdf_sort}} (with a time limit if necessary) before starting the interactive app. 

Adding scatterplots or glyphs is only advisable for a small number of variables. 


}

\seealso{
\code{\link{scaggram}}, \code{\link{sdf}}, \code{\link{scag2sdf}}, \code{\link{sdf_sort}}, 
\code{\link{sdf_quicksort}}
}
\examples{
\dontrun{
data(Election2005)
# some demographic/economic variables 
sdfdata1 <- sdf(Election2005[,5:40])
iascaggram(sdfdata1) 

# another smaller subset (for example to explore the glyphs in the scaggram)
sdfdata2 <- sdf(Election2005[,5:19])
iascaggram(sdfdata2) 
}
}
\keyword{interactive apps}
