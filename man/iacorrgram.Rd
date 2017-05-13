\name{iacorrgram}
\alias{iacorrgram}

\title{
Interactive corrgrams
}
\description{
Generates an interactive corrgram programmed with the package \pkg{shiny}. It uses the package \pkg{seriation} for reordering, optimal leaf ordering ('OLO') and reordering based on principal components ('PCA'). If 'OLO' is selected, cluster lines can be drawn by selecting the number of clusters or a minimum correlation (see \code{varclust} for details). A range of correlation can be chosen, where only pairs of variables with absolute correlation within the range are drawn with colored cells, otherwise the cells are colored white.
}
\usage{
iacorrgram(data)
}

\arguments{
  \item{data}{
A data frame.
}
}
\value{
A shiny app object. 
}
\references{
M. Friendly (2002) Corrgrams: Exploratory displays for correlation matrices
\emph{The American Statistician} \bold{56}(4), 316--324.

W. Chang, J. Cheng, J. Allaire, Y. Xie and J. McPherson (2016) shiny: Web Application Framework for R. \url{https://cran.r-project.org/package=shiny}.

}
\author{
Katrin Grimm 
}



\seealso{
\code{varclust}
}
\examples{
\dontrun{
data(Election2005)
iacorrgram(Election2005)
}
}
\keyword{interactive apps}
