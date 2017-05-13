\name{Election2013}
\alias{Election2013}
\docType{data}
\title{Election2013 data
%%   ~~ data name/kind ... ~~
}
\description{
A data set for the German 'Bundestag' election of 2013. It includes information about the elections
in 2013 and in 2009 separately for each of the 299 constituencies and also demographic and other information about the constituencies themselves.  
}  
\usage{data(Election2013)}
\format{
    A data frame with 299 observations on 114 variables.
  The variables of the data set are:
\itemize{
 \item 1-3: general information about the constituencies (Bundesland, ID and name of district)
 \item 4-42: demographic and economic information
 \item 43 - 50: general information about the elections in 2013 and 2009
 \itemize{
 \item \code{Wahlberechtigte}: number of eligible voters
 \item \code{Waehler}: votes cast
 \item \code{UNG_ZS}: invalid second votes
 \item \code{GUEL_ZS}: valid second votes
 }
 \item 51- 70: results of the seven biggest parties in 2013 and 2009 (smaller parties are summarized in the variable \code{Sonstige_ZS}). Everything with a '_VP' at the end is from the election in 2009. '_q' stands for proportion of second votes. 
 }

}

\seealso{
 \link[mbgraphic]{Election2013} \cr
 
In comparison to the \bold{Election2005} data, there are more smaller parties reported. That produces more variables and more missing values (not all of the parties stand for election in all of the constituencies). 
}


\source{
Original source was \url{http://www.bundeswahlleiter.de}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
dim(Election2013)
datatype(Election2013)
str(Election2013)
}
\keyword{datasets}
