% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prob.R
\name{Prob}
\alias{Prob}
\title{Calculating Rasch Probability}
\usage{
Prob(raschObj, theta)
}
\value{
A list with the input elements and the answer
 \item{final}{A dataframe comprised of columns P and PQ}
}
\description{
Calculating the Probability That the Respondent Answered Correctly or Incorrectly
}
\section{Slots}{

\describe{
\item{\code{raschObj}}{An object of class Rasch}

\item{\code{theta}}{A numerical input for theta}
}}

\examples{

myRasch <- newRasch("Emily", c(3, 4, 12), c(1, 1, 0))
Prob(myRasch, theta = 4)
}
\seealso{
newRasch
}
\author{
Emily Garner<\email{emily.garner@wustl.edu}>
}
