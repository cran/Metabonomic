\name{Met.Outliers}
\alias{Met.Outliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Outliers Identification }
\description{
  Outliers identification by Mehalanobis distance.
}
\usage{
Met.Outliers(datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Spectra data frame}
  
}
\details{
It shows Mahalanobis distances based on robust and classical estimates of the location and the covariance matrix in different plots.This function makes use of the 'robustbase' package and It runs in the R console.
}
\value{
  \item{datos }{Spectra data frame}
 }
\references{  Robustbase package  \url{http://finzi.psych.upenn.edu/R/library/robustbase/html/00Index.html}}
\author{ Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
