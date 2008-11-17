\name{Met.Norm}
\alias{Met.Norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Normalization }
\description{
  Types of variable normalization 
}
\usage{
Met.Norm(datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{  Spectra data frame}
}
\details{
A crucial step in data pre-processing of spectra from metabonomic studies is the so-called normalization. The normalization step tries to account for possible variations in sample concentrations. Normalization can also be necessary due to technical reasons. 
If spectra are recorded using a different number of scans or if spectra are recorded with different devices, the absolute values of the spectra are different and rendering a joint analysis of spectra without prior normalization is impossible. The normalization graphical application (Preprocesing / Normalization) allows choosing between several types of normalizations using functions from the clusterSim library.
Launched with the GUI. Beta version.
}
\references{ clusterSim package  \url{http://finzi.psych.upenn.edu/R/library/clusterSim/html/data.Normalization.html}}

\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
