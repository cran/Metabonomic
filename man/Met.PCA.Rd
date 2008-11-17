\name{Met.PCA}
\alias{Met.PCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Principal Components Analysis}
\description{
 Principal Components Analysis }
\usage{
Met.PCA(datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Spectra data frame}
}
\details{
Principal components analysis (PCA) is one of the most common exploratory steps for multivariate data analysis. The most important use of PCA is indeed to represent a multivariate data in a low-dimensional space. 
The first principal component represents the direction of largest variation in the swarm of points. The second principal component corresponds with the second largest variation, and so on.
The ''Metabonomic'' GUI incorporates a PCA graphical application (Metabonomic Analysis / PCA) to guide the users in the performance of PCA, allowing the selection of the algorithm parameters. In addition, interactive graphics have been developed to change the component showed, graphical parameters, etc.
The Principal Components algorithm used is based on the ''prcomp'' function from the stats library.
Launched with the GUI. Beta version.
}
\references{  stats package  \url{http://finzi.psych.upenn.edu/R/library/stats/html/prcomp.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
