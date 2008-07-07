\name{Met.KNN}
\alias{Met.KNN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ k-Nearest Neighbour Classification}
\description{
  k-nearest neighbour classification for test set from training set. For each row of the test set, the k nearest (in Euclidean distance) training set vectors are found, and the classification is decided by majority vote, with ties broken at random. If there are ties for the kth nearest vector, all candidates are included in the vote. 

}
\usage{
Met.KNN(datos, externa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Spectra data frame}
  \item{externa}{ Not implemented yet}
}
\details{
 Launched with the GUI. Beta version.

}

\references{ class package  \url{http://finzi.psych.upenn.edu/R/library/class/html/knn.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }