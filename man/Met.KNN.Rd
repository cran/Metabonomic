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
The k-Nearest Neighbors (KNN) rule for classification is the simplest of all supervised classification approaches. For classification of an unknown object, its distance, usually the Euclidian distance, to all other objects is computed. The minimum distance is selected and the object is assigned to the corresponding class. The KNN graphical interface (Metabonomic Analysis / KNN) allows to choose between random or manual selection of the samples to build the model, number of the neighbors, minimum vote for definite decision or the use or not of all the neighbors. 
If the use of all the neighbors is selected, all distances equal to the kth largest are included. If not, a random selection of distances equal to the kth is chosen to use exactly k neighbors. To finish, it returns the results of the validation test and the cross validation test. The KNN graphical application makes use of the ''knn'' function from the class package  

Launched with the GUI. Beta version.

}

\references{ class package  \url{http://finzi.psych.upenn.edu/R/library/class/html/knn.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
