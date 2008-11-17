\name{Met.NN1}
\alias{Met.NN1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Neural Networks}
\description{
 Fit single-hidden-layer neural network, possibly with skip-layer connections. 
}
\usage{
Met.NN1(datos, externa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{datos}{ Spectra data frame}
  \item{externa}{ Not implemented yet}

}
\details{
Application of artificial neural networks (ANN) for data processing is characterized by analogy with a biological neuron. An ANN consists of a layered network of nodes, each of which performs a simple operation on several inputs to produce a single output. 
This application (Metabonomic Analysis / Neural Network / Neural Network (Single hidden layer)) makes use of the ''nnet'' function from the ''nnet'' R package. This graphical application allows the user to build a single-hidden-layer neural network, selecting the number of units in the hidden layer, the initial random weight and the weight decay.
In addition, the user can choose between random or manual selection of the training samples.
Launched with the GUI. Beta version.
}
\references{  nnet package  \url{http://finzi.psych.upenn.edu/R/library/nnet/html/nnet.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }

