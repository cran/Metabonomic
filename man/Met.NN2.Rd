\name{Met.NN2}
\alias{Met.NN2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Neural Network 2}
\description{
  Creates a feedforward artificial neural network according to the structure established by the AMORE package standard. 

}
\usage{
Met.NN2(datos, externa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{datos}{ Spectra data frame}
  \item{externa}{ Not implemented yet}

}
\details{
This application  creates a feedforward artificial neural network according to the structure established by the ''AMORE'' package. With this application the user can select the number of layers and the number of neurons in each layer. Moreover, a lot of parameters can be controlled by the user like the learning rate at which every neuron is trained, the momentum for every neuron, the error criterion (Least Mean Square or Least Mean Logarithm Squares), the activation function of the hidden and the output layer (Purelin, Tansig, Sigmoide or Hardlim) and the training method (Adaptative gradient descend or BATCH gradient descend, with or without momentum). With these parameters selected, the algorithm trains the network with the samples manually or randomly selected, and it tests the networks with the other samples.
Launched with the GUI. Beta version.
}
\references{  AMORE package  \url{http://finzi.psych.upenn.edu/R/library/AMORE/html/newff.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
