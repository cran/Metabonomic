\name{Met.PLS2}
\alias{Met.PLS2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Partial Least Squares Regresion}
\description{
  Partial Least Squares Regresion
}
\usage{
Met.PLS2(datos, externa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Spectra data frame}
  \item{externa}{ Not implemented yet}

}
\details{
The application (Metabonomic Analysis / Partial Least Squares / PLS with graphics) is performed using the ''plsr'' function from the ''pls'' package .  This PLS-DA is more complex and the user is guided by the application to execute all the steps in the proper order. Firstly, the user chooses between manual or random selection of the samples. 
Secondly, the user selects the PLS algorithm and the validation method. Four PLSR algorithms are available: the kernel algorithm, the wide kernel algorithm, the SIMPLS algorithm and the classical orthogonal scores algorithm. 
Next, the application creates a PLS model with the maximum number of components and it shows the explained variance and the R2 graphics of this model. With this information, the user can select the optimum number of PLS components to build the model. In addition, the Standard Error of Prediction (SEP) and the Root Mean Standard of Prediction (RMSEP) are plotted in the R console. 
Finally, the PLS graphical application returns the results of the validation test and different interactive graphics of the PLS model.
Launched with the GUI. Beta version.
}
\references{  pls package  \url{http://finzi.psych.upenn.edu/R/library/pls/html/mvr.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
